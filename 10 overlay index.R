

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, climateStability, sf, tidyverse, gtools, rgeos, stringr, glue, fs)

g <- gc(reset = TRUE)
rm(list = ls())
options(warn = -1, scipen = 999)

# Load data ---------------------------------------------------------------
path <- './raster/indices'
dirs <- dir_ls(path, type = 'directory') %>% as.character()
fles <- map(dirs, dir_ls) %>% flatten %>% as.character() %>% grep('.tif$', ., value = TRUE)

# Read as raster files ----------------------------------------------------
rstr <- purrr::map(fles, rast)

# Suitability processing - resample
rstr[[5]] <- terra::project(rstr[[5]], terra::crs(rstr[[1]]))
rstr[[5]] <- terra::resample(rstr[[5]], rstr[[1]])

# All as stack file -------------------------------------------------------
stck <- do.call('c', rstr)

# Calc the average --------------------------------------------------------
avrg <- terra::mean(stck)

# Using weights -----------------------------------------------------------
wgth <- tibble(name = c('MESLI_FINA', 'suit', 'index', 'dens_popu', 'IUA_'), 
               wgth = c(0.25, 0.25, 0.2, 0.1, 0.2))

wgth.rstr <- terra::weighted.mean(x = stck, w = pull(wgth, 2))

# To write the results
dir_create('./raster/indices/total')

writeRaster(x = avrg, filename = './raster/indices/total/mean.tif')
writeRaster(x = wgth.rstr, filename = './raster/indices/total/wgth_1.tif')




