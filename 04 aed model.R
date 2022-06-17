

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, crayon, tidyverse, gtools, fs, glue, raster, dismo, usdm)

g <- gc(reset = TRUE)
rm(list = ls())
options(warn = -1, scipen = 999)

# Load data --------------------------------------------------------------
fles <- dir_ls('./raster/climate/baseline') %>% 
  grep('gwr', ., value = TRUE) %>% 
  as.character() %>% 
  mixedsort()

prec <- grep('prec', fles, value = T)
tmax <- grep('tmax', fles, value = T)
tmin <- grep('tmin', fles, value = T)

# Read as a raster
prec <- raster::stack(prec)

# Check negative values
for(i in 1:12){
  prec[[i]] %>% min(.[], na.rm = T) %>% print()
}

tmax <- raster::stack(tmax)
tmin <- raster::stack(tmin)

# To create the bioclimatic variables -------------------------------------
bioc <- dismo::biovars(prec = prec, tmax = tmax, tmin = tmin)
terra::writeRaster(x = bioc, filename = './raster/climate/baseline/gwr_bioc.tif')



