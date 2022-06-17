

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, tidyverse, fs, gtools, rgeos, stringr, RColorBrewer, usdm)

g <- gc(reset = TRUE)
rm(list = ls())
options(warn = -1, scipen = 999)


# Load data ---------------------------------------------------------------
srtm <- terra::rast('./raster/srtm/90m/srtm90m_proj_fill.tif')
proj <- terra::crs(srtm)
zone <- terra::vect('./shp/b_coello.shp')

# Read the precipitation layers
prec <- dir_ls('./raster/climate/baseline', regexp = 'prec') %>% 
  mixedsort() %>% 
  terra::rast() %>% 
  sum()

# Get a sample 
smpl <- terra::as.data.frame(prec, xy = TRUE) %>% 
  as_tibble() %>% 
  sample_n(tbl = ., size = 0.1 * nrow(.), replace = FALSE)

# Project -----------------------------------------------------------------
srtm <- terra::project(srtm, '+proj=longlat +datum=WGS84 +no_defs')
zone <- terra::project(zone, '+proj=longlat +datum=WGS84 +no_defs')

# Extract by mask  --------------------------------------------------------
srtm <- terra::crop(srtm, zone) %>% terra::mask(zone)
srtm_proj <- terra::project(srtm, proj)

# Terrain variables -------------------------------------------------------
trrn <- terra::terrain(srtm, v = c('slope', 'aspect', 'TPI', 'TRI'))
trrn <- c(srtm, trrn)

# Extract the values for the sample ---------------------------------------
vles <- terra::extract(trrn, smpl[,c('x', 'y')])
vles <- cbind(smpl, vles) %>% as_tibble()
vles <- dplyr::select(vles, -ID)
colnames(vles) <- c('lon', 'lat', 'prec', 'srtm', 'slope', 'aspect', 'TPI', 'TRI')

mtrx <- as.data.frame(vles[,3:8])
mtrx <- drop_na(mtrx)
round(cor(mtrx), 2)

# Result
srtm

