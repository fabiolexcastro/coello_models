
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, crayon, geodata, RSAGA, tidyverse, gtools, fs, glue, raster, dismo, usdm)

g <- gc(reset = TRUE)
rm(list = ls())
options(warn = -1, scipen = 999)

downscaling <- function(inp, out){
  
  # inp <- fle.inp[1]
  # out <- fle.out[1]
  
  cat(green(basename(inp)), '\n')
  
  rsl <- rsaga.geoprocessor(
    lib = 'statistics_regression',
    module = 'GWR for Grid Downscaling',
    param = list(PREDICTORS = fle.srt,
                 REGRESSION = out,
                 DEPENDENT = inp),
    env = env)
  
  rst.inp <- terra::rast(inp)
  rst.out <- terra::rast(out)
  
  par(mfrow = c(1, 2))
  plot(rst.inp, main = '1 km')
  plot(rst.out, main = '90 m')
  par(mfrow = c(1, 1))
  
}

# Load data ---------------------------------------------------------------
zone <- terra::vect('./shp/b_coello.shp') %>% st_as_sf %>% st_transform(., st_crs(4326)) %>% vect()
cntr <- terra::centroids(zone) %>% crds() %>% as.vector()

# Download SRTM raster file
srt1 <- geodata::elevation_3s(lon = cntr[1], lat = cntr[2], path = './tmpr')
srt2 <- geodata::elevation_3s(lon = -74.9, lat = cntr[2], path = './tmpr')
srtm <- terra::mosaic(srt1, srt2)
srtm <- terra::crop(srtm, zone)
srtm <- terra::mask(srtm, zone)
plot(zone)
plot(srt1, add = TRUE)
plot(zone, add = TRUE)

dir_create('./raster/srtm/90m')
terra::writeRaster(srtm, './raster/srtm/90m/srtm_90m.tif')

# Climate files download
prec <- geodata::worldclim_country(country = 'COL', var = 'prec', path = './tmpr')
tmax <- geodata::worldclim_country(country = 'COL', var = 'tmax', path = './tmpr')
tmin <- geodata::worldclim_country(country = 'COL', var = 'tmin', path = './tmpr')

# Extract by mask
prec <- terra::crop(prec, zone) %>% terra::mask(zone)
tmax <- terra::crop(tmax, zone) %>% terra::mask(zone)
tmin <- terra::crop(tmin, zone) %>% terra::mask(zone)

# To write these files
purrr::map(.x = 1:12, .f = function(i){
  
  terra::writeRaster(x = prec[[i]], filename = glue('./raster/climate/baseline/prec_{i}.tif'))
  terra::writeRaster(x = tmax[[i]], filename = glue('./raster/climate/baseline/tmax_{i}.tif'))
  terra::writeRaster(x = tmin[[i]], filename = glue('./raster/climate/baseline/tmin_{i}.tif'))
  
})

fles <- dir_ls('./raster/climate/baseline', regexp = '.tif$') %>% 
  mixedsort() %>% 
  grep('tm', ., value = T) %>% 
  as.character()

# To make the downscaling -------------------------------------------------

# Environments
env <- rsaga.env(path = 'C:/saga-8.0.0_x64')

# Files 
fle.srt <- './raster/srtm/90m/srtm_90m.tif'
fle.inp <- fles
fle.out <- glue('{dirname(fle.inp)}/gwr_{basename(fle.inp)}')
purrr::map2(.x = fle.inp[2:24], .y = fle.out[2:24], .f = downscaling)

# Terrain analysis --------------------------------------------------------
proj <- '+proj=tmerc +lat_0=4.596200416666666 +lon_0=-74.07750791666666 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

srtm_proj <- terra::project(srtm, proj)
zone_proj <- terra::project(zone, proj)

# Calc area
area <- terra::expanse(zone_proj)
area <- area / 1e4

srtm_proj
terra::writeRaster(x = srtm_proj, filename = './raster/srtm/90m/srtm_90m_proj.tif')

