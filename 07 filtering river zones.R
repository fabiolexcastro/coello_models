
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, rmapshaper, readxl, crayon, tidyverse, gtools, fs, glue, raster, dismo, usdm)

g <- gc(reset = TRUE)
rm(list = ls())
options(warn = -1, scipen = 999)

# Load data ---------------------------------------------------------------
dirs <- dir_ls('./models/maxent') %>% as.character()
shts <- excel_sheets('./tbl/Base de datos FINAL.xlsx')
type <- read_excel('./tbl/Base de datos FINAL.xlsx', sheet = 'Global')
wter <- terra::vect('./shp/rivers_bff_90m.gpkg') %>% st_as_sf %>% st_transform(st_crs(4326)) %>% terra::vect()

# Filtering agua ----------------------------------------------------------
spcs <- filter(type, Tipo %in% c('Pez', 'Macroinvertebrado')) %>% pull(1)

spcs
dirs <- dirs[c(4, 7, 6, 8, 9)]
fles <- map(dirs, dir_ls) %>% as.character() %>% dir_ls() %>% grep('.tif$', ., value = T) %>% as.character()
fles <- grep('rslt', fles, value = T)
rstr <- terra::rast(fles)

# To make the crop --------------------------------------------------------
rstr_crop <- terra::crop(rstr, wter)
rstr_crop <- terra::mask(rstr_crop, wter)

for(i in 1:nlyr(rstr_crop)){
  
  print(i)
  terra::writeRaster(x = rstr_crop[[i]], 
                     filename = glue('./results/acuaticos/predict_{spcs[i]}.tif'))
  
}
