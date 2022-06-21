
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, rmapshaper, crayon, tidyverse, gtools, fs, glue, raster, dismo, usdm)

g <- gc(reset = TRUE)
rm(list = ls())
options(warn = -1, scipen = 999)

# Functions ---------------------------------------------------------------
make_avrg <- function(dir){
  
  cat(green(dir, '\n'))
  
  run <- dir_ls(dir) %>% dir_ls() %>% as.character()
  fls <- dir_map(run, identity) %>% as.character()
  
  rst <- grep('.tif$', fls, value = TRUE)
  rst <- raster::stack(rst)
  avr <- raster::mean(rst)
  sdt <- calc(x = rst, fun = sd)
  
  raster::writeRaster(avr, filename = glue('{dir}/run_1/predict_rslt_{basename(dir)}.tif'), overwrite = T)
  raster::writeRaster(sdt, filename = glue('{dir}/run_1/predict_sdtd_{basename(dir)}.tif'), overwrite = T)
  cat('Finish!\n')
  
}

# Load data ---------------------------------------------------------------
dirs <- dir_ls('./models/maxent') %>% as.character()

# To make the average -----------------------------------------------------
purrr::map(.x = dirs[2:length(dirs)], .f = make_avrg)


