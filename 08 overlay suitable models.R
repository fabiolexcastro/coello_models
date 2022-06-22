
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, climateStability, sf, tidyverse, gtools, rgeos, stringr, glue, fs)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load libraries ----------------------------------------------------------
spcs <- dir_ls('./models/maxent') %>% as.character()
fles <- map(spcs, dir_ls) %>%
  flatten %>% 
  as.character() %>% 
  dir_ls() %>% 
  grep('predict_rslt', ., value = T) %>% 
  as.character() %>% 
  grep('.tif$', ., value = T)

# Acuaticos
acua <- dir_ls('./results/acuaticos') %>% terra::rast()

# Terrestres
trrs <- dir_ls('./results/terrestres') %>% terra::rast()

acua <- terra::resample(acua, trrs, method = 'bilinear')

# To make a stack file ----------------------------------------------------

stck <- c(acua, trrs)
stck <- raster::stack(stck)

# To scale 0 to 1 ---------------------------------------------------------
plot(stck[[1]])

plot(rescale0to1(stck[[1]]))

for(i in 1:nlayers(stck)){
  cat(i, '\n')
  stck[[i]] <- rescale0to1(stck[[i]])
}

# Raster to table ---------------------------------------------------------
tble <- rasterToPoints(stck, spatial = FALSE)
tble <- as.data.frame(tble)
tble <- as_tibble(tble)
colnames(tble) <- gsub('predict_rslt_sp_', '', colnames(tble))

scale(stck[[1]])

suma <- apply(tble[,3:ncol(tble)], 1, sum, na.rm = T)
tble <- mutate(tble, suma = suma)

plot(rasterFromXYZ(tble[,c(1, 2, 13)]))

rslt <- rasterFromXYZ(tble[,c(1, 2, 13)])
rslt <- rescale0to1(rslt)



