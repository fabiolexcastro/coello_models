
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, climateStability, corrplot, sf, tidyverse, gtools, rgeos, stringr, glue, fs)

g <- gc(reset = TRUE)
rm(list = ls())
options(warn = -1, scipen = 999)

# Load data ---------------------------------------------------------------
path <- './shp/zonificacion/zonificacion/SE'
fles <- dir_ls(path, regexp = '.shp$')

# Read as shape files -----------------------------------------------------
shpf <- map(fles, vect)

# Empty raster -----------------------------------------------------------
extn <- terra::ext(shpf[[1]])
resolution <- 30
naraster <- terra::rast(extn, ncols = (diff(extn[1:2])/resolution),
                        nrows = (diff(extn[3:4])/resolution), 
                        crs = terra::crs(shpf[[1]]))

naraster[] <- 0
naraster <- terra::crop(naraster, bsin)
naraster <- terra::mask(naraster, shpf[[1]])

# To rasterize ------------------------------------------------------------

agua <- terra::rasterize(shpf[[1]], naraster, field = 'PESO')
cldd <- terra::rasterize(shpf[[2]], naraster, field = 'peso')
crbn <- terra::rasterize(shpf[[3]], naraster, field = 'PESO')
cltv <- terra::rasterize(shpf[[4]], naraster, field = 'pesos')
ntrt <- terra::rasterize(shpf[[5]], naraster, field = 'PESO')
plnz <- terra::rasterize(shpf[[6]], naraster, field = 'peso')
rcrs <- terra::rasterize(shpf[[7]], naraster, field = 'PESO')
suel <- terra::rasterize(shpf[[8]], naraster, field = 'peso')

# Create a stack 
stck <- do.call('c', list(agua, cldd, crbn, cltv, ntrt, plnz, rcrs, suel))

# Make a correlation  -----------------------------------------------------
tble <- terra::as.data.frame(stck, xy = T) 
colnames(tble) <- c('lon', 'lat', basename(fles))
tble <- as_tibble(tble)
colnames(tble) <- gsub('.shp', '', colnames(tble))
mtrx <- as.matrix(tble[,3:10])
crlt <- cor(mtrx)

dir_create('./png')
png(filename = './png/corplot_vars_mesli.png', units = 'in', width = 7, height = 6, res = 300)
corrplot(crlt, method = 'number')
dev.off()
