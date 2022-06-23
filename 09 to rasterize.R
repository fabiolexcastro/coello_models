

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, climateStability, sf, tidyverse, gtools, rgeos, stringr, glue, fs)

g <- gc(reset = TRUE)
rm(list = ls())
options(warn = -1, scipen = 999)

# Load data ---------------------------------------------------------------
col1 <- geodata::gadm(country = 'COL', level = 1, path = './tmpr')
dpts <- terra::vect('D:/data/IGAC/MGN2018_MPIO_POLITICO/MGN_MPIO_POLITICO.shp')
dpts <- dpts[dpts$DPTO_CNMBR == 'TOLIMA',]
mesl <- terra::vect('./shp/zonificacion/zonificacion/mesli/mesli_coello.shp')
iuag <- terra::vect('./shp/zonificacion/zonificacion/IUA/IUAl_Cca_Coello.shp')
bsin <- terra::vect('./shp/zonificacion/zonificacion/cuenca r_coello/cuenca_coello.shp')
popu <- terra::vect('./shp/zonificacion/zonificacion/poblacion/grilla_dane.gpkg')

# Cobertura de uso del suelo (rasterizacion) - Aqui quedamos

# To project the shapes ---------------------------------------------------
extn <- terra::ext(mesl)
resolution <- 30
naraster <- terra::rast(extn, ncols = (diff(extn[1:2])/resolution),
                              nrows = (diff(extn[3:4])/resolution), 
                              crs = terra::crs(mesl))

naraster[] <- 0
naraster <- terra::crop(naraster, bsin)
naraster <- terra::mask# terra::as.data.frame(naraster, xy = TRUE) %>% nrow()

# To rasterize ------------------------------------------------------------

# Mesli
mesl.rstr <- terra::rasterize(x = mesl, y = naraster, field = 'MESLI_FINA')
raster::writeRaster(mesl.rstr, './raster/indices/mesli/mesli.tif', overwrite = TRUE)

# Indice del uso del agua
iuag$IUA_
iuag <- terra::project(iuag, terra::crs(mesl))
iuag.rstr <- terra::rasterize(x = iuag, y = naraster, fiel = 'IUA_')
iuag.rstr <- rescale0to1(raster::raster(iuag.rstr))
iuag.rstr <- rasterToPoints(iuag.rstr, spatial = F) %>% as_tibble() %>% terra::rast(., type = 'xyz')
iuag.rstr <- 1 - iuag.rstr

writeRaster(iuag.rstr, './raster/indices/iuag/iuag.tif')

# Densidad de poblacion
popu
popu <- terra::project(popu, terra::crs(mesl))
popu.rstr <- terra::rasterize(x = popu, y = naraster, fiel = 'total_pers')
popu.rstr <- rescale0to1(raster::raster(popu.rstr))
popu.rstr <- 1 - popu.rstr
writeRaster(popu.rstr, './raster/indices/popu/dens_popu.tif')

# Indice del uso del suelo




