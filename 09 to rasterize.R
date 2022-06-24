

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
cver <- st_read('./shp/zonificacion/zonificacion/uso_suelo/coello_luc.shp')

# To project the shapes ---------------------------------------------------
extn <- terra::ext(mesl)
resolution <- 30
naraster <- terra::rast(extn, ncols = (diff(extn[1:2])/resolution),
                              nrows = (diff(extn[3:4])/resolution), 
                              crs = terra::crs(mesl))

naraster[] <- 0
naraster <- terra::crop(naraster, bsin)
naraster <- terra::mask(naraster, bsin)# terra::as.data.frame(naraster, xy = TRUE) %>% nrow()

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
popu.rstr <- terra::rasterize(x = popu, y = naraster, field = 'total_pers')
popu.rstr <- rescale0to1(raster::raster(popu.rstr))
popu.rstr <- 1 - popu.rstr
writeRaster(popu.rstr, './raster/indices/popu/dens_popu.tif')

# Indice del uso del suelo
clss <- pull(cver, LULC_desc) %>% sort()
clss <- tibble(class = clss)
clss <- mutate(clss, index = c(0, 0.5, 1, 1, 1, 1, 0.3, 0.5, 0.2, 0.2, 0.2, 0.5, 0.2, 0.2, 0.2, 0, 1, 0, 0, 0, 0.5, 1, 0))

cver <- inner_join(cver, clss, by = c('LULC_desc' = 'class'))

clss.rstr <- terra::rasterize(x = terra::vect(cver), y = naraster, field = 'index')
unique(clss.rstr)
clss.rstr <- round(clss.rstr, 1)

writeRaster(clss.rstr, './raster/indices/cover/cover.tif')


# Weighted mean -----------------------------------------------------------
x1 <- c(9, 5, 2, 7, 3, 6, 4, 5)                    # Create example data
w1 <- c(2, 3, 1, 5, 7, 1, 3, 7)  
weighted.mean(x1, w1) 


b <- brick(system.file("external/rlogo.grd", package="raster"))

# give least weight to first layer, most to last layer
wm1 <- weighted.mean(b, w=1:3)

# spatially varying weights
# weigh by column number
w1 <- init(b, v='col')

# weigh by row number
w2 <- init(b, v='row')
w <- stack(w1, w2, w2)

wm2 <- weighted.mean(b, w=w)
