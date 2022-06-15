

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, crayon, tidyverse, gtools, fs, glue, raster, dismo, usdm)

g <- gc(reset = TRUE)
rm(list = ls())
options(warn = -1, scipen = 999)

spce <- dirs[2]

# Function to check the points --------------------------------------------
check_points <- function(spce){
  
  cat(green(basename(spce)), '\n')
  
  # To read the file as a table
  tbl <- read_csv(spce)
  
  # Conditional to read the table
  if(ncol(tbl) == 1){
    tbl <- read.table(spce, sep = ';', header = T)
  } else {
    tbl <- read_csv(spce)
  }
  
  colnames(tbl)[1] <- 'especie'
  
  # To extract the cell size
  vls <- raster::extract(mask, tbl[,c('longitud', 'latitud')], cellnumbers = TRUE)
  tbl <- mutate(tbl, cell = vls[,1])
  tbl <- tbl[!duplicated(tbl$cell),]
  
  cat(red('Finish!'), '\n')
  return(tbl)
  
}

# Load data ---------------------------------------------------------------
path <- './tbl/points'
grps <- dir_ls(path)
dirs <- map(grps, dir_ls)
dirs <- flatten(dirs)
dirs <- as.character(dirs)

# Study zone
zone <- st_read('./shp/b_coello.shp')
zone <- st_transform(zone, crs = st_crs(4326))

# Climate
clma <- dir_ls('./raster/climate/baseline', regexp = '.asc$')
clma <- mixedsort(clma)
clma <- raster::stack(clma)
mask <- clma[[1]] * 0 + 1
names(mask) <- 'mask'

rasterToPoints(clma, spatial = FALSE) %>% nrow()

# To check ----------------------------------------------------------------
rslt <- map(dirs, check_points)
rslt <- bind_rows(rslt)
rslt <- rslt[,1:4]

# Summarize
smmr <- rslt %>% 
  group_by(especie) %>% 
  dplyr::summarise(count = n()) %>% 
  ungroup()

write.csv(smmr, './tbl/summarize_conteo.csv', row.names = FALSE)

# Check the nrow for each table
sapply(rslt, nrow)









