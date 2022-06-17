
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, crayon, tidyverse, gtools, fs, glue, raster, dismo, usdm)

g <- gc(reset = TRUE)
rm(list = ls())
options(warn = -1, scipen = 999)

# Function ----------------------------------------------------------------
chck_dupl <- function(fle){
  
  cat(green(fle), '\n')
  
  # Read the table
  pnt <- suppressMessages(read_csv(fle))
  
  # Conditional to read the table
  if(ncol(pnt) == 1){
    pnt <- read.table(fle, sep = ';', header = T)
  } else {
    pnt <- read_csv(fle)
  }
  
  # Check duplicated by cell
  dup <- raster::extract(mask, pnt[,c('longitud', 'latitud')], cellnumbers = T)
  pnt <- mutate(pnt, dup = dup[,1])
  vec <- duplicated(pnt$dup)
  pnt <- pnt[,c('longitud', 'latitud', 'dup')]
  pnt <- mutate(pnt, specie = basename(fle) %>% gsub('.csv', '', .))
  pnt <- pnt[!duplicated(pnt$dup),]
  pnt <- dplyr::select(pnt, specie, longitud, latitud, dup)
  return(pnt)
  
}

# Load data ---------------------------------------------------------------

bioc <- raster::stack('./raster/climate/baseline/gwr_bioc.tif')
names(bioc) <- glue('bioc_{1:19}')
mask <- bioc[[1]] * 0 + 1
grps <- dir_ls('./tbl/points') %>% as.character()
fles <- dir_ls(grps) %>% as.character()

# To check the duplicated  ------------------------------------------------
dupt <- map(fles, chck_dupl)
dupt <- bind_rows(dupt)
smmr_dupt <- dupt %>% group_by(specie) %>% dplyr::summarise(count = n()) %>% ungroup()
View(smmr_dupt)
smmr_dupt <- filter(smmr_dupt, count > 15)
spcs <- pull(smmr_dupt, specie)
saveRDS(spcs, file = './rds/spcies.rds')

# Filtering
fles <- grep(paste0(spcs, collapse = '|'), fles, value = T)

# To make the model -------------------------------------------------------
fle <- fles[1]

make_model <- function(fle){
  
  cat(green(fle), '\n')
  
  # To read and remove duplicated by cell
  dup <- chck_dupl(fle)
  pnt <- dup[!duplicated(dup$dup),]
  pnt <- dplyr::select(pnt, -dup)

  # To extract the values for the climate (biovars)
  vls <- raster::extract(bioc, pnt[,c('longitud', 'latitud')])
  vls <- cbind(pnt, vls)
  vls <- as_tibble(vls)
  vls <- drop_na(vls)
  vls <- as.data.frame(vls)
  
  # VIF analysis
  vrs <- vifstep(x = vls[,4:22], th = 10)
  vrs <- vrs@results$Variables
  
  # To select from the dataframe
  vls <- dplyr::select(vls, 'specie', 'longitud', 'latitud', vrs)
  vls <- as_tibble(vls)
  write.csv(vls, glue('./tbl/swd/swd_{basename(fle)}'), row.names = FALSE)
  
  # To generate the pseudo-absences 
  nrw <- nrow(vls)
  
  
  
}




