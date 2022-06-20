
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, rmapshaper, crayon, tidyverse, gtools, fs, glue, raster, dismo, usdm)

g <- gc(reset = TRUE)
rm(list = ls())
options(warn = -1, scipen = 999)

# Functions ---------------------------------------------------------------
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
make_model <- function(fle){
  
  cat(green(fle), '\n')
  
  # To read and remove duplicated by cell
  dup <- chck_dupl(fle)
  pnt <- dup[!duplicated(dup$dup),]
  pnt <- dplyr::select(pnt, -dup)
  spc <- basename(fle) %>% gsub('.csv', '', .) %>% gsub('Sp_', '', .)
  
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
  
  # To make a new stack with bioclimatic variables 
  vrs
  bio <- bioc[[parse_number(vrs)]]
  
  # To generate the pseudo-absences 
  nrw <- nrow(vls)
  
  # Create a buffer 
  bff <- st_as_sf(vls, coords = c('longitud', 'latitud'), crs = st_crs(4326)) %>% 
    st_transform(st_crs(3116)) %>% 
    st_buffer(x = ., dist = 1000) %>%
    ms_dissolve() %>% 
    st_transform(st_crs(4326))
  
  msk <- raster::mask(mask, bff, inverse = TRUE)
  bck <- rasterToPoints(msk) %>% 
    as_tibble() %>% 
    sample_n(tbl = ., size = nrw * 2, replace = FALSE) %>% 
    dplyr::select(x, y)
  
  bck <- cbind(bck, raster::extract(bioc, bck[,1:2]))
  bck <- dplyr::select(bck, x, y, vrs)
  
  head(vls)
  head(bck)
  
  # Tidy the dataframes
  vls <- dplyr::select(vls, x = longitud, y = latitud, starts_with('bio'))
  
  # Add pb
  bck$pb <- 0
  vls$pb <- 1
  
  # Relocate 
  occ <- dplyr::select(vls, pb, x, y, starts_with('bio'))
  bck <- dplyr::select(bck, pb, x, y, starts_with('bio'))
  
  fld_occ <- kfold(occ, k = 5)
  fld_bck <- kfold(bck, k = 5)
  
  mdls <- purrr::map(.x = 1:5, .f = function(m){
    
    cat(green('Starts with ', m, '\n'))
    
    tst <- occ[fld_occ == m,]
    trn <- occ[fld_occ != m,]
    tst_bck <- bck[fld_bck == m,]
    trn_bck <- bck[fld_bck != m,]
    
    # Presences and pseudabsences (join)
    env <- rbind(trn, trn_bck)
    y <- c(trn$pb, trn_bck$pb)
    
    # Output directory
    out <- glue('./models/maxent/{spc}/run_1/model_{m}')
    ifelse(!file.exists(out), dir_create(out), print('Directory exists!\n'))
    
    # To make the model
    mxn <- maxent(env[,4:ncol(env)], y, argcs = c('addsamplestobackground=true', 'responsecurves'), path = out)
    rst <- raster::predict(3, bio, progress = 'txt')
    
    save(mxn, file = glue('{out}/mxn.rda'))
    writeRaster(rst, filename = glue('{out}/predict_mdl_{m}.tif'), overwrite = T)
    
    # To evaluate the model
    evl <- evaluate(mxn, p = data.frame(tst[,4:ncol(tst)]), a = data.frame(tst_bck[,4:ncol(tst_bck)]))
    prc <- as.data.frame(mxn@results)
    prc <- data.frame(variables = vrs, percentage = prc[grep('contribution', rownames(prc)),], rutin = m)
    auc <- evl@auc
    tss <- evl@TPR + evl@TNR - 1
    tss <- evl@t[which.max(tss)]
    
    dfm <- data.frame(routine = m, threshold = tss, auc = auc)
    
    cat(red('Finish!\n'))
    return(list(rst, prc, dfm))
    
  })
  
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
purrr::map(.x = fles[2:length(fles)], .f = make_model)


