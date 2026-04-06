#' ---
#' title: 
#' author: mauricio vancine
#' date: 2023-10-12
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(terra)
library(tidyterra)

# import data -------------------------------------------------------------

# hex
hex <- terra::vect("01_data/03_vect/hex_sel.gpkg") %>% 
  tidyterra::mutate(hid = 1:nrow(.)) %>% 
  tidyterra::relocate(hid, .before = 1)
hex
plot(hex)

# raster
f <- terra::rast("01_data/02_raster/mapbiomas_brazil_af_trinacional_2023_forests.tif")
f

# sdm
sdm_b <- vroom::vroom("01_data/05_hex_sdm/00_hex_bird_sdm.csv")
sdm_p <- vroom::vroom("01_data/05_hex_sdm/00_hex_plant_sdm.csv")

# crop
for(i in 20008:nrow(hex)){

  print(i)
  
  hex_i <- hex[i, ]
  
  f_i <- terra::crop(f, hex_i, mask = TRUE)
  f_i_val <- terra::values(f_i)
  
  if(all(is.na(f_i_val))){
    
  }else{
    
    v_i <- terra::as.polygons(f_i)
    v_i$pid <- v_i$mapbiomas_brazil_af_trinacional_2023_forest_na_patches
    v_i <- v_i[, -1]
    
    sdm_b_i <- dplyr::filter(sdm_b, hid == hex_i$hid)
    sdm_p_i <- dplyr::filter(sdm_p, hid == hex_i$hid)
    
    hid_name <- ifelse(hex_i$hid < 10, paste0("0000", hex_i$hid), 
                ifelse(hex_i$hid < 100, paste0("000", hex_i$hid), 
                ifelse(hex_i$hid < 1000, paste0("00", hex_i$hid), 
                ifelse(hex_i$hid < 10000, paste0("0", hex_i$hid), 
                       hex_i$hid))))
    hid_name
    
    if(all(is.na(sdm_b_i[, -1])) & rowSums(sdm_b_i[, -1], na.rm = TRUE) == 0 & 
       all(is.na(sdm_p_i[, -1])) & rowSums(sdm_p_i[, -1], na.rm = TRUE) == 0){

    } else{
      
      terra::writeVector(hex_i, paste0("01_data/06_hex_forest_sdm/hex_", hid_name, ".gpkg"), overwrite = TRUE)
      terra::writeVector(v_i, paste0("01_data/06_hex_forest_sdm/hex_forest_", hid_name, ".gpkg"), overwrite = TRUE)
      
      readr::write_csv(sdm_b_i, paste0("01_data/06_hex_forest_sdm/sdm_birds_", hid_name, ".csv"))
      readr::write_csv(sdm_p_i, paste0("01_data/06_hex_forest_sdm/sdm_plants_", hid_name, ".csv"))
    }
    
  }

}

# end ----------------------------------------------------------------------


