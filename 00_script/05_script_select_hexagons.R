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

hex_camp <- filter(hex, hid == 15314)
hex_camp

# hex
hex10 <- terra::vect("01_data/03_vect/10_hex_teste.gpkg")
hex10

hex10_hid <- dplyr::filter(hex, cat %in% hex10$cat)
hex10_hid

hex10$hid <- hex10_hid$hid
hex10

hex10 <- rbind(hex10, hex_camp)
hex10

# raster
f <- terra::rast("01_data/02_raster/mapbiomas_brazil_af_trinacional_2023_forest_na_patches.tif")
f

# sdm
# sdm_b <- readr::read_tsv("01_data/05_hex_sdm/00_hex_bird_sdm.csv")
# sdm_p <- readr::read_tsv("01_data/05_hex_sdm/00_hex_plant_sdm.csv")  

# crop
for(i in 1:nrow(hex10)){
  
  hex10_i <- hex10[i, ]
  
  f_i <- terra::crop(f, hex10_i, mask = TRUE)
  v_i <- terra::as.polygons(f_i)
  
  sdm_b_i <- dplyr::filter(sdm_b, hid == hex10_i$hid)
  sdm_p_i <- dplyr::filter(sdm_p, hid == hex10_i$hid)
  
  terra::writeVector(hex10_i, paste0("02_test/hex10_", hex10_i$hid, ".gpkg"), overwrite = TRUE)
  terra::writeVector(v_i, paste0("02_test/forest_hex10_", hex10_i$hid, ".gpkg"), overwrite = TRUE)
  
  readr::write_csv(sdm_b_i, paste0("02_test/sdm_birds_", hex10_i$hid, ".csv"))
  readr::write_csv(sdm_p_i, paste0("02_test/sdm_plants_", hex10_i$hid, ".csv"))
  
}

# aggregate fragments -----------------------------------------------------

# import
f <- sf::st_read("02_test/forest_hex10_4792.gpkg")
f

f %>% 
  sf::st_drop_geometry() %>% 
  count(mapbiomas_brazil_af_trinacional_2023_forest_na_patches)

