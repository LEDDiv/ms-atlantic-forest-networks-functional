#' ----
#' title: atlantic forest functional network 
#' author: mauricio vancine
#' date: 12-05-2025
#' ----

# prepare r -------------------------------------------------------------

# packages
library(tidyverse)
library(parallelly)
library(doParallel)
library(foreach)
library(terra)
library(sf)
library(tmap)

# move files --------------------------------------------------------------

# # list files
# list_files_sdm <- dir(path = "/media/mude/Seagate Portable Drive/data/ms_network_af_degradation_2025_05/Rubia e Mauricio/coisas mestrado_espacial/Mauricio/02_results/", 
#                       pattern = "_af_msdm.tif", full.names = TRUE, recursive = TRUE)
# list_files_sdm
# 
# # copy files
# file.copy(from = list_files_sdm, to = "01_data/04_sdms/00_continuous/")

# import birds --------------------------------------------------------------

# species list
sp_list_animal <- readr::read_csv2("01_data/04_sdms/00_species_list/fauna_species_list_frugivore_synonymes.csv") %>% 
  dplyr::select(1:3)
sp_list_animal

sp_list_animal_total <- sp_list_animal %>%  
  dplyr::pull(species) %>% 
  unique()
sp_list_animal_total

sp_list_bird <- sp_list_animal %>% 
  dplyr::filter(group == "bird") %>% 
  dplyr::pull(species) %>% 
  unique()
sp_list_bird

# import birds sdms
sdm_bird_names <- dir("01_data/04_sdms/01_sdms", pattern = ".tif*") %>% 
  stringr::str_replace_all(".tif", "") %>% 
  stringr::str_replace_all("03_05_pred_", "") %>% 
  stringr::str_replace_all("_af_msdm", "") %>% 
  stringr::str_subset(paste0(sp_list_bird, collapse = "|"))
sdm_bird_names

sdm_bird <- dir("01_data/04_sdms/01_sdms", pattern = ".tif*", full.names = TRUE) %>% 
  stringr::str_subset(paste0(sp_list_bird, collapse = "|")) %>% 
  terra::rast()
sdm_bird

names(sdm_bird) <- sdm_bird_names
sdm_bird

# import plants -----------------------------------------------------------

# sdm_plant_aux <- terra::rast("01_data/04_sdms/01_sdms/03_05_pred_Tocoyena bullata_af_msdm.tif")
# sdm_plant_aux
# plot(sdm_plant_aux)
# 
# for(i in sdm_plant_names){
#   
#   print(i)
#   
#   sdm_plant_file <- dir("01_data/04_sdms/01_sdms", pattern = ".tif*", full.names = TRUE) %>% 
#     stringr::str_subset(i)
#   
#   if(length(sdm_plant_file) > 0){
#     
#     sdm_plant_i <- sdm_plant_file %>%   
#       terra::rast() %>% 
#       terra::crop(sdm_plant_aux, mask = TRUE)
#     
#     terra::writeRaster(sdm_plant_i, paste0("01_data/04_sdms/02_sdm_adj/", i, ".tif"), overwrite = TRUE)
#     
#   }
#   
# }

# import
sdm_plant_names <- dir("01_data/04_sdms/02_sdm_adj", pattern = ".tif*") %>% 
  stringr::str_replace_all(".tif", "")
sdm_plant_names

sdm_plant <- dir("01_data/04_sdms/02_sdm_adj", pattern = ".tif*", full.names = TRUE) %>% 
  terra::rast()
sdm_plant

names(sdm_plant) <- sdm_plant_names
sdm_plant

readr::write_csv(sdm_birds_names, "sdm_birds_names.csv")
readr::write_csv(sdm_plant_names, "sdm_plant_names.csv")


# hexagons ----------------------------------------------------------------

# import hexagons
hex <- terra::vect("01_data/03_vect/hex_sel.gpkg") %>% 
  tidyterra::mutate(hid = 1:nrow(.)) %>% 
  tidyterra::relocate(hid, .before = 1)
hex
plot(hex)

# import data processed
hex_processed_bird <- dir("01_data/05_hex_sdm/00_raw/") %>% 
  stringr::str_subset("hex_bird_hid") %>% 
  stringr::str_replace("hex_bird_hid", "") %>% 
  stringr::str_replace(".csv", "") %>% 
  as.numeric() %>% 
  sort()
hex_processed_bird

hex_processed_plant <- dir("01_data/05_hex_sdm/00_raw/") %>% 
  stringr::str_subset("hex_plant_hid") %>% 
  stringr::str_replace("hex_plant_hid", "") %>% 
  stringr::str_replace(".csv", "") %>% 
  as.numeric() %>% 
  sort()
hex_processed_plant

hex_processed <- unique(c(hex_processed_bird, hex_processed_plant))
hex_processed

hex_not_processed <- setdiff(hex$hid, hex_processed)
hex_not_processed

# sdm and hexagons
doParallel::registerDoParallel(parallelly::availableCores(omit = 2))

# foreach::foreach(i=hex_processed:nrow(hex)) %dopar% {
for(i in hex_not_processed){
  
  print(i)
  
  hex_i <- hex[i,]
  
  hex_i_bird_md <- terra::zonal(sdm_bird, hex_i, fun = "median", na.rm = TRUE) %>% 
    tibble::as_tibble() %>%
    dplyr::rename_with(~ str_to_lower(.) %>%         
                  str_replace_all(" ", "_") %>% 
                  str_c("md_", .)) %>% 
    dplyr::mutate(hid = hex_i$hid, .before = 1)
  
  hex_i_plant_md <- terra::zonal(sdm_plant, hex_i, fun = "median", na.rm = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::rename_with(~ str_to_lower(.) %>%         
                         str_replace_all(" ", "_") %>% 
                         str_c("md_", .)) %>% 
    dplyr::mutate(hid = hex_i$hid, .before = 1)
  
  readr::write_csv(hex_i_bird_md, paste0("01_data/05_hex_sdm/00_raw/hex_bird_hid", i, ".csv"))
  readr::write_csv(hex_i_plant_md, paste0("01_data/05_hex_sdm/00_raw/hex_plant_hid", i, ".csv"))
    
}
doParallel::stopImplicitCluster()

# import data -------------------------------------------------------------

# birds
hex_bird_sdm <- dir("01_data/05_hex_sdm/00_raw", pattern = "bird", full.names = TRUE) %>% 
  vroom::vroom() %>% 
  dplyr::arrange(hid)
hex_bird_sdm

# plants
hex_plant_sdm <- dir("01_data/05_hex_sdm/00_raw", pattern = "plant", full.names = TRUE) %>% 
  vroom::vroom() %>% 
  dplyr::arrange(hid)
hex_plant_sdm

# export
vroom::vroom_write(hex_bird_sdm, "01_data/05_hex_sdm/00_hex_bird_sdm.csv")
vroom::vroom_write(hex_plant_sdm, "01_data/05_hex_sdm/00_hex_plant_sdm.csv")

# end ---------------------------------------------------------------------
