#' ---
#' title: confer
#' author: mauricio vancine
#' date: 2023-10-12
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(terra)
library(tmap)

# import data -------------------------------------------------------------

# verify
ver <- readr::read_csv("01_data/97_verify/00_missed_hex_cause.csv")
ver

count(ver, cause)

# hexagons ----------------------------------------------------------------

# import
hex <- terra::vect("01_data/03_vect/hex_sel.gpkg") %>% 
  tidyterra::mutate(hid = 1:nrow(.)) %>% 
  tidyterra::relocate(hid, .before = 1)
hex

# 1_frag ------------------------------------------------------------------

# import
for(i in 1:nrow(ver)){
  
  print(ver[i, ]$hex_id)
  
  if(ver[i, 2] == "1_frag"){
    
    hex <- terra::vect(paste0("01_data/06_hex_forest_sdm/hex_", ver[i, 1], ".gpkg"))
    hex
    
    frag <- terra::vect(paste0("01_data/06_hex_forest_sdm/hex_forest_", ver[i, 1], ".gpkg"))
    frag
    
    map_1_frag <- tm_shape(hex) +
      tm_borders() +
      tm_shape(frag) +
      tm_polygons(fill = "forestgreen") +
      tm_title(text = ver[i, 1]) +
      tm_layout(frame = FALSE)
    tmap_save(tm = map_1_frag, paste0("01_data/97_verify/map_hex_", ver[i, 1], "_1_frag.png"))
    
  }
  
}

# verify
dir(path = "01_data/97_verify/", pattern = ".png")


# No_birds ----------------------------------------------------------------

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

sdm_bird_rich <- sum(sdm_bird)
sdm_bird_rich

plot(sdm_bird_rich)

# hexagons
ver_no_birds <- ver %>% 
  dplyr::filter(cause == "No_birds")
ver_no_birds

hex_no_birds <- terra::vect()
for(i in ver_no_birds$hex_id){
  
  print(i)
  hex_no_birds_i <- paste0("01_data/06_hex_forest_sdm/hex_", i, ".gpkg") %>% 
    terra::vect()
  
  hex_no_birds <- rbind(hex_no_birds, hex_no_birds_i)

  }
hex_no_birds

plot(sdm_bird_rich)
plot(hex_no_birds, border = "red", add = TRUE)

# median
hex_no_birds_md <- sdm_bird %>% 
  terra::zonal(hex_no_birds, fun = "median", na.rm = TRUE) %>% 
  round(3)
hex_no_birds_md

sum(rowSums(hex_no_birds_md > 0.25) == 0)


hex_no_birds_i <- paste0("01_data/06_hex_forest_sdm/hex_", ver_no_birds$hex_id[1], ".gpkg") %>% 
  terra::vect()
hex_no_birds_i

plot(hex[hex_no_birds_i$hid,])
plot(hex_no_birds_i, add = TRUE, border = "red")

# No_plants ----------------------------------------------------------------

# import
sdm_plant_names <- dir("01_data/04_sdms/02_sdm_adj", pattern = ".tif*") %>% 
  stringr::str_replace_all(".tif", "")
sdm_plant_names

sdm_plant <- dir("01_data/04_sdms/02_sdm_adj", pattern = ".tif*", full.names = TRUE) %>% 
  terra::rast()
sdm_plant

names(sdm_plant) <- sdm_plant_names
sdm_plant

# sdm_plant_rich <- sum(sdm_plant)
# sdm_plant_rich
# plot(sdm_plant_rich)

# hexagons
ver_no_plants <- ver %>% 
  dplyr::filter(cause == "No_plants")
ver_no_plants

hex_no_plants <- terra::vect()
for(i in ver_no_plants$hex_id){
  
  print(i)
  hex_no_plants_i <- paste0("01_data/06_hex_forest_sdm/hex_", i, ".gpkg") %>% 
    terra::vect()
  
  hex_no_plants <- rbind(hex_no_plants, hex_no_plants_i)
  
}
hex_no_plants

plot(hex)
plot(hex_no_plants, border = "red", add = TRUE)
plot(hex_no_birds, border = "blue", add = TRUE)

# median
hex_no_plants_md <- sdm_plant %>% 
  terra::zonal(hex_no_plants, fun = "median", na.rm = TRUE) %>% 
  round(3)
hex_no_plants_md

sum(rowSums(hex_no_plants_md > 0.25) == 0)

# end ---------------------------------------------------------------------
