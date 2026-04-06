#' ---
#' title: treeco
#' author: mauricio vancine
#' date: 2025-10-12
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(terra)
library(sf)
library(tmap)
library(landscapemetrics)
library(geobr)

# options
options(timeout = 1e5)

# sp ----------------------------------------------------------------------

# sp
sp <- geobr::read_state("SP", year = 2020)
sp

sf::st_write(sp, "01_data/sp.gpkg")

tm_shape(sp) +
  tm_polygons()

# newfor ------------------------------------------------------------------

# import
newfor <- readxl::read_excel("01_data/NewFor_by_plot_2024_06_12_AM_AFR.xlsx") %>% 
  dplyr::filter(typology %in% c("Rest", "Reg"))
newfor

newfor_v <- sf::st_as_sf(newfor, coords = c("mean_long", "mean_lat"), crs = 4326)
newfor_v

sf::st_write(newfor_v, "01_data/newfor_v.gpkg")

tm_shape(newfor_v) +
  tm_bubbles(.3)

# treeco data ------------------------------------------------------------

# treeco
treeco <- readr::read_csv("01_data/sites_data_set_v1.csv") %>% 
  dplyr::filter(year >= 1990)
treeco

y <- count(treeco, year)
View(y)

# vector
treeco_v <- sf::st_as_sf(treeco, coords = c("long1", "lat1"), crs = 4326)
treeco_v

# export
sf::st_write(treeco_v, "01_data/treeco_v.gpkg")

# map
tm_shape(treeco_v) +
  tm_bubbles(size = .3)

# mapbiomas data ----------------------------------------------------------

# download
# download.file(url = "https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/lulc_10m/collection_2/integration/mapbiomas_10m_collection2_integration_v1-classification_2023.tif",
#               destfile = "01_data/mapbiomas_10m_collection2_integration_v1-classification_2023.tif")

# import
mapbiomas <- terra::rast("01_data/mapbiomas_10m_collection2_integration_v1-classification_2023.tif")
mapbiomas

# utm
utm <- sf::st_read("01_data/utm_zones_epsg.shp")
utm

tm_shape(utm) +
  tm_polygons() +
  tm_crs("auto")

# landscape metric ------------------------------------------------------

# treeco
treeco_metrics <- NULL
for(i in 1:nrow(treeco)){
  
  print(i)
  
  treeco_v_i <- treeco_v[i, ]
  treeco_v_i_buffer <- terra::buffer(vect(treeco_v_i), 5e3)
  utm_treeco_v_i <- utm[treeco_v_i,]
  
  mapbiomas_i <- terra::crop(mapbiomas, treeco_v_i_buffer, mask = TRUE)
  mapbiomas_i_utm <- terra::project(mapbiomas_i, utm_treeco_v_i$prj4, method = "near")
  mapbiomas_i_utm_forest <- terra::ifel(mapbiomas_i_utm %in% c(3, 5, 6, 49), 1, NA)
  
  if(!all(is.na(values(mapbiomas_i_utm_forest)))){
    m <- landscapemetrics::spatialize_lsm(mapbiomas_i_utm_forest, metric = "area")
    m_e <- round(as.numeric(terra::extract(m$layer_1$lsm_p_area, vect(treeco_v_i))$value), 2)
  }else{
    m_e <- 0
  }
  
  treeco_metrics_i <- treeco_v_i %>% 
    sf::st_drop_geometry() %>% 
    dplyr::mutate(area_ha = m_e)
  treeco_metrics <- rbind(treeco_metrics, treeco_metrics_i)
  
}
treeco_metrics

# export
readr::write_csv(treeco_metrics, "01_data/treeco_metrics.csv")

## newfor ----
newfor_metrics <- NULL
for(i in 1:nrow(newfor)){
  
  print(i)
  
  newfor_v_i <- newfor_v[i, ]
  newfor_v_i_buffer <- terra::buffer(vect(newfor_v_i), 5e3)
  utm_newfor_v_i <- utm[newfor_v_i,]
  
  mapbiomas_i <- terra::crop(mapbiomas, newfor_v_i_buffer, mask = TRUE)
  mapbiomas_i_utm <- terra::project(mapbiomas_i, utm_newfor_v_i$prj4, method = "near")
  mapbiomas_i_utm_forest <- terra::ifel(mapbiomas_i_utm %in% c(3, 5, 6, 49), 1, NA)
  
  m <- landscapemetrics::spatialize_lsm(mapbiomas_i_utm_forest, metric = "area")
  m_e <- round(as.numeric(terra::extract(m$layer_1$lsm_p_area, vect(newfor_v_i))$value), 2)
  
  newfor_metrics_i <- newfor_v_i %>% 
    sf::st_drop_geometry() %>% 
    dplyr::mutate(area_ha = m_e)
  newfor_metrics <- rbind(newfor_metrics, newfor_metrics_i)
}
newfor_metrics

# export
readr::write_csv(newfor_metrics, "01_data/newfor_metrics.csv")

# export ------------------------------------------------------------------
