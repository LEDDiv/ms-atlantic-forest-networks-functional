#' ---
#' title: 
#' author: mauricio vancine
#' date: 2025-12-03
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(terra)
library(tidyterra)
library(landscapemetrics)

# import data -------------------------------------------------------------

# hexagons
hex <- terra::vect("01_data/03_vect/hex_sel.gpkg") %>% 
  tidyterra::mutate(hid = 1:nrow(.)) %>% 
  tidyterra::relocate(hid, .before = 1)
hex
plot(hex)

# landscape
l <- terra::rast("01_data/02_raster/mapbiomas_brazil_af_trinacional_2023_forest.tif")
l

# utm
utm <- terra::vect("01_data/03_vect/utm_zones_epsg.shp")
utm
plot(utm)

# landscapes
metrics_i <- NULL
for(i in 1:nrow(hex)){

  print(i)
  
  hex_i <- hex[i, ]
  
  utm_int <- terra::relate(utm, hex_i, "intersects")
  utm_i <- utm[utm_int,]
  
  l_i <- terra::crop(l, hex_i, mask = TRUE)
  l_i_utm <- terra::project(l_i, paste0("EPSG:", utm_i$epsg_code[1]), method = "near")
  
  # values
  l_i_utm_val <- l_i_utm %>% 
    terra::freq() %>%
    tibble::as_tibble() %>% 
    dplyr::filter(value == 1)
  
  #### 1. area mn ----
  if(nrow(l_i_utm_val) == 1){
    am <- landscapemetrics::lsm_c_area_mn(l_i_utm) %>% 
      dplyr::filter(class == 1) %>% 
      dplyr::mutate(area_mn = value) %>% 
      dplyr::select(area_mn)
    
  } else{
    am <- tibble::tibble(area_mn = 0)
  }
  
  #### 2. percentage of landscape ----
  if(nrow(l_i_utm_val) == 1){
    pl <- landscapemetrics::lsm_c_pland(l_i_utm) %>% 
      dplyr::filter(class == 1) %>% 
      dplyr::mutate(pland = value) %>%
      dplyr::select(pland)
    
  } else{
    pl <- tibble::tibble(pland = 0)
  }
  
  #### 3. number of patches ----
  if(nrow(l_i_utm_val) == 1){
    np <- landscapemetrics::lsm_c_np(l_i_utm) %>% 
      dplyr::filter(class == 1) %>% 
      dplyr::mutate(np = value) %>% 
      dplyr::select(np)
    
  } else{
    np <- tibble::tibble(np = 0)
  }
  
  #### 4. patch density ----
  if(nrow(l_i_utm_val) == 1){
    pd <- landscapemetrics::lsm_c_pd(l_i_utm) %>% 
      dplyr::filter(class == 1) %>% 
      dplyr::mutate(pd = value) %>%
      dplyr::select(pd)
    
  } else{
    pd <- tibble::tibble(pd = 0)
  }
  
  #### 5. euclidean nearest-neighbor distance ----
  if(np$np > 1){
    enn <- landscapemetrics::lsm_c_enn_mn(l_i_utm) %>% 
      dplyr::filter(class == 1) %>% 
      dplyr::mutate(enn_mn = value) %>%
      dplyr::select(enn_mn)
    
  } else{
    enn <- tibble::tibble(enn_mn = 0)
  }
  
  #### 6. aggregation index ----
  if(np$np > 1){
    ai <- landscapemetrics::lsm_c_ai(l_i_utm) %>% 
      dplyr::filter(class == 1) %>% 
      dplyr::mutate(ai = value) %>%
      dplyr::select(ai)
    
  } else{
    ai <- tibble::tibble(ai = 0)
  }
  
  #### 7. edge density ----
  if(nrow(l_i_utm_val) == 1){
    ed <- landscapemetrics::lsm_c_ed(l_i_utm) %>% 
      dplyr::filter(class == 1) %>% 
      dplyr::mutate(ed = value) %>%
      dplyr::select(ed)
    
  } else{
    ed <- tibble::tibble(ed = 0)
  }
  
  #### 8. largest patch index ----
  if(nrow(l_i_utm_val) == 1){
    lpi <- landscapemetrics::lsm_c_lpi(l_i_utm) %>% 
      dplyr::filter(class == 1) %>% 
      dplyr::mutate(lpi = value) %>%
      dplyr::select(lpi)
    
  } else{
    lpi <- tibble::tibble(lpi = 0)
  }
  
  #### 9. perimeter-area ratio ----
  if(nrow(l_i_utm_val) == 1){
    para <- landscapemetrics::lsm_c_para_mn(l_i_utm) %>% 
      dplyr::filter(class == 1) %>% 
      dplyr::mutate(para_mn = value) %>%
      dplyr::select(para_mn)
    
  } else{
    para <- tibble::tibble(para_mn = 0)
  }
  
  #### 10. contiguity index ----
  if(np$np > 1){
    contig <- landscapemetrics::lsm_c_contig_mn(l_i_utm) %>% 
      dplyr::filter(class == 1) %>% 
      dplyr::mutate(contig_mn = value) %>%
      dplyr::select(contig_mn)
    
  } else{
    contig <- tibble::tibble(contig_mn = 0)
  }
  
  # combine j 
  metrics_j <- dplyr::bind_cols(am, pl, np, pd, enn, ai, ed, lpi, para, contig) %>%
    dplyr::mutate(across(everything(), ~ round(.x, 4))) %>% 
    dplyr::mutate(id = hex_i$hid, .before = 1)
  
  # combine i
  metrics_i <- dplyr::bind_rows(metrics_i, metrics_j)
  
}
metrics_i

# export
readr::write_csv(metrics_i, "01_data/07_landscape_metrics/07_landscape_metrics.csv")

# end ---------------------------------------------------------------------
