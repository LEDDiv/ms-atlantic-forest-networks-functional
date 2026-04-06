#' ---
#' title: regeneration
#' author: mauricio vancine
#' date: 2025-12-03
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(terra)
library(tidyterra)
library(rgrass)

# import data -------------------------------------------------------------

# hexagons
hex <- terra::vect("01_data/03_vect/hex_sel.gpkg") %>% 
  tidyterra::mutate(hid = 1:nrow(.)) %>% 
  tidyterra::relocate(hid, .before = 1)
hex
plot(hex)

# utm
utm <- terra::vect("01_data/03_vect/utm_zones_epsg.shp")
utm
plot(utm)

# regeneration
reg <- terra::vect("01_data/08_mapbiomas_recuperation/recuperacao_080126_v2.gpkg")
reg

reg %>% 
  as_tibble() %>% 
  count(status)

# landscape
l <- terra::rast("01_data/02_raster/mapbiomas_brazil_af_trinacional_2023_forest_na_patches.tif")
l

# overlap analysis --------------------------------------------------------

# grassdb
rgrass::initGRASS(gisBase = system("grass --config path", inter = TRUE),
                  gisDbase = "01_data/grassdb",
                  location = "newLocation",
                  mapset = "PERMANENT",
                  override = TRUE)

# import
rgrass::execGRASS(cmd = "v.in.ogr", 
                  input = "01_data/03_vect/hex_sel.gpkg", 
                  output = "hex")

rgrass::execGRASS(cmd = "v.in.ogr", 
                  input = "01_data/08_mapbiomas_recuperation/recuperacao_080126_v2.gpkg", 
                  output = "rec")


# overlay
rgrass::execGRASS(cmd = "v.select", 
                  flags = "overwrite",
                  ainput = "hex", 
                  binput = "rec",
                  output = "hex_rec",
                  operator = "overlap")

# export
rgrass::execGRASS(cmd = "v.out.ogr",
                  input = "hex_rec",
                  output = "01_data/08_mapbiomas_recuperation/00_hex_rec.gpkg")


# rasterize and patch id --------------------------------------------------

# import raster
rgrass::execGRASS(cmd = "r.in.gdal", 
                  input = "01_data/02_raster/mapbiomas_brazil_af_trinacional_2023_forest_na_patches.tif",
                  output = "forest_id")

# region
rgrass::execGRASS(cmd = "g.region", 
                  flags = c("a", "p"), 
                  raster = "forest_id")

# rasterize
rgrass::execGRASS(cmd = "v.to.rast",
                  input = "rec", 
                  output = "rec",
                  use = "val")


# metrics
metrics_i <- NULL
# for(i in 1:nrow(reg)){}
i = 2

print(i)

# hexagon
hex_i <- hex[i, ]

# utm
utm_int <- terra::relate(utm, hex_i, "intersects")
utm_i <- utm[utm_int,]  

# landscape
l_i <- terra::crop(l, hex_i, mask = TRUE)
l_i_utm <- terra::project(l_i, paste0("EPSG:", utm_i$epsg_code[1]), method = "near")

# hex
hex_i_utm <- terra::project(hex_i, paste0("EPSG:", utm_i$epsg_code[1]))

# regeneration
reg_hex_i <- terra::crop(reg, hex_i)

if(nrow(reg_hex_i) > 0){
  
  # utm
  reg_hex_i_utm <- terra::project(reg_hex_i, paste0("EPSG:", utm_i$epsg_code[1]))
  reg_hex_i_utm_r <- terra::rasterize(reg_hex_i_utm, l_i_utm)
  
  
  # new landscape
  l_i_reg_utm <- terra::ifel((is.na(l_i_utm) & reg_hex_i_utm_r == 1) | 
                               (l_i_utm == 0 & reg_hex_i_utm_r == 1), 1, l_i_utm)
  
  # patches
  l_i_utm_fid <- terra::patches(l_i_utm, directions = 8, zeroAsNA = TRUE)
  reg_hex_i_utm_r_rid <- terra::patches(reg_hex_i_utm_r, directions = 8)
  l_i_reg_utm_fid <- terra::patches(l_i_reg_utm, directions = 8, zeroAsNA = TRUE)
  
  # export
  hid_name <- ifelse(hex_i$hid < 10, paste0("0000", hex_i$hid), 
                     ifelse(hex_i$hid < 100, paste0("000", hex_i$hid), 
                            ifelse(hex_i$hid < 1000, paste0("00", hex_i$hid), 
                                   ifelse(hex_i$hid < 10000, paste0("0", hex_i$hid), 
                                          hex_i$hid))))
  
  terra::writeVector(hex_i_utm, paste0("01_data/08_mapbiomas_recuperation/hex", hid_name, ".gpkg"))
  
  terra::writeVector(reg_hex_i_utm, paste0("01_data/08_mapbiomas_recuperation/recuperacao_hex", hid_name, ".gpkg"))
  terra::writeRaster(reg_hex_i_utm_r, paste0("01_data/08_mapbiomas_recuperation/recuperacao_hex", hid_name, ".tif"))
  
  
  
  # landscape values
  l_i_reg_utm_val <- l_i_reg_utm %>% 
    terra::freq() %>%
    tibble::as_tibble() %>% 
    dplyr::filter(value == 1)
  
  
  #### 1. area mn ----
  if(nrow(l_i_reg_utm_val) == 1){
    am <- landscapemetrics::lsm_c_area_mn(l_i_reg_utm) %>% 
      dplyr::filter(class == 1) %>% 
      dplyr::mutate(area_mn = value) %>% 
      dplyr::select(area_mn)
    
  } else{
    am <- tibble::tibble(area_mn = 0)
  }
  
  #### 2. percentage of landscape ----
  if(nrow(l_i_reg_utm_val) == 1){
    pl <- landscapemetrics::lsm_c_pland(l_i_reg_utm) %>% 
      dplyr::filter(class == 1) %>% 
      dplyr::mutate(pland = value) %>%
      dplyr::select(pland)
    
  } else{
    pl <- tibble::tibble(pland = 0)
  }
  
  #### 3. number of patches ----
  if(nrow(l_i_reg_utm_val) == 1){
    np <- landscapemetrics::lsm_c_np(l_i_reg_utm) %>% 
      dplyr::filter(class == 1) %>% 
      dplyr::mutate(np = value) %>% 
      dplyr::select(np)
    
  } else{
    np <- tibble::tibble(np = 0)
  }
  
  #### 4. patch density ----
  if(nrow(l_i_reg_utm_val) == 1){
    pd <- landscapemetrics::lsm_c_pd(l_i_reg_utm) %>% 
      dplyr::filter(class == 1) %>% 
      dplyr::mutate(pd = value) %>%
      dplyr::select(pd)
    
  } else{
    pd <- tibble::tibble(pd = 0)
  }
  
  #### 5. euclidean nearest-neighbor distance ----
  if(np$np > 1){
    enn <- landscapemetrics::lsm_c_enn_mn(l_i_reg_utm) %>% 
      dplyr::filter(class == 1) %>% 
      dplyr::mutate(enn_mn = value) %>%
      dplyr::select(enn_mn)
    
  } else{
    enn <- tibble::tibble(enn_mn = 0)
  }
  
  #### 6. aggregation index ----
  if(np$np > 1){
    ai <- landscapemetrics::lsm_c_ai(l_i_reg_utm) %>% 
      dplyr::filter(class == 1) %>% 
      dplyr::mutate(ai = value) %>%
      dplyr::select(ai)
    
  } else{
    ai <- tibble::tibble(ai = 0)
  }
  
  #### 7. edge density ----
  if(nrow(l_i_reg_utm_val) == 1){
    ed <- landscapemetrics::lsm_c_ed(l_i_reg_utm) %>% 
      dplyr::filter(class == 1) %>% 
      dplyr::mutate(ed = value) %>%
      dplyr::select(ed)
    
  } else{
    ed <- tibble::tibble(ed = 0)
  }
  
  #### 8. largest patch index ----
  if(nrow(l_i_reg_utm_val) == 1){
    lpi <- landscapemetrics::lsm_c_lpi(l_i_reg_utm) %>% 
      dplyr::filter(class == 1) %>% 
      dplyr::mutate(lpi = value) %>%
      dplyr::select(lpi)
    
  } else{
    lpi <- tibble::tibble(lpi = 0)
  }
  
  #### 9. perimeter-area ratio ----
  if(nrow(l_i_reg_utm_val) == 1){
    para <- landscapemetrics::lsm_c_para_mn(l_i_reg_utm) %>% 
      dplyr::filter(class == 1) %>% 
      dplyr::mutate(para_mn = value) %>%
      dplyr::select(para_mn)
    
  } else{
    para <- tibble::tibble(para_mn = 0)
  }
  
  #### 10. contiguity index ----
  if(np$np > 1){
    contig <- landscapemetrics::lsm_c_contig_mn(l_i_reg_utm) %>% 
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
readr::write_csv(metrics_i, "01_data/07_landscape_metrics/07_landscape_metrics_regeneration.csv")

}


