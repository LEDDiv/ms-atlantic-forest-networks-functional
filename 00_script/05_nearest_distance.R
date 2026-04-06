#' ---
#' title: nearest distance
#' author: mauricio vancine
#' date: 2025-11-18
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(renv)
library(tidyverse)
library(sf)
library(tmap)

# source
source("00_script/function_st_nearest_dist.R")

# import data -------------------------------------------------------------

# patches
p <- sf::st_read("02_test/forest_hex10_15314.gpkg") %>% 
  dplyr::rename(pid = 1) 
p

tm_shape(p) +
  tm_polygons()

# nearest distance --------------------------------------------------------

# nearest distance
p_nearest_dist <- st_nearest_dist(p)
p_nearest_dist

p_nearest_dist$data
p_nearest_dist$points
p_nearest_dist$lines
p_nearest_dist$centroids
p_nearest_dist$coordinates

tm_shape(p) +
  tm_polygons(fill = "forestgreen") +
  tm_shape(p_nearest_dist$points) +
  tm_dots() +
  tm_shape(p_nearest_dist$lines) +
  tm_lines(col = "gray", col_alpha = .1)

# data
data_matrix <- p_nearest_dist$data %>% 
  dplyr::select(1, 2, 4) %>% 
  dplyr::mutate(pid1 = paste0("p", pid1),
                pid2 = paste0("p", pid2)) %>%
  tidyr::pivot_wider(names_from = pid2, values_from = l_length) %>% 
data_matrix

# pivot table -------------------------------------------------------------

# pivot
p_nearest_dist_pivot <- p_nearest_dist$data %>% 
  dplyr::select(-3) %>% 
  tidyr::pivot_wider(
    names_from = pid2,
    values_from = l_length,
    values_fill = 0) %>%
  dplyr::select(-1) %>% 
  as.data.frame()
rownames(p_nearest_dist_pivot) <- unique(p_nearest_dist$data$pid1)
p_nearest_dist_pivot

# renv --------------------------------------------------------------------

renv::status()
renv::snapshot()

# end ---------------------------------------------------------------------
