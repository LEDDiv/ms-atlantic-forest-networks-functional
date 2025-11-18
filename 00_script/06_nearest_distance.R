#' ---
#' title: nearest distance
#' author: mauricio vancine
#' date: 2025-11-18
#' ----

# prepare r ---------------------------------------------------------------

# packages
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

# end ---------------------------------------------------------------------
