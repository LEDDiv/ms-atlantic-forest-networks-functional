#' ----
#' title: atlantic forest functional network 
#' author: mauricio vancine
#' date: 12-05-2025
#' ----

# prepare r -------------------------------------------------------------

# packages
library(tidyverse)
library(rgrass)
library(sf)
library(terra)

# mapbiomas ------------------------------------------------------------

# connect
rgrass::initGRASS(gisBase = system("grass --config path", inter = TRUE),
                  gisDbase = "01_data/01_grassdb",
                  location = "newProject",
                  mapset = "PERMANENT",
                  override = TRUE)

# region
rgrass::execGRASS(cmd = "g.region", 
                  flags = c("a", "p"), 
                  res = "00:04:30",
                  vector = "af_lim")

# create hexagons
rgrass::execGRASS(cmd = "v.mkgrid", 
                  flags = c("h", "overwrite"), 
                  map = "af_lim_hex")

# select hexagons
rgrass::execGRASS(cmd = "v.select", 
                  flags = c("t", "overwrite"), 
                  ainput = "af_lim_hex", 
                  binput = "af_lim", 
                  output = "af_lim_hex_sel", 
                  operator = "overlap")

# Adiciona a coluna 'area_ha' (hectares)
rgrass::execGRASS("v.db.addtable", map = "af_lim_hex_sel")

rgrass::execGRASS(cmd = "v.db.addcolumn",
                  map = "af_lim_hex_sel",
                  columns = "area_ha double precision")

# Calcula a área dos polígonos e salva na nova coluna
rgrass::execGRASS(cmd = "v.to.db",
                  flags = "overwrite",
                  map = "af_lim_hex_sel",
                  option = "area",
                  columns = "area_ha",
                  units = "h")

rgrass::execGRASS(cmd = "v.info",
                  map = "af_lim_hex_sel",
                  flags = "t")

# export
rgrass::execGRASS(cmd = "v.out.ogr",
                  input = "af_lim_hex_sel",
                  output = "01_data/03_vect/hex_sel.gpkg")

# end ---------------------------------------------------------------------
