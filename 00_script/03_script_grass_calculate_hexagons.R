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

# filter
rgrass::execGRASS(cmd = "v.extract", 
                  flags = "overwrite", 
                  input = "af_lim_hex_sel", 
                  output="af_lim_hex_sel_92728", 
                  where="cat = '92728'")

# region
rgrass::execGRASS(cmd = "g.region", flags = c("a", "p"), raster = "mapbiomas_brazil_af_trinacional_2023_forest_na_patches")
rgrass::execGRASS(cmd = "g.region", flags = c("a", "p"), vector = "af_lim_hex_sel_92728")
rgrass::execGRASS(cmd = "r.mask", flags = "overwrite", vector = "af_lim_hex_sel_92728")

# raster to vector
rgrass::execGRASS(cmd = "r.to.vect", 
                  flags = "overwrite", 
                  input = "mapbiomas_brazil_af_trinacional_2023_forest_na_patches_area", 
                  output = "mapbiomas_brazil_af_trinacional_2023_forest_na_patches_area_92728", 
                  type = "area")

rgrass::execGRASS(cmd = "v.db.addcolumn", 
                  map = "mapbiomas_brazil_af_trinacional_2023_forest_na_patches_area_92728",
                  columns = "fid DOUBLE PRECISION")

rgrass::execGRASS(cmd = "v.rast.stats", 
                  map = "mapbiomas_brazil_af_trinacional_2023_forest_na_patches_area_92728",
                  raster = "mapbiomas_brazil_af_trinacional_2023_forest_na_patches",
                  column_prefix = "fid", method = "average")

rgrass::execGRASS(cmd = "v.dissolve", flags = "overwrite",
                  input = "mapbiomas_brazil_af_trinacional_2023_forest_na_patches_area_92728", 
                  output = "mapbiomas_brazil_af_trinacional_2023_forest_na_patches_area_92728",
                  column = "fid")

# export
rgrass::execGRASS(cmd = "v.out.ogr", flags = "overwrite", 
                  input = "af_lim_hex_sel_92728", 
                  output = "01_data/03_vect/hex_sel_92728.gpkg", 
                  type = "area")

rgrass::execGRASS(cmd = "v.out.ogr", 
                  flags = "overwrite", 
                  input = "mapbiomas_brazil_af_trinacional_2023_forest_na_patches_area_92728", 
                  output = "01_data/03_vect/mapbiomas_brazil_af_trinacional_2023_forest_na_patches_area_92728.gpkg", 
                  type = "area")

rgrass::execGRASS(cmd = "r.out.gdal", 
                  flags = "overwrite", 
                  input = "mapbiomas_brazil_af_trinacional_2023_forest_na_patches_area_92728",
                  output = "01_data/mapbiomas_brazil_af_trinacional_2023_forest_na_patches_area_92728.tif",
                  format = "GTiff",
                  createopt = "COMPRESS=DEFLATE,ZLEVEL=9,TILED=YES")

# end ---------------------------------------------------------------------
