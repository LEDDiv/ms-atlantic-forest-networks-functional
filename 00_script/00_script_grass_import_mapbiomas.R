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

# import limit
# rgrass::execGRASS("v.in.ogr",
#                   flags = c("overwrite"),
#                   input = "01_data/00_raw/limit_af_wwf_terr_ecos_biorregions_2017_gcs_wgs84.shp",
#                   output = "af_lim")

# import mapbiomas brazil
# rgrass::execGRASS("r.in.gdal",
#                   flags = "overwrite",
#                   input = "01_data/00_raw/brasil_coverage_2023.tif",
#                   output = "mapbiomas_brazil_2023")

# import mapbiomas trinacional
# rgrass::execGRASS("r.in.gdal",
#                   flags = "overwrite",
#                   input = "01_data/00_raw/mapbiomas_atlantic_forest_collection4_integration_v1-classification_2023.tif",
#                   output = "mapbiomas_af_trinacional_2023")

# region and mask
rgrass::execGRASS(cmd = "g.region", flags = c("a", "p"), raster = "mapbiomas_brazil_2023,mapbiomas_af_trinacional_2023")

# zero by na
rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite", expression = "mapbiomas_brazil_2023_na = if(mapbiomas_brazil_2023 == 0, null(), mapbiomas_brazil_2023)")
rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite", expression = "mapbiomas_af_trinacional_2023_na = if(mapbiomas_af_trinacional_2023 == 0, null(), mapbiomas_af_trinacional_2023)")

# patch
rgrass::execGRASS(cmd = "r.patch",
                  flags = "overwrite",
                  input = "mapbiomas_brazil_2023_na,mapbiomas_af_trinacional_2023_na",
                  output = "mapbiomas_brazil_af_trinacional_2023",
                  nprocs = 10,
                  memory= 1000)

# region and mask
rgrass::execGRASS(cmd = "g.region", flags = c("a", "p"), vector = "af_lim")
rgrass::execGRASS(cmd = "r.mask", flags = "overwrite", vector = "af_lim")

# forest and vegetation
rgrass::execGRASS(cmd = "r.mapcalc",
                  flags = "overwrite",
                  expression = "mapbiomas_brazil_af_trinacional_2023_forest = mapbiomas_brazil_af_trinacional_2023 == 3 || mapbiomas_brazil_af_trinacional_2023 == 5 || mapbiomas_brazil_af_trinacional_2023 == 49")

rgrass::execGRASS(cmd = "r.mapcalc",
                  flags = "overwrite",
                  expression = "mapbiomas_brazil_af_trinacional_2023_natural = mapbiomas_brazil_af_trinacional_2023 == 3 || mapbiomas_brazil_af_trinacional_2023 == 5 || mapbiomas_brazil_af_trinacional_2023 == 49 || mapbiomas_brazil_af_trinacional_2023 == 4 || mapbiomas_brazil_af_trinacional_2023 == 6 || mapbiomas_brazil_af_trinacional_2023 == 11 || mapbiomas_brazil_af_trinacional_2023 == 12 || mapbiomas_brazil_af_trinacional_2023 == 50")

# export
rgrass::execGRASS(cmd = "r.out.gdal", 
                  flags = "overwrite", 
                  input = "mapbiomas_brazil_af_trinacional_2023_forest",
                  output = "01_data/mapbiomas_brazil_af_trinacional_2023_forest.tif",
                  format = "GTiff",
                  createopt = "COMPRESS=DEFLATE,ZLEVEL=9,TILED=YES")

rgrass::execGRASS(cmd = "r.out.gdal", 
                  flags = "overwrite", 
                  input = "mapbiomas_brazil_af_trinacional_2023_natural",
                  output = "01_data/mapbiomas_brazil_af_trinacional_2023_natural.tif",
                  format = "GTiff",
                  createopt = "COMPRESS=DEFLATE,ZLEVEL=9,TILED=YES")

# end ---------------------------------------------------------------------
