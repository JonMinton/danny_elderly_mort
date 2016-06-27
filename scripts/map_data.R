# 27/ 6/ 2016

# Attempt at mapping data from components of change to look at age-specific death rates 

require(pacman)
pacman::p_load(
  readr, readxl, 
  purrr, tidyr, dplyr, 
  broom,
  ggplot2, tmap
)


# Need shapefiles 

lsoas <- read_shape("E:/Dropbox/Data/Shapefiles/england/Lower_layer_super_output_areas_(E+W)_2001_Boundaries_(Full_Clipped)_V2/LSOA_2001_EW_BFC_V2.shp")
