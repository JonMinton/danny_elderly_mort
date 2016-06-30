# 27/ 6/ 2016

# Attempt at mapping data from components of change to look at age-specific death rates 
rm(list = ls())

require(pacman)
pacman::p_load(
  readr, readxl, 
  purrr, tidyr, dplyr, 
  broom,
  ggplot2, tmap
)


# Need shapefiles 

lads <- read_shape("E:/Dropbox/Data/Shapefiles/england/England_lad_2011/england_lad_2011.shp")


ew_new_2014 <- read_excel(
"workbook/analysistoolmid2014uk/Analysis Tool mid-2014 UK.xlsx",
sheet = "Detailed Components EW"
)


# Now to just append total population sizes for a cartogram

ew_new_2014 %>% 
  .[,c(7, 33)] %>% 
  rename(code = `LA Code`, population = `Estimated Population 2014`) %>% 
  group_by(code) %>% 
  summarise(population = sum(population, na.rm = T)) -> tmp

lads_joined <- append_data(shp = lads, data = tmp, key.shp = "code", key.data = "code")
write_shape(lads_joined, "shapefiles/lad_with_pop.shp")


# To check the newly created cartogram

lad_cart <- read_shape("shapefiles/lad_with_pop_cartogram.shp", current.projection = "longlat")
