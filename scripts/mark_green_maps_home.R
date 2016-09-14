# Mark map script

# 23/7/2016

rm(list = ls())


require(pacman)

pacman::p_load(
  stringr, car,
  readr,  readxl,
  tidyr, dplyr,
<<<<<<< HEAD
  ggplot2, cowplot,
  maptools,
=======
  ggplot2, 
  rgeos, sp, maptools,
>>>>>>> 600c65c26ff8b53283003c7cce9e242af052cc68
  tmap
)


# rectangular_data --------------------------------------------------------

read_csv(
  "data/mark_green/all_data_03_15.csv"
) %>% 
  .[,-1] %>% 
  gather(key = "tmp", value = "amount", -c(1:3)) %>% 
  separate(tmp, c("variable", "year")) %>% 
  mutate(sex = recode(sex, "1 = 'male'; 2 = 'female'; else = NA")) %>% 
  mutate(year = as.numeric(year) + 2000) %>% 
  spread(variable, amount) %>% 
  select(lad2014_code, sex, age_band, year, pop, deaths) -> pop_data


# produce derived data 
pop_data  %>% 
  mutate(deaths = deaths + 0.1, pop = pop + 0.1) %>% # Continuity correction
  mutate(death_rate = deaths / pop) %>% 
  group_by(lad2014_code, sex, age_band) %>% 
  arrange(year) %>% 
  mutate(ratio = death_rate / lag(death_rate, 1)) %>% 
  ungroup() -> pop_data



# spatial data  -----------------------------------------------------------


read_shape("shapefiles/England and Wales 2011 LADs/eng_wales_lad_2011.shp") -> lad_shp



# filter and merge data  --------------------------------------------------

# First plot death rate for just one age group, one year, one age band, one sex

pop_data %>% 
  filter(sex == "female", age_band == "80-84", year == 2002) %>% 
  append_data(lad_shp, ., key.data = "lad2014_code", key.shp = "CODE") %>% 
  tm_shape(.) + 
  tm_polygons(col = "death_rate", border.col = NULL)


# Example of adding border to each region

region_lad_lookup <- read_excel(
  path = "workbook/analysistoolmid2014uk/Analysis Tool mid-2014 UK.xlsx",
  sheet = "Detailed Components EW"
) %>% 
  .[c(4, 7)] %>% 
  select(
    region = `Region Name`, la_code = `LA Code`
  ) 

append_data(
  shp = lad_shp, data = region_lad_lookup,
  key.shp = "CODE", key.data = "la_code",
  ignore.duplicates = T
              ) %>% 
  unionSpatialPolygons(., IDs = .$region) -> region_shp

# Note unionSpatialPolygons returns a SpatialPolygons object not a spatialpolygons DF
# to convert back
#http://gis.stackexchange.com/questions/61633/r-convert-a-spatial-polygon-objet-to-spatial-polygon-data-frame
tmp_df <- data.frame(id = getSpPPolygonsIDSlots(region_shp))
row.names(tmp_df) <- getSpPPolygonsIDSlots(region_shp)

region_shp <- SpatialPolygonsDataFrame(region_shp, data = tmp_df)
rm(tmp_df)

pop_data %>% 
  filter(sex == "female", age_band == "80-84", year == 2002) %>% 
  append_data(lad_shp, ., key.data = "lad2014_code", key.shp = "CODE") %>% 
  tm_shape(.) + 
  tm_polygons(col = "death_rate", border.col = NULL) + 
  tm_shape(region_shp) + # This now works with the region_level shapefile
  tm_borders() # this adds the border of the polygons

# Now if I want labels
# pop_data %>% 
#   filter(sex == "female", age_band == "80-84", year == 2002) %>% 
#   append_data(lad_shp, ., key.data = "lad2014_code", key.shp = "CODE") %>% 
#   tm_shape(.) + 
#   tm_polygons(col = "death_rate", border.col = NULL) + 
#   tm_shape(region_shp) + 
#   tm_text("id", shadow = T)


# 
# # Now to try the same but for three years 
# 
# pop_data %>% 
#   filter(sex == "female", age_band == "80-84", year %in% c(2002, 2004, 2006)) %>% 
#   select(-pop, -deaths, -ratio)  %>% 
#   mutate(year = paste0("y_", year)) %>% 
#   spread(year, death_rate) %>% 
#   append_data(lad_shp, ., key.data = "lad2014_code", key.shp = "CODE") %>% 
#   tm_shape(.) + 
#   tm_polygons(col = c("y_2002", "y_2004", "y_2006"), border.col = NULL)
# 
# # Different scales for each of the facets. Will look at other way of appending
# 
# pop_data %>% 
#   filter(sex == "female", age_band == "80-84", year %in% c(2002, 2004, 2006)) %>% 
#   append_data(lad_shp, ., key.data = "lad2014_code", key.shp = "CODE", ignore.duplicates = T) %>% 
#   tm_shape(.) + tm_facets(by = "year") +
#   tm_polygons(col = "death_rate", border.col = NULL) 
# # Does not work as expected. Only shows one map rather than three
# 
# # Returning to older version but with free.scales = F
# 
# # That didn't work, as argument not available 
# pop_data %>% 
#   filter(sex == "female", age_band == "80-84", year %in% c(2002, 2004, 2006, 2008)) %>% 
#   select(-pop, -deaths, -ratio)  %>% 
#   spread(year, death_rate) %>% 
#   append_data(lad_shp, ., key.data = "lad2014_code", key.shp = "CODE") %>% 
#   tm_shape(.) + 
#   tm_polygons(
#     col = c("2002", "2004", "2006", "2008"), 
#     border.col = NULL, 
#     style = "fixed", breaks = seq(0, 0.2, by = 0.025)
#   )


# This works as proof of concept, but what's needed is..
# rate ratio 2015-2014, faceted by gender and age group

# Will look again at faceting options, otherwise produce as two separate lines 
# # to be merged together 
# 
# pop_data %>% 
#   filter(year == 2015) %>% 
#   append_data(lad_shp, ., key.data = "lad2014_code", key.shp = "CODE", ignore.duplicates = T) %>% 
#   tm_shape(.) + 
#   tm_facets(by = c("sex", "age_band"), free.scales.fill = F) +
#   tm_polygons(
#     col = "ratio", 
#     border.col = NULL
# #    style = "fixed", breaks = seq(0, 0.2, by = 0.025)
#   )
# 
# # Again, I can't get this approach to work as expected, so will bodge by producing 
# # two separate rows, one for males and the other for females 

pop_data %>% 
  filter(year == 2015, sex == "female") %>%
  select(-pop, -deaths, -death_rate) %>% 
  spread(age_band, ratio) %>% 
  append_data(lad_shp, ., key.data = "lad2014_code", key.shp = "CODE") %>% 
  tm_shape(.) + 
  tm_polygons(
    col = c("80-84", "85-89", "90+"), 
    border.col = NULL, 
    style = "fixed", breaks = c(0, 0.6, 0.9, 1.2, 1.5, 1.8, 2.1, 8),
    palette = "Blues"
  ) + tm_facets(ncol = 3) +
  tm_shape(region_shp) + # This now works with the region_level shapefile
  tm_borders() -> chor_female

save_tmap(chor_female,
          filename = "maps/females_ratio.png", 
          width = 30, height = 10, units = "cm", dpi = 300
          )



pop_data %>% 
  filter(year == 2015, sex == "male") %>%
  select(-pop, -deaths, -death_rate) %>% 
  spread(age_band, ratio) %>% 
  append_data(lad_shp, ., key.data = "lad2014_code", key.shp = "CODE") %>% 
  tm_shape(.) + 
  tm_polygons(
    col = c("80-84", "85-89", "90+"), 
    border.col = NULL, 
    style = "fixed", breaks = c(0, 0.6, 0.9, 1.2, 1.5, 1.8, 2.1, 8),
    palette = "Blues"
  ) + 
  tm_facets(ncol = 3) + 
  tm_shape(region_shp) + # This now works with the region_level shapefile
  tm_borders() -> chor_male

save_tmap(chor_male,
          filename = "maps/males_ratio.png", 
          width = 30, height = 10, units = "cm", dpi = 300
)

  

# Redo cartograms as projection is very weird 
# Getting data from detailed components within analysis tool mid 2014 uk

# components <- read_excel(
#   path = "workbook/analysistoolmid2014uk/Analysis Tool mid-2014 UK.xlsx", 
#   sheet = "Detailed Components EW"
# )
components <- read_csv("workbook/analysistoolmid2014uk/detailed_components_ew.csv")

# From this want the country ode and Estimated Population Pyramid 2014
# 
components %>%
  .[,c(7, 33)] %>%
  rename(code = `LA Code`, population = `Estimated Population 2014`) %>%
  group_by(code) %>%
  summarise(population = sum(population, na.rm = T)) %>% 
  append_data(lad_shp, ., key.shp = "CODE", key.data = "code") -> shp_pop_merged
# 
# write_shape(shp_pop_merged, file = "shapefiles/population_2014")
# # First save these 

read_shape("shapefiles/2014_recaculated_cartogram/recaculated_2014_population_cartogram.shp",
           current.projection = "longlat") -> new_cart


# Alternative approach : using ggplot2 and fortify

new_cart_fort <- fortify(new_cart, region = "CODE")

ggplot(new_cart_fort, aes(x = long, y = lat, group = id)) + 
  geom_line(col = NA, alpha = 0.8)

# Using example from Winston Chang book

theme_clean <- function(base_size = 12) {
  require(grid)
  theme_grey(base_size) %+replace%
    theme(
      axis.title  = element_blank(),
      axis.text = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.ticks.length = unit(0, "cm"),
      axis.ticks.margin = unit(0, "cm"),
      panel.margin = unit(0, "lines"),
      plot.margin = unit(c(0, 0, 0, 0), "lines"),
      complete = TRUE
    )
}
      
new_cart_fort  %>% ggplot(., aes(x = long, y = lat, group = id)) + 
  geom_path() + theme_clean()

ggsave(filename = "maps/cartogram_border.png", height = 15, width = 12, units = "cm", dpi = 300)


# To do a choropleth
pop_data %>% 
  filter(year == 2015, sex == "male") %>% # vary sex
  filter(age_band == "85-89") %>% # vary age band
  select(-pop, -deaths, -death_rate) %>% 
  right_join(new_cart_fort, ., by = c("id" = "lad2014_code" )) %>% tbl_df %>%  
  ggplot(., aes(x = long, y = lat, group = id, fill = ratio)) + 
  geom_polygon() + theme_clean()

ggsave(filename = "maps/cartogram_choropleth.png", height = 15, width = 12, units = "cm", dpi = 300)

