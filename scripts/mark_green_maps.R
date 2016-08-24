# Mark map script

# 23/7/2016

rm(list = ls())


require(pacman)

pacman::p_load(
  stringr, car,
  readr,  readxl,
  tidyr, dplyr,
  ggplot2, cowplot,
  maptools,
  tmap
)


# rectangular_data --------------------------------------------------------

read_csv(
  "data/mark_green/all_data_03_15.csv",
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

# Now to try the same but for three years 

pop_data %>% 
  filter(sex == "female", age_band == "80-84", year %in% c(2002, 2004, 2006)) %>% 
  select(-pop, -deaths, -ratio)  %>% 
  mutate(year = paste0("y_", year)) %>% 
  spread(year, death_rate) %>% 
  append_data(lad_shp, ., key.data = "lad2014_code", key.shp = "CODE") %>% 
  tm_shape(.) + 
  tm_polygons(col = c("y_2002", "y_2004", "y_2006"), border.col = NULL)

# Different scales for each of the facets. Will look at other way of appending

pop_data %>% 
  filter(sex == "female", age_band == "80-84", year %in% c(2002, 2004, 2006)) %>% 
  append_data(lad_shp, ., key.data = "lad2014_code", key.shp = "CODE", ignore.duplicates = T) %>% 
  tm_shape(.) + tm_facets(by = "year") +
  tm_polygons(col = "death_rate", border.col = NULL) 
# Does not work as expected. Only shows one map rather than three

# Returning to older version but with free.scales = F

# That didn't work, as argument not available 
pop_data %>% 
  filter(sex == "female", age_band == "80-84", year %in% c(2002, 2004, 2006, 2008)) %>% 
  select(-pop, -deaths, -ratio)  %>% 
  spread(year, death_rate) %>% 
  append_data(lad_shp, ., key.data = "lad2014_code", key.shp = "CODE") %>% 
  tm_shape(.) + 
  tm_polygons(
    col = c("2002", "2004", "2006", "2008"), 
    border.col = NULL, 
    style = "fixed", breaks = seq(0, 0.2, by = 0.025)
  )


# This works as proof of concept, but what's needed is..
# rate ratio 2015-2014, faceted by gender and age group

# Will look again at faceting options, otherwise produce as two separate lines 
# to be merged together 

pop_data %>% 
  filter(year == 2015) %>% 
  append_data(lad_shp, ., key.data = "lad2014_code", key.shp = "CODE", ignore.duplicates = T) %>% 
  tm_shape(.) + 
  tm_facets(by = c("sex", "age_band"), free.scales.fill = F) +
  tm_polygons(
    col = "ratio", 
    border.col = NULL
#    style = "fixed", breaks = seq(0, 0.2, by = 0.025)
  )

# Again, I can't get this approach to work as expected, so will bodge by producing 
# two separate rows, one for males and the other for females 

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
  ) + tm_facets(ncol = 3) -> chor_female



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
  ) + tm_facets(ncol = 3) -> chor_male



  

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

# Cartograms --------------------------------------------------------------


read_shape("shapefiles/2014 Total Pop cartogram/eng_wales_lads_cart.shp",
           current.projection = "longlat") -> cart_shp

pop_data %>% 
  filter(year == 2015, sex == "female") %>%
  select(-pop, -deaths, -death_rate) %>% 
  spread(age_band, ratio) %>% 
  append_data(cart_shp, ., key.data = "lad2014_code", key.shp = "CODE") %>% 
  tm_shape(.) + 
  tm_polygons(
    col = c("80-84", "85-89", "90+"), 
    border.col = NULL 
#    style = "fixed", breaks = c(0, 0.6, 0.9, 1.2, 1.5, 1.8, 2.1, 8),
#    palette = "Blues"
  ) + tm_facets(ncol = 3) -> cart_female



pop_data %>% 
  filter(year == 2015, sex == "male") %>%
  mutate(pop = pop + 0.1, deaths = deaths + 0.1) %>% #continuity correction
  select(-pop, -deaths, -death_rate) %>% 
  spread(age_band, ratio) %>% 
  append_data(cart_shp, ., key.data = "lad2014_code", key.shp = "CODE") %>% 
  tm_shape(.) + 
  tm_polygons(
    col = c("80-84", "85-89", "90+"), 
    border.col = NULL, 
    style = "fixed", breaks = c(0, 0.6, 0.9, 1.2, 1.5, 1.8, 2.1, 8),
    palette = "Blues"
  ) + tm_facets(ncol = 3) -> cart_male








