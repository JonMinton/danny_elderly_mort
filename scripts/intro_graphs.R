# Some macroeconomic stats 

# Want GDP per capita

require(pacman)

pacman::p_load(
  readr, readxl, xlsx,
  purrr, tidyr, dplyr, 
  broom,
  ggplot2, cowplot, scales
)


# GDP per capita, quarterly

gdp_data <- read_csv("data/data1550604577416261464.csv", skip = 9, col_names = c("year", "gdp"))
gdp_data <- gdp_data %>% 
  slice(1:68) %>%  # only annual data 
  mutate(year = as.integer(year))
  
# Now UK capita, using HMD as data go back further

pop_data <- read_csv("hmd/counts.csv")
pop_data %>% 
  filter(country == "GBR_NP") %>% 
  select(-country) %>%
  filter(sex != "total") %>% 
  mutate(year = as.integer(year)) %>% 
  group_by(year) %>% 
  summarise(population = sum(population_count)) %>%
  filter(year >= 1950) -> hmd_pop

# UK population for most recent years 
pop_data_ons <- read_csv("data/data4793381934825294105.csv", skip = 9, col_names = c("year", "population"))

pop_data_ons %>% 
  filter(year > 2011) %>% 
  bind_rows(hmd_pop) %>% 
  arrange(year) -> pop_data_merged

  
pop_data_merged %>% 
  inner_join(gdp_data) %>% 
  mutate(gdp_per_cap = 10 ^ 6 * (gdp / population)) -> gdp_per_cap

gdp_per_cap %>% # gdp in millions
  qplot(x = year, y = gdp_per_cap, data = .)

gdp_per_cap %>% 
  mutate(log_pcgdp = log(gdp_per_cap)) %>% 
  filter(year < 2008) %>% 
  lm(log_pcgdp ~ year, data = .) %>% 
  tidy() -> coeffs

gdp_per_cap %>% 
  mutate(predicted_gdp_per_cap = exp(coeffs[1,2] + coeffs[2, 2] * year)) %>% 
  ggplot(., aes(x = year)) + 
  geom_point(aes(y = gdp_per_cap)) + 
  geom_line(aes(y = predicted_gdp_per_cap), linetype = "dashed") + 
  geom_ribbon(
    aes(
      ymin = ifelse(gdp_per_cap < predicted_gdp_per_cap, gdp_per_cap, predicted_gdp_per_cap),
      ymax = ifelse(gdp_per_cap > predicted_gdp_per_cap, gdp_per_cap, predicted_gdp_per_cap)
      ),
    fill = "lightgrey", alpha = 0.4, colour = NA
    ) + 
  scale_x_continuous(limits = c(1950, 2015), breaks = seq(1950, 2010, by = 10)) + 
  scale_y_continuous(limits = c(0, 35000), breaks = seq(5000, 35000, by = 5000), labels = comma) + 
  labs(y = "GDP per capita in £", x = "Year") + 
  theme_minimal() -> long_term_gdp_trend

# Something similar for long-term trend in E0, e5, e50 and e65


#Let's do this properly using the algorithm 

ex <- function(am, age, death_count, lower = 0) {
  sum(am[age >= lower]) / sum(death_count[age >= lower])
}

# Need to combine with UK part of ONS data 
source("scripts/extract_combined_ons.R")

dta_ons <- dta

ons_part <- dta_ons %>% 
  filter(place == "uk") %>% 
  filter(!is.na(age)) %>% 
  filter(year > 2011) %>% 
  select(year, age, sex, death_count = deaths, population_count = population)

  

hmd_part <- pop_data  %>% 
  filter(country == "GBR_NP")  %>% 
  filter(sex != "total") %>% 
  select(year, age, sex, death_count, population_count)

both_parts <- bind_rows(hmd_part, ons_part) %>% arrange(year, sex, age)

both_parts %>% 
  mutate(am = age * death_count) %>% 
  group_by(year, sex) %>% 
  summarise(
    e0 = ex(am, age, death_count, 0),
    e5 = ex(am, age, death_count, 5),
    e50 = ex(am, age, death_count, 50),
    e65 = ex(am, age, death_count, 65),
    e80 = ex(am, age, death_count, 80)
  ) %>% 
  ungroup()  -> ex_data

ex_data %>% 
  filter(year >= 1990) %>% 
  select(year, sex, e65) %>% 
  group_by(sex) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(e65 ~ year, data = subset(., year < 2008)))) %>%
  mutate(coefficients = map(model, coefficients)) %>% 
  mutate(
    intercept = map_dbl(coefficients, ~.[[1]]),
    trend = map_dbl(coefficients, ~ .[[2]])
    ) %>% 
  select(sex, data, intercept, trend) %>% 
  unnest() %>% 
  mutate(projected_e65 = intercept + year * trend) %>% 
  select(sex, year, e65, projected_e65) -> e65_actualproj

e65_actualproj %>% 
  ggplot(., aes(x = year, group = sex, shape = sex)) + 
  geom_point(aes(y = e65)) + 
  geom_line(aes(y = projected_e65), linetype = "dashed") +  
  geom_ribbon(
    aes(
      ymin = ifelse(projected_e65 < e65, e65, projected_e65),
      ymax = ifelse(projected_e65 > e65, e65, projected_e65)
    ),
    fill = "lightgrey", alpha = 0.4, colour = NA
  ) + 
  scale_x_continuous(limits = c(1990, 2015), breaks = seq(1990, 2015, by = 5)) + 
  scale_y_continuous(limits = c(75, 85), breaks = seq(75, 85, by = 0.5)) + 
  theme_minimal() + 
  theme(legend.position = c(0.8,0.2)) +
  labs(y = "Mean age of deaths for over 65s", x = "Year") -> recent_e65



dta   %>% 
  filter(place == "uk")  %>% 
  filter(age >= 35, age <= 89)  %>% 
  mutate(mr = deaths / population)  %>% 
  group_by(sex, year)  %>% 
  arrange(age)  %>% 
  mutate(lg_mr = lag(mr))  %>% 
  mutate(pc = mr / lg_mr)  %>% 
  summarise(mean_pc = mean(pc, na.rm = T))  %>% 
  ungroup()  %>%  
  mutate(mean_pc = 100 * (mean_pc - 1))  %>%  
  ggplot(., aes(x = year, y = mean_pc, group = sex, shape = sex)) + 
  geom_point() + 
  stat_smooth(se = F, colour = "black", aes(linetype = sex)) + 
  theme_minimal() +  
  scale_x_continuous(limits = c(1960, 2015), breaks = seq(1960, 2010, by = 5)) + 
  scale_y_continuous(limits = c(9, 11.5), breaks = seq(9, 11.5, .5)) + 
  labs(y = "Average % increase in risk of death per year of age", x = "Year") + 
  theme(legend.position = c(0.2, 0.2)) -> trend_in_pc_increase

dta   %>% 
  filter(place == "ew")  %>% 
  filter(age >= 35, age <= 89)  %>% 
  mutate(mr = deaths / population)  %>% 
  group_by(sex, year)  %>% 
  arrange(age)  %>% 
  mutate(lg_mr = lag(mr))  %>% 
  mutate(pc = mr / lg_mr)  %>% 
  summarise(mean_pc = mean(pc, na.rm = T))  %>% 
  ungroup()  %>%  
  mutate(mean_pc = 100 * (mean_pc - 1))  %>%  
  ggplot(., aes(x = year, y = mean_pc, group = sex, shape = sex)) + 
  geom_point() + 
  stat_smooth(se = F, colour = "black", aes(linetype = sex)) + 
  theme_minimal() +  
  scale_x_continuous(limits = c(1960, 2015), breaks = seq(1960, 2010, by = 5)) + 
  scale_y_continuous(limits = c(9, 11.5), breaks = seq(9, 11.5, .5)) + 
  labs(y = "Average % increase in risk of death per year of age", x = "Year") + 
  theme(legend.position = c(0.2, 0.2))










  



  gather(key = metric, value = value, 3:7)  %>% 
  filter(year >= 1990) %>%
  filter(metric == "e65") %>% 
  ggplot(., 
         aes(x = year, group = sex, colour = sex, y = value)) + 
  geom_point() 
  





  


