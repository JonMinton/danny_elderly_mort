rm(list = ls())


require(pacman)

pacman::p_load(
  WDI,
  tidyr, dplyr,
  ggplot2, 
  purrr,
  broom, 
  scales
  
)

dta <- WDI(country = "all", indicator = "NY.GDP.PCAP.CD", start = 1950, end = 2015)

dta %>% as_data_frame -> dta


dta %>% 
  rename(gdp_per_cap = NY.GDP.PCAP.CD) %>% 
  filter(country %in% c(
    "United Kingdom", "United States",
    "Canada", "France", "Germany", "Greece",
    "Norway", "Spain", "Italy"
                        )) %>% 
  ggplot(., aes(x = year, y = gdp_per_cap)) + 
  geom_line() + facet_wrap(~country)

# Now to do as before but for a number of countries

dta %>% 
  rename(gdp_per_cap = NY.GDP.PCAP.CD) %>% 
  filter(country %in% c(
    "United Kingdom", "United States",
    "Canada", "France", "Germany", "Greece",
    "Norway", "Spain", "Italy"
  ))  %>% 
  mutate(log_pcgdp = log(gdp_per_cap)) %>% 
  group_by(country) %>% 
  nest() %>% 
  mutate(preds = map(data, ~lm(log_pcgdp ~ year, data = subset(., subset = year < 2008)))) %>% 
  mutate(preds = map(preds, tidy)) %>% 
  mutate(
    intercept = map_dbl(preds, ~.[1,2]),
    trend = map_dbl(preds, ~.[2, 2])
  ) %>% 
  select(-preds) %>% 
  unnest() %>% 
  mutate(predicted_gdp_per_cap = exp(intercept + trend * year)) %>% 
  ggplot(., aes(x = year)) +
  facet_wrap(~country) +
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
  scale_y_continuous(limits = c(0, 80000), breaks = seq(5000, 80000, by = 5000), labels = comma) + 
  labs(y = "GDP per capita in £", x = "Year") + 
  theme_minimal() 


  .[1,3] %>% .[[1]]




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
