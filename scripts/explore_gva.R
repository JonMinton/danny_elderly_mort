rm(list = ls())

# Want GDP per capita

require(pacman)

pacman::p_load(
  readr, readxl, xlsx,
  purrr, tidyr, dplyr, 
  broom,
  ggplot2, cowplot, scales
)

gva_i <- read_excel(path = "workbook/gvaireferencetables.xls", sheet = "Table 2", skip = 1)

gva_i %>% 
  slice(1:231) %>% 
  gather(key = "year", value = "amount", -`NUTS level`, -`NUTS code`, -`Region name`) %>% 
  mutate(year = as.integer(year)) %>% 
  mutate(year = ifelse(year == 20143, 2014, year)) %>% 
  rename(level = `NUTS level`, code = `NUTS code`, name = `Region name`) %>% 
  filter(level == "NUTS1") %>%
  ggplot(., aes(x = year, y = amount)) + geom_line() + geom_point() + facet_wrap(~name)+
  theme_bw( ) + 
  scale_y_continuous(
    limits = c(0, 50000), 
    labels = comma, 
    breaks = seq(0, 50000, by = 5000), 
    minor_breaks = seq(0, 50000, by = 1000))

# index to 1997 levels
gva_i %>% 
  slice(1:231) %>% 
  gather(key = "year", value = "amount", -`NUTS level`, -`NUTS code`, -`Region name`) %>% 
  mutate(year = as.integer(year)) %>% 
  mutate(year = ifelse(year == 20143, 2014, year)) %>% 
  rename(level = `NUTS level`, code = `NUTS code`, name = `Region name`) %>% 
  filter(level == "NUTS1") %>%
  group_by(name) %>% 
  mutate(amount = 100 * amount / amount[year == 1997]) %>% 
  ggplot(., aes(x = year, y = amount)) + 
  geom_line() + geom_point() + facet_wrap(~name)+
  theme_bw( ) 


# index to 2007 levels
gva_i %>% 
  slice(1:231) %>% 
  gather(key = "year", value = "amount", -`NUTS level`, -`NUTS code`, -`Region name`) %>% 
  mutate(year = as.integer(year)) %>% 
  mutate(year = ifelse(year == 20143, 2014, year)) %>% 
  rename(level = `NUTS level`, code = `NUTS code`, name = `Region name`) %>% 
  filter(level == "NUTS1") %>%
  group_by(name) %>% 
  mutate(amount = 100 * amount / amount[year == 2007]) %>% 
  ggplot(., aes(x = year, y = amount)) + 
  geom_line() + geom_point() + facet_wrap(~name)+
  theme_bw( ) 

