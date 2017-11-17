# Danny - difference in projections 

rm(list = ls())


pacman::p_load(
  tidyverse,
  readxl,
  scales,
  stringr
)


# 2014 tab

proj_2014 <- read_excel(path = "data/Spreadsheet.xlsx", sheet = '2014')
proj_2016 <- read_excel(path = "data/Spreadsheet.xlsx", sheet = '2016')

proj_2014 %>% 
  gather(key = "year", value = "deaths", `2014 - 2015`:`2113 - 2114`) %>% 
  mutate(proj_year = 2014) %>% 
  bind_rows(
    proj_2016 %>% 
      gather(key = "year", value = "deaths", `2016 - 2017`:`2115 - 2116`) %>% 
      mutate(proj_year = 2016)
  ) %>% 
  select(proj_year, sex = Sex, age = Age, year, deaths) %>% 
  mutate(age = case_when(
    age == 'Birth' ~ '-1',
    age == '105+' ~ '105',
    TRUE ~ age
  ) %>% as.numeric() ) %>% 
  mutate(
    year = str_extract(year, "^[0-9]{4}") %>% as.numeric() 
  ) -> proj_joined

proj_joined %>% 
  ggplot(aes(x = year, y = age, fill = deaths)) + 
  geom_tile() + 
  coord_equal() +
  scale_fill_gradientn(
    "Deaths", 
    colours = scales::brewer_pal(palette = "Paired")(12)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10)
  ) + 
  facet_grid(sex ~ proj_year)

ggsave("figures/ons_projections_compared.png", width = 30, height = 30, units = "cm", dpi = 300)

proj_joined %>% 
  spread(proj_year, deaths) %>% 
  mutate(difference = `2016` - `2014`) %>% 
  ggplot(aes(x = year, y = age, fill = difference)) + 
  geom_tile() + 
  coord_equal() +
  scale_fill_gradientn(
    "Difference", 
    colours = scales::brewer_pal(palette = "Paired")(12)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10)
  ) + 
  facet_wrap(~sex)

ggsave("figures/ons_projections_difference.png", width = 30, height = 15, units = "cm", dpi = 300)


proj_joined %>% 
  spread(proj_year, deaths) %>% 
  mutate(difference = `2016` - `2014`) %>% 
  mutate(difference = ifelse(difference < -5000, -5000, difference)) %>% 
  ggplot(aes(x = year, y = age, fill = difference)) + 
  geom_tile() + 
  coord_equal() +
  scale_fill_gradientn(
    "Difference", 
    colours = scales::brewer_pal(palette = "Paired")(12)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10)
  ) + 
  facet_wrap(~sex)

ggsave("figures/ons_projections_difference_clipped.png", width = 30, height = 15, units = "cm", dpi = 300)



proj_joined %>% 
  spread(proj_year, deaths) %>% 
  mutate(difference = (`2016` - `2014`) / `2014`) %>% 
  ggplot(aes(x = year, y = age, fill = difference)) + 
  geom_tile() + 
  coord_equal() +
  scale_fill_gradientn(
    "Prop Diff", 
    colours = scales::brewer_pal(palette = "Paired")(12)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10)
  ) + 
  facet_wrap(~sex)

ggsave("figures/ons_projections_ratio.png", width = 30, height = 15, units = "cm", dpi = 300)


proj_joined %>% 
  spread(proj_year, deaths) %>% 
  mutate(proportion = (`2016` - `2014`) / `2014`) %>% 
  mutate(proportion = ifelse(proportion < -0.5, -0.5, proportion)) %>% 
  ggplot(aes(x = year, y = age, fill = proportion)) + 
  geom_tile() + 
  coord_equal() +
  scale_fill_gradientn(
    "Proportion", 
    colours = scales::brewer_pal(palette = "RdBu", direction = -1)(11),
    limits = c(-0.5, 0.5)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10)
  ) + 
  facet_wrap(~sex)

ggsave("figures/ons_projections_ratio_rdblu_clipped.png", width = 30, height = 15, units = "cm", dpi = 300)



