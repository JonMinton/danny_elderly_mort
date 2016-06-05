#install.packages("pacman")


require(pacman)
pacman::p_load(readr, readxl, tidyr, dplyr, ggplot2)

location <- "workbook/MF_ewuksyoadeathspopdata_tcm77-427952.xlsx"


uk_female_deaths <- read_excel(location, sheet = "UK female deaths", skip = 1)
uk_female_deaths <- uk_female_deaths[,1:55]

uk_female_deaths <- uk_female_deaths %>% 
  gather(key = year, value = deaths, -age) %>% 
  mutate(sex = "female", place = "uk") %>% 
  select(sex, place, year, age, deaths)


uk_male_deaths <- read_excel(location, sheet = "UK male deaths", skip = 1)
uk_male_deaths <- uk_male_deaths[,1:55]

uk_male_deaths <- uk_male_deaths %>% 
  gather(key = year, value = deaths, -age) %>% 
  mutate(sex = "male", place = "uk") %>% 
  select(sex, place, year, age, deaths)



ew_female_deaths <- read_excel(location, sheet = "EW female deaths", skip = 1)
ew_female_deaths <- ew_female_deaths[,1:55]

ew_female_deaths <- ew_female_deaths %>% 
  gather(key = year, value = deaths, -age) %>% 
  mutate(sex = "female", place = "ew") %>% 
  select(sex, place, year, age, deaths)


ew_male_deaths <- read_excel(location, sheet = "EW male deaths", skip = 1)
ew_male_deaths <- ew_male_deaths[,1:55]

ew_male_deaths <- ew_male_deaths %>% 
  gather(key = year, value = deaths, -age) %>% 
  mutate(sex = "male", place = "ew") %>% 
  select(sex, place, year, age, deaths)


uk_female_pop <- read_excel(location, sheet = "UK female pop", skip = 1)
uk_female_pop <- uk_female_pop[,1:55]

uk_female_pop <- uk_female_pop %>% 
  gather(key = year, value = population, -age) %>% 
  mutate(sex = "female", place = "uk") %>% 
  select(sex, place, year, age, population)


uk_male_pop <- read_excel(location, sheet = "UK male pop", skip = 1)
uk_male_pop <- uk_male_pop[,1:55]

uk_male_pop <- uk_male_pop %>% 
  gather(key = year, value = population, -age) %>% 
  mutate(sex = "male", place = "uk") %>% 
  select(sex, place, year, age, population)


ew_female_pop <- read_excel(location, sheet = "EW female pops")
ew_female_pop <- ew_female_pop[,1:55]

ew_female_pop <- ew_female_pop %>% 
  gather(key = year, value = population, -age) %>% 
  mutate(sex = "female", place = "ew") %>% 
  select(sex, place, year, age, population)


ew_male_pop <- read_excel(location, sheet = "EW male pops", skip = 1)
ew_male_pop <- ew_male_pop[,1:55]

ew_male_pop <- ew_male_pop %>% 
  gather(key = year, value = population, -age) %>% 
  mutate(sex = "male", place = "ew") %>% 
  select(sex, place, year, age, population)



dta_population <- Reduce(bind_rows, list(ew_female_pop, ew_male_pop, uk_female_pop, uk_male_pop))
dta_deaths <- Reduce(bind_rows, list(ew_female_deaths, ew_male_deaths, uk_female_deaths, uk_male_deaths))

dta <- inner_join(dta_deaths, dta_population)



dta %>% 
  mutate(cmr = deaths / population, lmr = log(cmr, 10)) %>% 
  group_by(sex, place, year) %>% 
  mutate(
    lg2 = lag(lmr, 2),
    lg1 = lag(lmr, 1), 
    ld1 = lead(lmr, 1), 
    ld2 = lead(lmr, 2)  ) %>% 
  mutate(tmp = lg2 + lg1 + lmr + ld1 + ld2) %>% 
  mutate(smr = tmp / 5) %>% 
  select(sex, place, year, age, deaths, population, cmr, lmr, smr) %>% 
  ungroup %>% mutate(year = as.numeric(year)) -> smooth_dta

smooth_dta %>% 
  filter(place == "ew") %>% 
  filter(age %in% seq(50, 90, by = 5)) %>% 
  ggplot(., aes(x = year, y = smr, group = factor(age), colour = factor(age))) + 
  geom_point() + facet_wrap(~ sex) + scale_x_continuous(breaks = seq(1960, 2010, by = 10)) -> g1

g1 

ggsave("figures/points_only.png", width = 30, height=30, dpi = 300, units = "cm")



g1 + geom_vline(mapping = aes(xintercept = 2010))

ggsave("figures/points_2010line.png", width = 30, height=30, dpi = 300, units = "cm")

g1 + coord_cartesian(xlim = c(2000, 2014), ylim = c(-1.5, -0.7)) + stat_smooth()

ggsave("figures/points_zoomedin_smoother.png", width = 30, height = 30, dpi = 300, units = "cm")

g1 + stat_smooth() + geom_vline(mapping = aes(xintercept= 2010))

ggsave("figures/points_smoother.png", width = 30, height = 30, dpi = 300, units = "cm")



smooth_dta  %>% mutate(birth_year = year - age) %>% 
  filter(place == "ew") %>% 
  filter(age %in% seq(50, 90, by = 5)) %>% 
  ggplot(., aes(x = birth_year, y = smr, group = factor(age), colour = factor(age))) + 
  geom_point() + facet_wrap(~ sex) + 
  geom_vline(aes(xintercept = c(1925))) + geom_vline(aes(xintercept = c(1920)))

ggsave("figures/points_by_birth_cohort.png", width = 30, height = 30, dpi = 300, units = "cm")


smooth_dta  %>% mutate(birth_year = year - age) %>% 
  filter(place == "ew") %>% 
  filter(age %in% seq(50, 90, by = 5)) %>% 
  ggplot(., aes(x = birth_year, y = smr, group = factor(age), colour = factor(age))) + 
  geom_point() + facet_wrap(~ sex) + 
  geom_vline(aes(xintercept = c(1925))) + geom_vline(aes(xintercept = c(1920))) + 
  coord_cartesian(xlim = c(1910, 1940), ylim = c(-1.5, -0.5))

ggsave("figures/points_by_birth_cohort_zoomedin.png", width = 30, height = 30, dpi = 300, units = "cm")
