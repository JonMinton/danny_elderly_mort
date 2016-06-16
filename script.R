#install.packages("pacman")


require(pacman)
pacman::p_load(
  readr, readxl, 
  purrr, tidyr, dplyr, 
  broom,
  ggplot2
  )

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


# augment with hmd --------------------------------------------------------

hmd <- read_csv("hmd/counts.csv")

hmd <- hmd %>% filter(country == "GBRTENW", sex != "total")

hmd %>% 
  mutate(cmr = death_count / population_count, lmr = log(cmr, 10)) %>% 
  group_by(sex, year) %>% 
  mutate(
    lg2 = lag(lmr, 2),
    lg1 = lag(lmr, 1), 
    ld1 = lead(lmr, 1), 
    ld2 = lead(lmr, 2)  ) %>% 
  mutate(tmp = lg2 + lg1 + lmr + ld1 + ld2) %>% 
  mutate(smr = tmp / 5) %>% 
  select(sex, year, age, deaths = death_count, population=population_count, cmr, lmr, smr) %>% 
  ungroup %>% mutate(year = as.numeric(year)) -> smooth_dta_hmd

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
  ungroup %>% mutate(year = as.numeric(year)) %>% filter(place == "ew") -> smooth_dta_ons

smooth_dta_ons <- smooth_dta_ons %>% mutate(source = "ons") %>% 
  select(source, sex, year, age, deaths, population, cmr, lmr, smr)

smooth_dta_hmd <- smooth_dta_hmd %>% mutate(source = "hmd") %>% 
  select(source, sex, year, age, deaths, population, cmr, lmr, smr)

smooth_dta_both <- bind_rows(smooth_dta_ons, smooth_dta_hmd)

smooth_dta_both <- smooth_dta_both  %>% 
  filter(!is.na(deaths), !is.na(population))  %>% 
  distinct()

smooth_dta_both %>% 
  mutate(source_sex = paste(source, sex, sep = "_")) %>% 
  filter(age %in% seq(50, 90, by = 5)) %>% 
  ggplot(., aes(x = year, y = lmr, group = factor(source_sex), colour = factor(age), shape = source)) + 
  geom_point() + facet_wrap(~ sex) -> g1

g1 

ggsave("figures/points_only_lmr.png", width = 40, height=30, dpi = 300, units = "cm")

smooth_dta_both %>% 
  mutate(source_sex = paste(source, sex, sep = "_")) %>% 
  filter(age %in% seq(50, 90, by = 5)) %>% 
  ggplot(., aes(x = year, y = smr, group = factor(source_sex), colour = factor(age), shape = source)) + 
  geom_point() + facet_wrap(~ sex) -> g1

g1 

ggsave("figures/points_only_smr.png", width = 40, height=30, dpi = 300, units = "cm")

g1 + coord_cartesian(xlim = c(2000, 2014), ylim = c(-1.5, -0.7)) + stat_smooth(mapping = aes(group = factor(age)))

ggsave("figures/points_zoomedin_smoother_smr.png", width = 30, height = 30, dpi = 300, units = "cm")



smooth_dta_both  %>% mutate(birth_year = year - age) %>% 
  mutate(source_sex = paste(source, sex, sep = "_")) %>% 
  filter(age %in% seq(50, 90, by = 5)) %>% 
  ggplot(., aes(x = birth_year, y = smr, group = source_sex, colour = factor(age), shape = source)) + 
  geom_point() + facet_wrap(~ sex) + 
  geom_vline(aes(xintercept = c(1925))) +
  geom_vline(aes(xintercept = c(1850))) + 
  geom_vline(aes(xintercept = c(1900))) 
  
  

ggsave("figures/points_by_birth_cohort_smr.png", width = 40, height = 30, dpi = 300, units = "cm")

smooth_dta_both  %>% mutate(birth_year = year - age) %>% 
  mutate(source_sex = paste(source, sex, sep = "_")) %>% 
  filter(age %in% seq(50, 90, by = 5)) %>% 
  ggplot(., aes(x = birth_year, y = lmr, group = source_sex, colour = factor(age), shape = source)) + 
  geom_point() + facet_wrap(~ sex) + 
  geom_vline(aes(xintercept = c(1925))) +
  geom_vline(aes(xintercept = c(1850))) + 
  geom_vline(aes(xintercept = c(1900))) 



ggsave("figures/points_by_birth_cohort_lmr.png", width = 40, height = 30, dpi = 300, units = "cm")



smooth_dta_both  %>% mutate(birth_year = year - age) %>% 
  filter(age %in% seq(50, 90, by = 5)) %>% 
  mutate(source_sex = paste(source, sex, sep = "_")) %>% 
  ggplot(., aes(x = birth_year, y = smr, group = source_sex, colour = factor(age), shape = source)) + 
  geom_point() + facet_wrap(~ sex) + 
  geom_vline(aes(xintercept = c(1925))) + geom_vline(aes(xintercept = c(1920))) + 
  coord_cartesian(xlim = c(1910, 1940), ylim = c(-1.5, -0.5))

ggsave("figures/points_by_birth_cohort_zoomedin.png", width = 30, height = 30, dpi = 300, units = "cm")




# Produce further merged/ averaged data -----------------------------------

smooth_dta_both  %>% 
  filter(source == "ons")   %>% 
  rename(ons = smr)  %>% 
  select(sex, year, age, ons) -> tmp_ons

smooth_dta_both  %>% 
  filter(source == "hmd")  %>% 
  rename(hmd = smr)  %>% 
  select(sex, year, age, hmd) -> tmp_hmd

tmp_joined <- full_join(tmp_ons, tmp_hmd)

rm(tmp_ons, tmp_hmd)

tmp_joined %>% 
  group_by(sex, age) %>% 
  arrange(year) %>% 
  mutate(lg_ons = lag(ons), lg_hmd = lag(hmd)) %>%
  ungroup() %>% 
  mutate(
    sm_smr = ifelse(
      !is.na(ons) & !is.na(lg_ons), 
      (lg_ons + ons) / 2,
      ifelse(
        !is.na(hmd) & !is.na(lg_hmd),
        (hmd + lg_hmd) / 2,
        NA
      )
    )
  ) %>% 
  mutate(year = year - 0.5) %>% 
  select(sex, year, age, smr = sm_smr) %>% 
  filter(!is.na(smr)) -> double_smoothed_data


double_smoothed_data %>% 
  filter(age %in% seq(50, 90, by = 5)) %>% 
  ggplot(., aes(x = year, y = smr, colour = factor(age))) + 
  geom_point() + facet_wrap(~ sex)

double_smoothed_data %>% 
  filter(age %in% seq(50, 90, by = 5)) %>% 
  filter(year >= 1980) %>% 
  ggplot(., aes(x = year, y = smr, colour = factor(age))) + 
  geom_point() + facet_wrap(~ sex)


double_smoothed_data  %>% 
  mutate(birth_year = year - age) %>% 
  filter(age %in% seq(50, 90, by = 5)) %>% 
  ggplot(., aes(x = birth_year, y = smr, colour = factor(age))) + 
  geom_point() + facet_wrap(~ sex) + 
  geom_vline(aes(xintercept = c(1925))) + geom_vline(aes(xintercept = c(1920))) + 
  coord_cartesian(xlim = c(1910, 1940), ylim = c(-1.5, -0.5))
    

double_smoothed_data %>% 
  filter(age %in% seq(50, 90, by = 5)) %>% 
  filter(year > 1996 & year < 2010) %>% 
  group_by(sex, age) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(smr ~ year, data = .))) %>% 
  




