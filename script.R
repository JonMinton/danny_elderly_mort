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
  filter(year > 1996 & year < 2008) %>% 
  group_by(sex, age) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(smr ~ year, data = .))) %>% 
  select(sex, age, model) -> sam


fn <- function(DATA, MODEL){
  out <-   predict(MODEL, newdata = data.frame(year = DATA), type = "response")
  return(out)
}

double_smoothed_data %>% 
  filter(age %in% seq(50, 90, by = 5)) %>% 
  filter(year >= 2010) %>% 
  left_join(sam) %>% 
  mutate( smr_predicted  = map2_dbl(year, model, fn)) %>% 
  select(sex, year, age, smr_predicted) -> predicted_on_newlabour

double_smoothed_data  %>% 
  left_join(predicted_on_newlabour) -> double_smoothed_data


double_smoothed_data %>% 
  filter(age %in% seq(50, 90, by = 5)) %>% 
  filter(year >= 1990) %>% 
  ggplot(., aes(x = year, y = smr, colour = factor(age))) + 
  geom_point() + facet_wrap(~ sex) + 
  geom_point(aes(y = smr_predicted), shape = 2) + 
  geom_vline(aes(xintercept = 1997)) + 
  geom_vline(aes(xintercept = 2010))


ggsave("figures/points_actual_predicted_on_NL.png", width = 30, height = 30, dpi = 300, units = "cm")


# Now to look at younger ages 
double_smoothed_data %>% 
  filter(age %in% seq(30, 50, by = 5)) %>% 
  filter(year > 1996 & year < 2008) %>% 
  group_by(sex, age) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(smr ~ year, data = .))) %>% 
  select(sex, age, model) -> sam


fn <- function(DATA, MODEL){
  out <-   predict(MODEL, newdata = data.frame(year = DATA), type = "response")
  return(out)
}

double_smoothed_data %>% 
  filter(age %in% seq(30, 50, by = 5)) %>% 
  filter(year >= 2010) %>% 
  left_join(sam) %>% 
  mutate( smr_predicted  = map2_dbl(year, model, fn)) %>% 
  select(sex, year, age, smr_predicted) -> predicted_on_newlabour

double_smoothed_data  %>% 
  left_join(predicted_on_newlabour) -> double_smoothed_data


double_smoothed_data %>% 
  filter(age %in% seq(30, 50, by = 5)) %>% 
  filter(year >= 1990) %>% 
  ggplot(., aes(x = year, y = smr, colour = factor(age))) + 
  geom_point() + facet_wrap(~ sex) + 
  geom_point(aes(y = smr_predicted), shape = 2) + 
  geom_vline(aes(xintercept = 1997)) + 
  geom_vline(aes(xintercept = 2010))


ggsave("figures/points_actual_predicted_on_NL_younger.png", width = 30, height = 30, dpi = 300, units = "cm")




# Now the same for 2, 5, 10, 15, 20

double_smoothed_data %>% 
  filter(age %in% c(2, 5, 10, 15, 20)) %>% 
  filter(year > 1996 & year < 2008) %>% 
  group_by(sex, age) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(smr ~ year, data = .))) %>% 
  select(sex, age, model) -> sam


fn <- function(DATA, MODEL){
  out <-   predict(MODEL, newdata = data.frame(year = DATA), type = "response")
  return(out)
}

double_smoothed_data %>% 
  filter(age %in% c(2, 5, 10, 15, 20)) %>% 
  filter(year >= 2010) %>% 
  left_join(sam) %>% 
  mutate( smr_predicted  = map2_dbl(year, model, fn)) %>% 
  select(sex, year, age, smr_predicted) -> predicted_on_newlabour

double_smoothed_data  %>% 
  left_join(predicted_on_newlabour) -> double_smoothed_data


double_smoothed_data %>% 
  filter(age %in% c(2, 5, 10, 15, 20)) %>% 
  filter(year >= 1990) %>% 
  ggplot(., aes(x = year, y = smr, colour = factor(age))) + 
  geom_point() + facet_wrap(~ sex) + 
  geom_point(aes(y = smr_predicted), shape = 2) + 
  geom_vline(aes(xintercept = 1997)) + 
  geom_vline(aes(xintercept = 2010))


ggsave("figures/points_actual_predicted_on_NL_youngest.png", width = 30, height = 30, dpi = 300, units = "cm")



# Some analyses  ----------------------------------------------------------

# Some possible things to do 

# 1) work out how likely 2013 levels are given previous trends 

# Start with double_smoothed_data produced as before

double_smoothed_data 


## What I want: fitted on the hypothesis that the new labour trend had continued 

double_smoothed_data  %>% 
  filter(age %in% 2:90)  %>% 
  filter(year >= 1990)  %>% 
  mutate(
    newlab = year >= 1997 & year <= 2010, 
    recession = year %in% c(2007.5, 2008.5, 2009.5)
    )  %>% 
  group_by(sex, age)  %>% 
  nest()  %>% 
  mutate(
    model = map(
      data, 
      function(x) { 
        out <- lm(smr ~ year * (newlab + recession), data = x ); 
        return(out)}
    )
  )  %>% 
  mutate(
    dta2 = map(
      data, 
      function(input) {
        output <- input 
        output$newlab <- TRUE
        return(output)
        }
      ),
    dta3 = map(
      data,
      function(input) {
        output <- input
        output$newlab <- TRUE
        output$recession <- FALSE
        return(output)
      }
    )
    )  %>% 
  mutate(
    pred_nl = map2(
      model, dta2, 
      function(x, y){
        out <- predict(x, newdata = y, type = "response")
        return(out)
    }),
    pred_nl_norec = map2(
      model, dta3, 
      function(x, y){
        out <- predict(x, newdata = y, type = "response")
        return(out)
    })
    ) %>% 
  select(sex, age, data, pred_nl, pred_nl_norec) %>% 
  unnest() %>% 
  select(sex, year, age, smr, pred_nl, pred_nl_norec) -> fitted_twoscenarios 



fitted_twoscenarios %>% 
  filter(age %in% c(2, seq(5, 90, by = 5))) %>% 
  ggplot(., aes(x = year, group = factor(age), colour = factor(age))) + 
  geom_point(aes(y = smr)) + 
  geom_line(aes(y = pred_nl)) + 
  geom_line(aes(y = pred_nl_norec), linetype = "dashed") + 
  facet_wrap(~sex)

ggsave("figures/age_fitted_scenarios.png", height = 30, width = 25, units = "cm", dpi = 300)

# Cumulative, actual vs projected
dta  %>% 
  filter(year == 2014)  %>% 
  filter(place == "ew")  %>% 
  mutate(year = 2014)  %>% 
  select(sex, age, population)  %>% 
  right_join(fitted_twoscenarios) %>% 
  filter(year == max(year)) %>% 
  mutate(
    mrt_actual = population * 10^smr, 
    mrt_proj = population * 10^pred_nl
  ) %>% 
  group_by(sex) %>% 
  arrange(age) %>% 
  mutate(
    cm_mrt_actual = cumsum(mrt_actual),
    cm_mrt_proj = cumsum(mrt_proj)
    ) %>% 
  ggplot(., aes(x =age, group = sex, shape = sex, linetype = sex)) +
  geom_point(aes(y = cm_mrt_actual)) +
  geom_line(aes(y = cm_mrt_proj)) +
  scale_x_continuous(breaks = c(0, seq(10, 90, by = 10))) + 
  theme_dark() + 
  labs(x = "Age in years", y = "Total actual and projected mortality by age")

ggsave("figures/total_actual_projected.png", height = 15, width = 15, dpi = 300, units = "cm")

dta  %>% 
  filter(year == 2014)  %>% 
  filter(place == "ew")  %>% 
  mutate(year = 2014)  %>% 
  select(sex, age, population)  %>% 
  right_join(fitted_twoscenarios) %>% 
  filter(year == max(year)) %>% 
  mutate(
    mrt_actual = population * 10^smr, 
    mrt_proj = population * 10^pred_nl
  ) %>% 
  group_by(sex) %>% 
  arrange(age) %>% 
  mutate(
    cm_mrt_actual = cumsum(mrt_actual),
    cm_mrt_proj = cumsum(mrt_proj)
  ) %>% 
  ggplot(., aes(x =age, group = sex, shape = sex, linetype = sex)) +
  geom_point(aes(y = cm_mrt_actual)) +
  geom_line(aes(y = cm_mrt_proj)) +
  scale_x_continuous(breaks = c(0, seq(10, 90, by = 10))) + 
  scale_y_log10() + 
  theme_dark() + 
  labs(x = "Age in years", y = "Total actual and projected mortality")

ggsave("figures/total_actual_projected_log.png", height = 15, width = 15, dpi = 300, units = "cm")


# Now the differences 
dta  %>% 
  filter(year == 2014)  %>% 
  filter(place == "ew")  %>% 
  select(sex, age, population)  %>% 
  right_join(fitted_twoscenarios) %>% 
  filter(year == max(year)) %>% 
  mutate(
    mrt_actual = population * 10^smr, 
    mrt_proj = population * 10^pred_nl
  ) %>% 
  group_by(sex) %>% 
  arrange(age) %>% 
  mutate(
    cm_mrt_actual = cumsum(mrt_actual),
    cm_mrt_proj = cumsum(mrt_proj)
  ) %>%
  mutate(dif = cm_mrt_actual - cm_mrt_proj) %>%  
  ggplot(., aes(x =age, group = sex, shape = sex, linetype = sex)) +
  geom_line(aes(y = dif)) +
  scale_x_continuous(breaks = c(0, seq(10, 90, by = 10))) + 
  scale_y_continuous(breaks = seq(-1500, 8000, by = 500)) + 
  theme_dark() + 
  labs(x = "Age in years", y = "Cumulative excess deaths by age")

ggsave("figures/total_excess_deaths_in_2014.png", height = 15, width = 15, dpi = 300, units = "cm")

dta  %>% 
  filter(year == 2013)  %>% 
  filter(place == "ew")  %>% 
  select(sex, age, population)  %>% 
  right_join(fitted_twoscenarios) %>% 
  filter(year == 2012.5) %>% 
  mutate(
    mrt_actual = population * 10^smr, 
    mrt_proj = population * 10^pred_nl
  ) %>% 
  group_by(sex) %>% 
  arrange(age) %>% 
  mutate(
    cm_mrt_actual = cumsum(mrt_actual),
    cm_mrt_proj = cumsum(mrt_proj)
  ) %>%
  mutate(dif = cm_mrt_actual - cm_mrt_proj) %>%  
  ggplot(., aes(x =age, group = sex, shape = sex, linetype = sex)) +
  geom_line(aes(y = dif)) +
  scale_x_continuous(breaks = c(0, seq(10, 90, by = 10))) + 
  scale_y_continuous(breaks = seq(-1500, 8000, by = 500)) + 
  theme_dark() + 
  labs(x = "Age in years", y = "Cumulative excess deaths by age")

ggsave("figures/total_excess_deaths_in_2013.png", height = 15, width = 15, dpi = 300, units = "cm")



dta  %>% 
  filter(year == 2012)  %>% 
  filter(place == "ew")  %>% 
  select(sex, age, population)  %>% 
  right_join(fitted_twoscenarios) %>% 
  filter(year == 2011.5) %>% 
  mutate(
    mrt_actual = population * 10^smr, 
    mrt_proj = population * 10^pred_nl
  ) %>% 
  group_by(sex) %>% 
  arrange(age) %>% 
  mutate(
    cm_mrt_actual = cumsum(mrt_actual),
    cm_mrt_proj = cumsum(mrt_proj)
  ) %>%
  mutate(dif = cm_mrt_actual - cm_mrt_proj) %>%  
  ggplot(., aes(x =age, group = sex, shape = sex, linetype = sex)) +
  geom_line(aes(y = dif)) +
  scale_x_continuous(breaks = c(0, seq(10, 90, by = 10))) + 
  scale_y_continuous(breaks = seq(-4000, 8000, by = 500)) + 
  theme_dark() + 
  labs(x = "Age in years", y = "Cumulative excess deaths by age")

ggsave("figures/total_excess_deaths_in_2012.png", height = 15, width = 15, dpi = 300, units = "cm")


dta  %>% 
  filter(year == 2011)  %>% 
  filter(place == "ew")  %>% 
  select(sex, age, population)  %>% 
  right_join(fitted_twoscenarios) %>% 
  filter(year == 2010.5) %>% 
  mutate(
    mrt_actual = population * 10^smr, 
    mrt_proj = population * 10^pred_nl
  ) %>% 
  group_by(sex) %>% 
  arrange(age) %>% 
  mutate(
    cm_mrt_actual = cumsum(mrt_actual),
    cm_mrt_proj = cumsum(mrt_proj)
  ) %>%
  mutate(dif = cm_mrt_actual - cm_mrt_proj) %>%  
  ggplot(., aes(x =age, group = sex, shape = sex, linetype = sex)) +
  geom_line(aes(y = dif)) +
  scale_x_continuous(breaks = c(0, seq(10, 90, by = 10))) + 
  scale_y_continuous(breaks = seq(-6000, 8000, by = 500)) + 
  theme_dark() + 
  labs(x = "Age in years", y = "Cumulative excess deaths by age")

ggsave("figures/total_excess_deaths_in_2011.png", height = 15, width = 15, dpi = 300, units = "cm")



  

# What about the coefficients?

double_smoothed_data  %>% 
  filter(age %in% 2:90)  %>% 
  filter(year >= 1990)  %>% 
  mutate(
    newlab = year >= 1997 & year <= 2010, 
    recession = year %in% c(2007.5, 2008.5, 2009.5)
  )  %>% 
  group_by(sex, age)  %>% 
  nest()  %>% 
  mutate(
    model = map(
      data, 
      function(x) { 
        out <- lm(smr ~ year * (newlab + recession), data = x ); 
        return(out)}
    )
  )  %>% 
  mutate(m2 = map(model, tidy))  %>% 
  select(-data, -model)   %>% 
  unnest()  %>% 
  mutate(est_lwr = estimate - 2 * std.error, est_upr = estimate + 2 * std.error)  %>% 
  ggplot(., aes(x = age)) + 
  facet_grid(term ~ sex, scales = "free_y") + 
  geom_ribbon(aes(ymin = est_lwr, ymax = est_upr), fill = "lightgrey") + 
  geom_line(aes(y = estimate)) + 
  geom_hline(yintercept = 0)


ggsave("figures/coefficients_with_age.png", height =30, width = 20, dpi = 300, units = "cm")



# Produce different variants of the above  --------------------------------


# Variants to produce 
# 1) ONS only - unsmoothed 
# 2) HMD only - unsmoothed 

# The following code, using the ONS data, shows how the R-squared of models 
# regressing log mortality rate against year varies with age.
# It clearly shows how the log-linear decrease assumption is most good 
# for trends in infancy and least good for trends for males in early adulthood, 
# especially arout age 27
# This is from year 1961 onwards 



dta %>% 
  filter(place == "ew") %>% 
  select(-place) %>% 
  mutate(
    year = as.integer(year), 
    mr = (0.5 + deaths) / (0.5 + population), 
    lmr = log(mr, 10)
    ) %>% 
  group_by(sex, age) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(lmr ~ year, data = .))) %>% 
  mutate(m2 = map(model, glance)) %>% 
  select(-data, -model) %>% 
  unnest() %>% 
  select(sex, age, r.squared) %>% 
  filter(age <=95) %>% 
  qplot(data = ., x = age, y = r.squared, group = sex, colour = sex, geom = c("line", "point")) 

# Now to do the same from 1990 onwards 

dta %>% 
  filter(place == "ew") %>% 
  select(-place) %>% 
  mutate(
    year = as.integer(year), 
    mr = (0.5 + deaths) / (0.5 + population), 
    lmr = log(mr, 10)
  ) %>% 
  filter(year >=1990) %>% 
  group_by(sex, age) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(lmr ~ year, data = .))) %>% 
  mutate(m2 = map(model, glance)) %>% 
  select(-data, -model) %>% 
  unnest() %>% 
  select(sex, age, r.squared) %>% 
  filter(age <=95) %>% 
  qplot(data = ., x = age, y = r.squared, group = sex, colour = sex, geom = c("line", "point")) 

# From 1990 onwards the pattern is strongest from around age 55 to around age 80.
# The correlations are weaker for both males and females 
