# Joinpoint exploration 

#10/1/2017

# The purpose of this is to (briefly) explore whether Joinpoint regression models
# identify a discontinuity in elderly mortality rates after 2012

# Analyses with ONS data only and simpler method

rm(list = ls())

require(pacman)
pacman::p_load(
  
  readr, readxl, xlsx,
  purrr, tidyr, dplyr, 
  stringr,
  broom,
  ggplot2, cowplot, scales,
  segmented
)


source("scripts/extract_combined_ons.R")

# Let's start with joinpoints at age 0, and at age 75

dta %>% 
  mutate(lmr = log(deaths/ population, 10)) %>% 
  filter(age == 0) %>% 
  filter(place == "ew") %>% 
  filter(sex == "female") -> dta_inf_fem

lm(lmr ~ year, data = dta_inf_fem) -> mdl_infant_female

mdl_infant_female_seg <- segmented(mdl_infant_female, seg.Z = ~year, psi = 1979)


dta %>% 
  mutate(lmr = log(deaths/ population, 10)) %>% 
  filter(age == 0) %>% 
  filter(place == "ew") %>% 
  filter(sex == "male") -> dta_inf_male

lm(lmr ~ year, data = dta_inf_male) -> mdl_infant_male

mdl_infant_male_seg <- segmented(mdl_infant_male, seg.Z = ~year, psi = 1979)

summary(mdl_infant_female_seg)
summary(mdl_infant_male_seg)

# Joinpoint suggests year 1995 for females and 1996 for males 
# To allow two joinpoints specify two values for psi

mdl_infant_female_seg_2 <- segmented(mdl_infant_female, seg.Z = ~year, psi = c(1970, 1985))
mdl_infant_male_seg_2 <- segmented(mdl_infant_male, seg.Z = ~year, psi = c(1970, 1985))

# When two joinpoints are specified the values are 
# males 1974 and 1994
# females 1974 and 1993


# Let's now do the same for age 75

dta %>% 
  mutate(lmr = log(deaths/ population, 10)) %>% 
  filter(age == 75) %>% 
  filter(place == "ew") %>% 
  filter(sex == "male") -> dta_75_male

dta %>% 
  mutate(lmr = log(deaths/ population, 10)) %>% 
  filter(age == 75) %>% 
  filter(place == "ew") %>% 
  filter(sex == "female") -> dta_75_female


lm(lmr ~ year, data = dta_75_male) -> mdl_75_male
lm(lmr ~ year, data = dta_75_female) -> mdl_75_female

mdl_75_male_seg <- segmented(mdl_75_male, seg.Z = ~year, psi = 1979)
mdl_75_female_seg <- segmented(mdl_75_female, seg.Z = ~year, psi = 1979)

# Break points are 
# male 1987
# female 2000

mdl_75_male_seg_2 <- segmented(mdl_75_male, seg.Z = ~year, psi = c(1970, 1990))
mdl_75_female_seg_2 <- segmented(mdl_75_female, seg.Z = ~year, psi = c(1970, 1990))

# Breakpoints are 
# male 1976 1997
# female 2002 2010

mdl_75_male_seg_3 <- segmented(mdl_75_male, seg.Z = ~year, psi = c(1970, 1980, 1990))
mdl_75_female_seg_3 <- segmented(mdl_75_female, seg.Z = ~year, psi = c(1970, 1980, 1990))

#Breakpoints are : 
# male 1976 1999 2011
# female 1963 2002 2010

# > BIC(mdl_75_female_seg, mdl_75_female_seg_2, mdl_75_female_seg_3)
# df       BIC
# mdl_75_female_seg    5 -322.6323
# mdl_75_female_seg_2  7 -341.8792
# mdl_75_female_seg_3  9 -334.9468
# > BIC(mdl_75_male_seg, mdl_75_male_seg_2, mdl_75_male_seg_3)
# df       BIC
# mdl_75_male_seg    5 -270.8370
# mdl_75_male_seg_2  7 -304.0718
# mdl_75_male_seg_3  9 -324.1183

# So, model including changepoint at around 2010 is best for males but not for females. 

run_joinpoints <- function(DATA, PSI = 1988){
  mdl <- lm(lmr ~ year, data = DATA)
  mdl_seg <- segmented(mdl, seg.Z = ~ year, psi = PSI)
  
  years_picked <- mdl_seg[["psi"]][,"Est."] %>% round()
  coeffs <- coefficients(mdl_seg)
  
  out <- list(years_picked = years_picked, coeffs = coeffs)
  out
}

dta %>% 
  mutate(lmr = log(deaths/ population, 10)) %>% 
  filter(place == "ew") %>% 
  group_by(sex, age) %>% 
  nest() %>% 
  mutate(jp_out = map(data, safely(run_joinpoints))) -> mdls_1

mdls_1 %>% 
  mutate(has_error = map_lgl(jp_out, ~ !is.null(.$error))) %>% 
  filter(!has_error) %>% 
  mutate(jp_out = map(jp_out, ~ .[["result"]])) %>% 
  mutate(yr_1 = map_dbl(jp_out, ~ .[["years_picked"]])) %>% 
  ggplot(., aes(x = age, y = yr_1)) + geom_point() + facet_wrap(~ sex)

# These seem to pick out the 1918 cohort effect! 

# Let's try the same, but 2 changepoints

dta %>% 
  mutate(lmr = log(deaths/ population, 10)) %>% 
  filter(place == "ew") %>% 
  group_by(sex, age) %>% 
  nest() %>% 
  mutate(jp_out = map(data, safely(run_joinpoints), PSI = c(1979, 1997))) -> mdls_2

mdls_2 %>% 
  mutate(has_error = map_lgl(jp_out, ~ !is.null(.$error))) %>% 
  filter(!has_error) %>% 
  mutate(jp_out = map(jp_out, ~ .[["result"]])) %>% 
  mutate(yr_1 = map_dbl(jp_out, ~ .[["years_picked"]][[1]])) %>%
  mutate(yr_2 = map_dbl(jp_out, ~ .[["years_picked"]][[2]])) %>% 
  ggplot(., aes(x = age, y = yr_1)) + geom_point() + facet_wrap(~ sex)


mdls_2 %>% 
  mutate(has_error = map_lgl(jp_out, ~ !is.null(.$error))) %>% 
  filter(!has_error) %>% 
  mutate(jp_out = map(jp_out, ~ .[["result"]])) %>% 
  mutate(yr_1 = map_dbl(jp_out, ~ .[["years_picked"]][[1]])) %>%
  mutate(yr_2 = map_dbl(jp_out, ~ .[["years_picked"]][[2]])) %>% 
  ggplot(., aes(x = age, y = yr_2)) + geom_point() + facet_wrap(~ sex)

# This also picks out some cohort effects. For yr2 there's 
# also some evidence of a fixed changepoint being identified around 1995 for 
# males aged 75-90. 

dta %>% 
  mutate(lmr = log(deaths/ population, 10)) %>% 
  filter(place == "ew") %>% 
  group_by(sex, age) %>% 
  nest() %>% 
  mutate(jp_out = map(data, safely(run_joinpoints), PSI = c(1974, 1988, 2001))) -> mdls_3


mdls_3 %>% 
  mutate(has_error = map_lgl(jp_out, ~ !is.null(.$error))) %>% 
  filter(!has_error) %>% 
  mutate(jp_out = map(jp_out, ~ .[["result"]])) %>% 
  mutate(yr_1 = map_dbl(jp_out, ~ .[["years_picked"]][[1]])) %>%
  mutate(yr_2 = map_dbl(jp_out, ~ .[["years_picked"]][[2]])) %>% 
  mutate(yr_3 = map_dbl(jp_out, ~ .[["years_picked"]][[3]])) %>% 
  ggplot(., aes(x = age, y = yr_1)) + geom_point() + facet_wrap(~ sex)


mdls_3 %>% 
  mutate(has_error = map_lgl(jp_out, ~ !is.null(.$error))) %>% 
  filter(!has_error) %>% 
  mutate(jp_out = map(jp_out, ~ .[["result"]])) %>% 
  mutate(yr_1 = map_dbl(jp_out, ~ .[["years_picked"]][[1]])) %>%
  mutate(yr_2 = map_dbl(jp_out, ~ .[["years_picked"]][[2]])) %>% 
  mutate(yr_3 = map_dbl(jp_out, ~ .[["years_picked"]][[3]])) %>% 
  ggplot(., aes(x = age, y = yr_2)) + geom_point() + facet_wrap(~ sex)

mdls_3 %>% 
  mutate(has_error = map_lgl(jp_out, ~ !is.null(.$error))) %>% 
  filter(!has_error) %>% 
  mutate(jp_out = map(jp_out, ~ .[["result"]])) %>% 
  mutate(yr_1 = map_dbl(jp_out, ~ .[["years_picked"]][[1]])) %>%
  mutate(yr_2 = map_dbl(jp_out, ~ .[["years_picked"]][[2]])) %>% 
  mutate(yr_3 = map_dbl(jp_out, ~ .[["years_picked"]][[3]])) %>% 
  ggplot(., aes(x = age, y = yr_3)) + geom_point() + facet_wrap(~ sex) + 
  stat_smooth()

# Results seem equivocal. The main finding
# seems to be the indication of cohort effects found in the 1 change 
# models. Other than that there's not a strong indication of change 
# after around 2010, but there may be too few new observations to tell





