# Analyses with ONS data only and simpler method

rm(list = ls())

require(pacman)
pacman::p_load(
  
  readr, readxl, xlsx,
  purrr, tidyr, dplyr, 
  stringr,
  broom,
  ggplot2, cowplot, scales
)


source("scripts/extract_combined_ons.R")


# 5/1/2017  - revision of model

# Basic idea is to present up to four different plausible models each with different specs

# The models I have so far: 

# 1) lmr ~ year * (lab + recession)
#   lmr ~ year + year:lab + year:recession + lab + recession
# 2) lmr ~ year * (lab + recession) + I(year^2)
#   lmr ~ year + year:lab + year:recession + lab + recession + year^2
# 3) lmr ~ year * lab 
#   lmr ~ year + year:lab + lab
# 4) lmr ~ year * recession
#   lmr ~ year + year:recession + recession
# 5) lmr ~ year + lab
# 6) lmr ~ year + recession 
# 7) lmr ~ year + year^2 
# 8) lmr ~ year + year^2 + year^3
# 9) lmr ~ (year + year^2) * recession
# 10) lmr ~ (year + year^2) * lab
# 11) lmr ~ (year + year^2) + lab
# 12) lmr ~ (year + year^2 + year^3) * lab
# 13) lmr ~ (year + year^2 + year^3) + lab
# 14) lmr ~ (year + year^2 + year^3) * recession
# 15) lmr ~ (year + year^2 + year^3) + recession
# 16) lmr ~ (year + year^2 + year^3 + year^4) 
# 17) lmr ~ (year + year^2 + year^3 + year^4) * recession
# 18) lmr ~ (year + year^2 + year^3 + year^4) + recession
# 19) lmr ~ (year + year^2 + year^3 + year^4) * lab
# 20) lmr ~ (year + year^2 + year^3 + year^4) + lab





# Modelling ---------------------------------------------------------------



# Model specifications  ---------------------------------------------------



run_model_01 <- function(dta){
  lm(lmr ~ year * (lab + recession), data = dta)
}

run_model_02 <- function(dta){ 
  lm(lmr ~ year * (lab + recession) + I(year^2), data = dta)
}

run_model_03 <- function(dta){ 
  lm(lmr ~ year * lab, data = dta)
}

run_model_04 <- function(dta){ 
  lm(lmr ~ year * recession, data = dta)
}

run_model_05 <- function(dta){ 
  lm(lmr ~ year + lab, data = dta)
}


run_model_06 <- function(dta){ 
  lm(lmr ~ year + recession , data = dta)
}

run_model_07 <- function(dta){ 
  lm(lmr ~ year + I(year^2) , data = dta)
}

run_model_08 <- function(dta){ 
  lm(lmr ~ year + I(year^2) + I(year^3) , data = dta)
}

run_model_09 <- function(dta){ 
  lm(lmr ~ recession * (year + I(year^2)) , data = dta)
}

run_model_10 <- function(dta){ 
  lm(lmr ~ lab * (year + I(year^2)) , data = dta)
}

run_model_11 <- function(dta){ 
  lm(lmr ~ lab + (year + I(year^2)) , data = dta)
}

run_model_12 <- function(dta){ 
  lm(lmr ~ lab * (year + I(year^2) + I(year^3)) , data = dta)
}

run_model_13 <- function(dta){ 
  lm(lmr ~ lab + (year + I(year^2) + I(year^3)) , data = dta)
}

run_model_14 <- function(dta){ 
  lm(lmr ~ recession * (year + I(year^2) + I(year^3)) , data = dta)
}

run_model_15 <- function(dta){ 
  lm(lmr ~ recession + (year + I(year^2) + I(year^3)) , data = dta)
}

run_model_16 <- function(dta){ 
  lm(lmr ~ (year + I(year^2) + I(year^3) + I(year^4)) , data = dta)
}

run_model_17 <- function(dta){ 
  lm(lmr ~ recession * (year + I(year^2) + I(year^3) + I(year^4)) , data = dta)
}

run_model_18 <- function(dta){ 
  lm(lmr ~ recession + (year + I(year^2) + I(year^3) + I(year^4)) , data = dta)
}

run_model_19 <- function(dta){ 
  lm(lmr ~ lab * (year + I(year^2) + I(year^3) + I(year^4)) , data = dta)
}

run_model_20 <- function(dta){ 
  lm(lmr ~ lab + (year + I(year^2) + I(year^3) + I(year^4)) , data = dta)
}

run_model_21 <- function(dta){ 
  lm(lmr ~ lab * year + I(year^2) + I(year^3) + I(year^4) , data = dta)
}

run_model_22 <- function(dta){ 
  lm(lmr ~ lab + year + I(year^2) + I(year^3) + I(year^4) , data = dta)
}

run_model_23 <- function(dta){ 
  lm(lmr ~ lab * recession + year + I(year^2) + I(year^3) + I(year^4) , data = dta)
}

run_model_24 <- function(dta){ 
  lm(lmr ~ lab * recession * year + I(year^2) + I(year^3) + I(year^4) , data = dta)
}


# Other functions  --------------------------------------------------------



extract_coeffs <- function(mdl){
  mdl %>% coefficients() 
}

extract_vcov <- function(mdl){
  mdl %>% vcov()
}

sim_betas <- function(cfs, vcv, n_sim = 1000){
  MASS::mvrnorm(n = n_sim, mu = cfs, Sigma = vcv)
}

# This will only work with model 1
vectorise_predictors <- function(dta, set_nl = F){
  # The output should be a list of vectors, one for each year
  out <- list()
  for (i in 1:nrow(dta)){
    yr <- as.numeric(dta[i, "year"])
    nl <- ifelse(set_nl, 1, as.numeric(dta[i, "lab"]))
    rc <- as.numeric(dta[i, "recession"])
    this_vec <- c(1, yr, nl, rc, yr * nl, yr * rc)
    out[[i]] <- this_vec
  }
  out  
}

# This will only work with model 2

vectorise_predictors_alt <- function(dta, set_nl = F){
  # The output should be a list of vectors, one for each year
  out <- list()
  for (i in 1:nrow(dta)){
    yr <- as.numeric(dta[i, "year"])
    yr2 <- yr^2
    nl <- ifelse(set_nl, 1, as.numeric(dta[i, "lab"]))
    rc <- as.numeric(dta[i, "recession"])
    this_vec <- c(1, yr,  nl, rc, yr2, yr * nl, yr * rc)
    out[[i]] <- this_vec
  }
  out  
}

vectorise_predictors_best_model <- function(dta, set_lab = F){
  # The output should be a list of vectors, one for each year
  out <- list()
  for (i in 1:nrow(dta)){
    yr <- as.numeric(dta[i, "year"])
    yr2 <- yr^2
    yr3 <- yr^3
    yr4 <- yr^4
    lab <- ifelse(set_lab, 1, as.numeric(dta[i, "lab"]))
    this_vec <- c(1, lab, yr, yr2, yr3, yr4, lab* yr, lab*yr2, lab*yr3, lab*yr4)
    out[[i]] <- this_vec
  }
  out  
}


make_deterministic_lmrs <- function(x, B){
  map_dbl(x, function(v) v %*% B)
}

make_stochastic_lmrs <- function(x, B){
  lapply(x, function(v) {apply(B, 1, function(xx, vec=v) vec %*% xx)}) 
}

predict_deaths <- function(dta, lmrs){
  pops <- map(dta[["population"]], ~.)
  map2(pops, lmrs, .f = function(x, y) x * 10 ^ y)
}

compare_actual_w_counter <- function(dths_actual, dths_counter){
  dths_diffs <- vector("list", length = length(dths_actual))
  for (i in seq_along(dths_actual)){
    dths_diffs[[i]] <- dths_actual - dths_counter[[i]]
  }
  dths_diffs
}

compare_model_w_countermodel <- function(dths_model, dths_counter_model){
  dths_diffs <- map2(dths_model, dths_counter_model, function(x, y) x - y)
  dths_diffs
}


# Automate model production  ----------------------------------------------

# Additional sensitivity analysis should be start year, 
# and whether to include additional years as recession years 

# All models, from 1990 onwards 

dta  %>% 
  filter(age <= 89)  %>% 
  filter(year >= 1990)  %>% 
  filter(place == "ew") %>% 
  mutate(lmr = log(deaths/population, 10)) %>% 
  mutate(
    lab = year %in% c(1964, 1974:1978, 1997:2010), 
    recession = year %in% c(1961, 1973:1975, 1990:1991, 2008:2009)
  )  %>% 
  mutate(year = year - min(year)) %>% 
  group_by(sex, age)  %>% 
  nest()  %>%
  mutate(
    mdl_01 = map(data, run_model_01),
    mdl_02 = map(data, run_model_02),
    mdl_03 = map(data, run_model_03),
    mdl_04 = map(data, run_model_04),
    mdl_05 = map(data, run_model_05),
    mdl_06 = map(data, run_model_06),
    mdl_07 = map(data, run_model_07),
    mdl_08 = map(data, run_model_08),
    mdl_09 = map(data, run_model_09),
    mdl_10 = map(data, run_model_10),
    mdl_11 = map(data, run_model_11),
    mdl_12 = map(data, run_model_12),
    mdl_13 = map(data, run_model_13),
    mdl_14 = map(data, run_model_14),
    mdl_15 = map(data, run_model_15),
    mdl_16 = map(data, run_model_16),
    mdl_17 = map(data, run_model_17),
    mdl_18 = map(data, run_model_18),
    mdl_19 = map(data, run_model_19),
    mdl_20 = map(data, run_model_20),
    mdl_21 = map(data, run_model_21),
    mdl_22 = map(data, run_model_22),
    mdl_23 = map(data, run_model_23),
    mdl_24 = map(data, run_model_24)
    
  ) %>% 
  select(-data) %>% 
  gather(key = "mod_number", value = "model", mdl_01:mdl_24) %>% 
  mutate(mod_number = str_replace(mod_number, "mdl_", "")) -> all_models_1990

# All models, all available years (from 1961)
dta  %>% 
  filter(age <= 89)  %>% 
  filter(place == "ew") %>% 
  mutate(lmr = log(deaths/population, 10)) %>% 
  mutate(
    lab = year %in% c(1964, 1974:1978, 1997:2010), 
    recession = year %in% c(1961, 1973:1975, 1990:1991, 2008:2009)
  )  %>% 
  mutate(year = year - min(year)) %>% 
  group_by(sex, age)  %>% 
  nest()  %>%
  mutate(
    mdl_01 = map(data, run_model_01),
    mdl_02 = map(data, run_model_02),
    mdl_03 = map(data, run_model_03),
    mdl_04 = map(data, run_model_04),
    mdl_05 = map(data, run_model_05),
    mdl_06 = map(data, run_model_06),
    mdl_07 = map(data, run_model_07),
    mdl_08 = map(data, run_model_08),
    mdl_09 = map(data, run_model_09),
    mdl_10 = map(data, run_model_10),
    mdl_11 = map(data, run_model_11),
    mdl_12 = map(data, run_model_12),
    mdl_13 = map(data, run_model_13),
    mdl_14 = map(data, run_model_14),
    mdl_15 = map(data, run_model_15),
    mdl_16 = map(data, run_model_16),
    mdl_17 = map(data, run_model_17),
    mdl_18 = map(data, run_model_18),
    mdl_19 = map(data, run_model_19),
    mdl_20 = map(data, run_model_20),
    mdl_21 = map(data, run_model_21),
    mdl_22 = map(data, run_model_22),
    mdl_23 = map(data, run_model_23),
    mdl_24 = map(data, run_model_24)
    
  ) %>% 
  select(-data) %>% 
  gather(key = "mod_number", value = "model", mdl_01:mdl_24) %>% 
  mutate(mod_number = str_replace(mod_number, "mdl_", "")) -> all_models_1961


# AIC, BIC and Rsquared by age 

all_models_1961 %>% 
  mutate(
    aic = map_dbl(model, AIC),
    bic = map_dbl(model, BIC),
    rsq = map_dbl(model, function(x) {summary(x)$r.squared}),
    rsq_adj = map_dbl(model, function(x) {summary(x)$adj.r.squared})
    ) %>% 
  select(-model) %>% 
  mutate(start_year = 1961) %>% 
  select(mod_number, start_year, sex, age, aic:rsq_adj) -> mod_summaries_1961

all_models_1990 %>% 
  mutate(
    aic = map_dbl(model, AIC),
    bic = map_dbl(model, BIC),
    rsq = map_dbl(model, function(x) {summary(x)$r.squared}),
    rsq_adj = map_dbl(model, function(x) {summary(x)$adj.r.squared})
  ) %>% 
  select(-model) %>% 
  mutate(start_year = 1990) %>% 
  select(mod_number, start_year, sex, age, aic:rsq_adj) -> mod_summaries_1990

mod_summaries_both <- bind_rows(mod_summaries_1961, mod_summaries_1990)


# Now to explore this 

mod_summaries_both %>% 
  group_by(mod_number, start_year) %>% 
  summarise_each(funs(mean), aic:rsq_adj) %>% 
  ggplot(., aes(x = mod_number, y = aic)) + 
  geom_point() + facet_wrap(~ start_year, scales = "free_y")
# AIC suggests model 11 is best, then model 8, if looking from 1961 onwards
# If looking from 1990, model 8 then model 11.
# model 8: lmr ~ year + year^2 + year^3
# model 11: lmr ~ lab * (year + year^2 + year^3)


mod_summaries_both %>% 
  group_by(mod_number, start_year) %>% 
  summarise_each(funs(mean), aic:rsq_adj) %>% 
  ggplot(., aes(x = mod_number, y = bic)) + 
  geom_point() + facet_wrap(~ start_year, scales = "free_y")

# BIC suggests model 8 is best, then model 11, if looking from 1961 onwards
# If looking from 1990, model 8 is best, then model 7.
# model 7: lmr ~ year + year^2
# model 8: lmr ~ year + year^2 + year^3
# model 11: lmr ~ lab * (year + year^2 + year^3)

mod_summaries_both %>% 
  group_by(mod_number, start_year) %>% 
  summarise_each(funs(mean), aic:rsq_adj) %>% 
  ggplot(., aes(x = mod_number, y = rsq)) + 
  geom_point() + facet_wrap(~ start_year, scales = "free_y")

# rsq suggests model 11, then model 2 (lab:recession:year interaction + year^2)  
# if looking from 1990 onwards.
# If looking from 1961 onwards, model 11 then model 8

# model 2: lmr ~ year * (lab + recession) + I(year^2)
# model 7: lmr ~ year + year^2
# model 8: lmr ~ year + year^2 + year^3
# model 11: lmr ~ lab * (year + year^2 + year^3)

mod_summaries_both %>% 
  group_by(mod_number, start_year) %>% 
  summarise_each(funs(mean), aic:rsq_adj) %>% 
  ggplot(., aes(x = mod_number, y = rsq_adj)) + 
  geom_point() + facet_wrap(~ start_year, scales = "free_y")

# Adjusted R-squared indicates model 11 for both start dates
# then model 8 for 1961 onwards, and model 2 for 1990 onwards


# I'm now looking at model 19

mod_summaries_both %>% 
  filter(mod_number == "19") %>% 
  filter(start_year == 1961) %>% 
  ggplot(., aes(x = age, colour = sex, y = rsq_adj)) + 
  geom_point() 

mod_summaries_both %>% 
  filter(mod_number == "19") %>% 
  filter(start_year == 1961) %>% 
  ggplot(., aes(x = age, colour = sex, y = aic)) + 
  geom_point() 

mod_summaries_both %>% 
  filter(mod_number == "19") %>% 
  filter(start_year == 1961) %>% 
  ggplot(., aes(x = age, colour = sex, y = bic)) + 
  geom_point() 

# The above suggest the model fit should be focused on ages 0 and 50+ 

mod_summaries_both %>% 
  filter(age %in% c(0, 50:89)) %>% 
  group_by(mod_number, start_year) %>% 
  summarise_each(funs(mean), aic:rsq_adj) %>% 
  ggplot(., aes(x = mod_number, y = aic)) + 
  geom_point() + facet_wrap(~ start_year, scales = "free_y")

mod_summaries_both %>% 
  filter(age %in% c(0, 50:89)) %>% 
  group_by(mod_number, start_year) %>% 
  summarise_each(funs(mean), aic:rsq_adj) %>% 
  ggplot(., aes(x = mod_number, y = rsq_adj)) + 
  geom_point() + facet_wrap(~ start_year, scales = "free_y")

mod_summaries_both %>% 
  filter(age %in% c(0, 50:89)) %>% 
  group_by(mod_number, start_year) %>% 
  summarise_each(funs(mean), aic:rsq_adj) %>% 
  ggplot(., aes(x = mod_number, y = bic)) + 
  geom_point() + facet_wrap(~ start_year, scales = "free_y")




# Conclusion
# Though results are equivocal, I will go for model 19 with all years 

# Names and coefficient positions of model 19
# (Intercept)           labTRUE              year         I(year^2)         I(year^3)         I(year^4) 
# -1.728501e+00     -5.216318e-02     -2.024589e-03     -9.519294e-04      2.131065e-05     -1.446998e-07 
# labTRUE:year labTRUE:I(year^2) labTRUE:I(year^3) labTRUE:I(year^4) 
# 2.160284e-02     -1.805440e-03      5.148380e-05     -4.637523e-07 

dta  %>% 
  filter(age <= 89)  %>% 
  filter(place == "ew") %>% 
  mutate(lmr = log(deaths/population, 10)) %>% 
  mutate(
    lab = year %in% c(1964, 1974:1978, 1997:2010), 
    recession = year %in% c(1961, 1973:1975, 1990:1991, 2008:2009)
  )  %>% 
  mutate(year = year - min(year)) %>% 
  group_by(sex, age)  %>% 
  nest()  %>%
  mutate(
    best_model = map(data, run_model_19),
    coeffs  = map(best_model, extract_coeffs),
    vcv = map(best_model, extract_vcov),
    betas = map2(coeffs, vcv, sim_betas),
    prd_vec = map(data, vectorise_predictors_best_model),
    prd_vec_lab = map(data, vectorise_predictors_best_model, set_lab = T),
    det_lmr_lab = map2(prd_vec_lab, coeffs, make_deterministic_lmrs),
    sim_lmrs = map2(prd_vec, betas, make_stochastic_lmrs),
    sim_lmrs_lab = map2(prd_vec_lab, betas, make_stochastic_lmrs),
    sim_deaths = map2(data, sim_lmrs, predict_deaths),
    sim_deaths_lab = map2(data, sim_lmrs_lab, predict_deaths),
    dif_deaths_lab = map2(map(data, ~ .$deaths), sim_deaths_lab, compare_actual_w_counter),
    dif_deaths_stoch = map2(sim_deaths, sim_deaths_lab, compare_model_w_countermodel)
  ) -> best_model_sims

# Models + simulations 


# Plots -------------------------------------------------------------------




# Figure 2 - banded plots at older ages  ----------------------------------

best_model_sims  %>% 
  select(sex, age, data, det_lmr_lab)  %>% 
  unnest() -> fitted_twoscenarios

best_model_sims %>% 
  select(sex, age, data, sim_lmrs_lab) %>% 
  mutate(
    ul_lwr = map(sim_lmrs_lab, function(x) {map_dbl(x, quantile, probs = 0.025)}),
    ul_upr = map(sim_lmrs_lab, function(x) {map_dbl(x, quantile, probs = 0.975)})
  ) %>% 
  select(sex, age, data, ul_lwr, ul_upr)  %>% 
  unnest() -> mod_bands 

mod_bands <- fitted_twoscenarios  %>% 
  select(sex, age, det_lmr_lab, place, year)  %>% 
  inner_join(mod_bands) 

make_valband <- function(ages, min_year = 1997, identity_scale = F){
  
  if(identity_scale) {
    mod_bands <- mod_bands %>% 
      mutate(
        ul_lwr = 10^ul_lwr,
        ul_upr = 10^ul_upr,
        det_lmr_lab = 10^det_lmr_lab,
        lmr = 10^lmr
      )
  }
  
  mod_bands %>% 
    mutate(year = year + 1990) %>% 
    mutate(
      ul_lwr = ifelse(year < 1997, NA, ul_lwr),
      ul_upr = ifelse(year < 1997, NA, ul_upr),
      det_lmr_lab = ifelse(year < 1997, NA, det_lmr_lab)
    ) %>%
    filter(age %in% ages) %>% 
    mutate(age = factor(age)) %>% 
    ggplot(., aes(x = year, group = age)) + 
    geom_ribbon(aes(ymin = ul_lwr, ymax = ul_upr), alpha = 0.2) + 
    geom_line(aes(y = det_lmr_lab)) + 
    geom_point(aes(y = lmr, colour = age, shape = age)) + 
    facet_wrap(~sex) + 
    labs(x = "Year", y = ifelse(identity_scale, "Mortality risk at age", "Base 10 log mortality risk at age")) + 
    geom_vline(xintercept = 1997, linetype = "dashed") + 
    geom_vline(xintercept = 2010, linetype = "dashed")   
}


plot_grid(
  make_valband(seq(60, 85, by = 5)),
  make_valband(seq(60, 85, by = 5), identity_scale  = T),
  nrow = 2,
  labels = c("A", "B")
)

ggsave("figures/banded_plot_older_ages.png", height = 25, width = 25, units = "cm", dpi = 300)



# Figure S1 - banded plots at younger ages  -------------------------------

plot_grid(
  make_valband(c(0, 15, 25, 35, 45)),
  make_valband(c(0, 15, 25, 35, 45), identity_scale  = T),
  nrow = 2,
  labels = c("A", "B")
)

ggsave("figures/banded_plot_younger.png", height = 25, width = 25, units = "cm", dpi = 300)



# Figure 3 - total excess mortality up to age 89 years  -------------------


draw_diffs <- function(dta, YEAR, XLIMS = c(0, 89), YLIMS = c(-12000, 12000), return_table = F){
  dta  %>% 
    mutate(year = year + 1990)  %>% 
    filter(year == YEAR) %>% 
    mutate(deaths_counterfactual = population * 10 ^ det_lmr_lab)  %>% 
    group_by(sex)  %>% 
    arrange(age)  %>% 
    mutate(
      cm_mrt_actual = cumsum(deaths), 
      cm_mrt_counter = cumsum(deaths_counterfactual),
      dif = cm_mrt_actual - cm_mrt_counter
    ) -> diffs_estimates
  
  if (return_table) {return(diffs_estimates)}
  
  diffs_estimates %>%     
    ggplot(., aes(x =age, group = sex, shape = sex, linetype = sex)) +
    geom_line(aes(y = dif)) +
    scale_x_continuous(limits = XLIMS, breaks = c(0, seq(10, 90, by = 10))) + 
    scale_y_continuous(limits = YLIMS, breaks = seq(YLIMS[1], YLIMS[2], by = 2000), labels = comma) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 65, linetype = "dashed") + 
    theme_minimal() + 
    labs(x = "Age in years", y = "Total additional deaths by\n age on x axis", title = YEAR)
}


draw_diffs(fitted_twoscenarios, 2010, XLIMS = c(0, 89)) -> dd_2010
draw_diffs(fitted_twoscenarios, 2011, XLIMS = c(0, 89)) -> dd_2011
draw_diffs(fitted_twoscenarios, 2012, XLIMS = c(0, 89)) -> dd_2012
draw_diffs(fitted_twoscenarios, 2013, XLIMS = c(0, 89)) -> dd_2013
draw_diffs(fitted_twoscenarios, 2014, XLIMS = c(0, 89)) -> dd_2014
draw_diffs(fitted_twoscenarios, 2015, XLIMS = c(0, 89)) -> dd_2015


plot_grid(
  dd_2010, dd_2011, dd_2012,
  dd_2013, dd_2014, dd_2015, 
  nrow = 2
) -> g
print(g)

ggsave("figures/excess_deaths_2010_2015.png", height = 30, width = 40, dpi = 300, units = "cm")


# 'excess' deaths by particular ages  -------------------------------------
# 
# draw_diffs(fitted_twoscenarios, 2010, XLIMS = c(0, 89), return_table = T) -> t_2010
# draw_diffs(fitted_twoscenarios, 2011, XLIMS = c(0, 89), return_table = T) -> t_2011
# draw_diffs(fitted_twoscenarios, 2012, XLIMS = c(0, 89), return_table = T) -> t_2012
# draw_diffs(fitted_twoscenarios, 2013, XLIMS = c(0, 89), return_table = T) -> t_2013
# draw_diffs(fitted_twoscenarios, 2014, XLIMS = c(0, 89), return_table = T) -> t_2014
# draw_diffs(fitted_twoscenarios, 2015, XLIMS = c(0, 89), return_table = T) -> t_2015
# 
# t_all <- reduce(list(t_2010, t_2011, t_2012, t_2013, t_2014, t_2015), bind_rows) %>% 
#   select(sex, age, year, cm_mrt_actual, cm_mrt_counter, dif)
# 
# t_all %>% filter(
#   age %in% c(0, 15, 25, 35, 50, 60, 70, 80, 89)
# ) %>% 
#   mutate(
#     cm_mrt_counter = round(cm_mrt_counter, 0), 
#     dif = round(dif, 0)
#          ) %>% write.csv("clipboard")

# Used to produce excess mort formatted


# Coefficents for standard model specification 

  best_model_sims %>% 
  select(sex, age, best_model) %>% 
  mutate(coef_tidy = map(best_model, tidy)) %>% 
  select(-best_model) %>% 
  unnest() %>% 
  # mutate(term = car::recode(
  #   term, 
  #   "
  #   '(Intercept)' = 'Intercept';
  #   'year' = 'Trend';
  #   'labTRUE' = 'NL Intercept';
  #   'recessionTRUE' = 'GFC Intercept'
  #   "
  # )) %>% 
  # mutate(
  #   term = ifelse(term == "year:labTRUE", "NL Trend", term),
  #   term = ifelse(term == "year:recessionTRUE", "GFC Trend", term)
  # ) %>% 
  # mutate(term = factor(term, levels = c("Intercept", "Trend", "NL Intercept", "NL Trend", "GFC Intercept", "GFC Trend"))
  # ) %>% 
  mutate(upper = estimate + 1.96 * std.error, lower = estimate - 1.96 * std.error) %>% 
  ggplot(., aes(x = age)) + 
  facet_grid(term ~ sex, scales = "free_y") + 
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightgrey") + 
  geom_line(aes(y = estimate)) + 
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = c(0, seq(10, 90, by = 10))) + 
  geom_vline(xintercept = 65, linetype = "dashed")

ggsave("figures/ons_only_coefficients_with_age.png", height =30, width = 20, dpi = 300, units = "cm")




# See whether standard or additional model performs better

ggplot(model_outputs, aes(x = age, y = compare_aic)) + 
  geom_point() + 
  stat_smooth(se = T) + 
  geom_hline(aes(yintercept = 0)) + 
  facet_wrap(~sex) + 
  labs(x = "Age in years", y = "Difference in AIC\n (Positive = Nonlinear performs better)")

ggsave("figures/diff_in_aic.png", width = 15, height = 10, units = "cm", dpi = 300)
# Slight evidence alt model performs better for young adult men and older women (post 50)


# Coefficents for alternative model specification 

model_outputs %>% 
  select(sex, age, mdl_alt) %>% 
  mutate(coef_tidy = map(mdl_alt, tidy)) %>% 
  select(-mdl_alt) %>% 
  unnest() %>% 
  mutate(term = car::recode(
    term,
    "
    '(Intercept)' = 'Intercept';
    'year' = 'Trend';
    'labTRUE' = 'NL Intercept';
    'recessionTRUE' = 'GFC Intercept';
    'I(year^2)' = 'Nonlinear trend'
    "
  )) %>%
  mutate(
    term = ifelse(term == "year:labTRUE", "NL Trend", term),
    term = ifelse(term == "year:recessionTRUE", "GFC Trend", term)
  ) %>%
  mutate(term = factor(term, levels = c("Intercept", "Trend", "Nonlinear trend", "NL Intercept", "NL Trend", "GFC Intercept", "GFC Trend"))
  ) %>%
  mutate(upper = estimate + 1.96 * std.error, lower = estimate - 1.96 * std.error) %>% 
  ggplot(., aes(x = age)) + 
  facet_grid(term ~ sex, scales = "free_y") + 
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightgrey") + 
  geom_line(aes(y = estimate)) + 
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = c(0, seq(10, 90, by = 10))) + 
  geom_vline(xintercept = 65, linetype = "dashed")

ggsave("figures/ons_only_coefficients_with_age_alt_spec.png", height =35, width = 20, dpi = 300, units = "cm")


# Now produce projections using alt spec at younger and older ages 


model_outputs  %>% 
  select(sex, age, data, det_lmr_nl_alt)  %>% 
  unnest() -> fitted_twoscenarios

model_outputs %>% 
  select(sex, age, data, sim_lmrs_nl_alt) %>% 
  mutate(
    ul_lwr = map(sim_lmrs_nl_alt, function(x) {map_dbl(x, quantile, probs = 0.025)}),
    ul_upr = map(sim_lmrs_nl_alt, function(x) {map_dbl(x, quantile, probs = 0.975)})
  ) %>% 
  select(sex, age, data, ul_lwr, ul_upr)  %>% 
  unnest() -> mod_bands 

mod_bands <- fitted_twoscenarios  %>% 
  select(sex, age, det_lmr_nl_alt, place, year)  %>% 
  inner_join(mod_bands) 

make_valband <- function(ages, min_year = 1997, identity_scale = F){
  
  if(identity_scale) {
    mod_bands <- mod_bands %>% 
      mutate(
        ul_lwr = 10^ul_lwr,
        ul_upr = 10^ul_upr,
        det_lmr_nl_alt = 10^det_lmr_nl_alt,
        lmr = 10^lmr
      )
  }
  
  mod_bands %>% 
    mutate(year = year + 1990) %>% 
    mutate(
      ul_lwr = ifelse(year < 1997, NA, ul_lwr),
      ul_upr = ifelse(year < 1997, NA, ul_upr),
      det_lmr_nl_alt = ifelse(year < 1997, NA, det_lmr_nl_alt)
    ) %>%
    filter(age %in% ages) %>% 
    mutate(age = factor(age)) %>% 
    ggplot(., aes(x = year, group = age)) + 
    geom_ribbon(aes(ymin = ul_lwr, ymax = ul_upr), alpha = 0.2) + 
    geom_line(aes(y = det_lmr_nl_alt)) + 
    geom_point(aes(y = lmr, colour = age, shape = age)) + 
    facet_wrap(~sex) + 
    labs(x = "Year", y = ifelse(identity_scale, "Mortality risk at age", "Base 10 log mortality risk at age")) + 
    geom_vline(xintercept = 1997, linetype = "dashed") + 
    geom_vline(xintercept = 2010, linetype = "dashed")   
}


plot_grid(
  make_valband(seq(60, 85, by = 5)),
  make_valband(seq(60, 85, by = 5), identity_scale  = T),
  nrow = 2,
  labels = c("A", "B")
)

ggsave("figures/banded_plot_older_ages_alt.png", height = 25, width = 25, units = "cm", dpi = 300)

plot_grid(
  make_valband(c(0, 15, 25, 35, 45)),
  make_valband(c(0, 15, 25, 35, 45), identity_scale  = T),
  nrow = 2,
  labels = c("A", "B")
)

ggsave("figures/banded_plot_younger_ages_alt.png", height = 25, width = 25, units = "cm", dpi = 300)



# Alternative model, additional cumulative deaths 



draw_diffs <- function(dta, YEAR, XLIMS = c(0, 89), YLIMS = c(-12000, 12000), return_table = F){
  dta  %>% 
    mutate(year = year + 1990)  %>% 
    filter(year == YEAR) %>% 
    mutate(deaths_counterfactual = population * 10 ^ det_lmr_nl_alt)  %>% 
    group_by(sex)  %>% 
    arrange(age)  %>% 
    mutate(
      cm_mrt_actual = cumsum(deaths), 
      cm_mrt_counter = cumsum(deaths_counterfactual),
      dif = cm_mrt_actual - cm_mrt_counter
    ) -> diffs_estimates
  
  if (return_table) {return(diffs_estimates)}
  
  diffs_estimates %>%     
    ggplot(., aes(x =age, group = sex, shape = sex, linetype = sex)) +
    geom_line(aes(y = dif)) +
    scale_x_continuous(limits = XLIMS, breaks = c(0, seq(10, 90, by = 10))) + 
    scale_y_continuous(limits = YLIMS, breaks = seq(YLIMS[1], YLIMS[2], by = 2000), labels = comma) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 65, linetype = "dashed") + 
    theme_minimal() + 
    labs(x = "Age in years", y = "Total additional deaths by\n age on x axis", title = YEAR)
}


draw_diffs(fitted_twoscenarios, 2010, XLIMS = c(0, 89)) -> dd_2010
draw_diffs(fitted_twoscenarios, 2011, XLIMS = c(0, 89)) -> dd_2011
draw_diffs(fitted_twoscenarios, 2012, XLIMS = c(0, 89)) -> dd_2012
draw_diffs(fitted_twoscenarios, 2013, XLIMS = c(0, 89)) -> dd_2013
draw_diffs(fitted_twoscenarios, 2014, XLIMS = c(0, 89)) -> dd_2014
draw_diffs(fitted_twoscenarios, 2015, XLIMS = c(0, 89)) -> dd_2015


plot_grid(
  dd_2010, dd_2011, dd_2012,
  dd_2013, dd_2014, dd_2015, 
  nrow = 2
) -> g
print(g)

ggsave("figures/excess_deaths_2010_2015_alt.png", height = 30, width = 40, dpi = 300, units = "cm")


# 'excess' deaths by particular ages   - alternative model spec-------------------------------------

draw_diffs(fitted_twoscenarios, 2010, XLIMS = c(0, 89), return_table = T) -> t_2010
draw_diffs(fitted_twoscenarios, 2011, XLIMS = c(0, 89), return_table = T) -> t_2011
draw_diffs(fitted_twoscenarios, 2012, XLIMS = c(0, 89), return_table = T) -> t_2012
draw_diffs(fitted_twoscenarios, 2013, XLIMS = c(0, 89), return_table = T) -> t_2013
draw_diffs(fitted_twoscenarios, 2014, XLIMS = c(0, 89), return_table = T) -> t_2014
draw_diffs(fitted_twoscenarios, 2015, XLIMS = c(0, 89), return_table = T) -> t_2015

t_all <- reduce(list(t_2010, t_2011, t_2012, t_2013, t_2014, t_2015), bind_rows) %>%
  select(sex, age, year, cm_mrt_actual, cm_mrt_counter, dif)

t_all %>% filter(
  age %in% c(0, 15, 25, 35, 50, 60, 70, 80, 89)
) %>%
  mutate(
    cm_mrt_counter = round(cm_mrt_counter, 0),
    dif = round(dif, 0)
         ) %>% write.csv("clipboard")





# For each sex, age, year, extract 1000 estimates of numbers of excess deaths

make_sims_into_df <- function(yr, dths){
  output <- vector("list", length(yr))
  for (i in seq_along(dths)){
    sims <- seq_along(dths[[i]])
    these_dths <- dths[[i]]
    year <- yr[[i]]
    df <- data_frame(year = year, sim = sims, deaths = these_dths)
    output[[i]] <- df
  }
  output <- reduce(output, bind_rows)
  output  
}

model_outputs %>% 
  select(sex, age, data, dif_deaths_nl) %>% 
  mutate(year = map(data, ~.$year)) %>% 
  select(-data) %>% 
  mutate(sim_deaths = map2(year, dif_deaths_nl, make_sims_into_df)) %>% 
  select(sex, age, sim_deaths) %>% 
  unnest() -> dif_deaths_stoch

dif_deaths_stoch %>% 
  group_by(sex, age, year) %>% 
  summarise(
    d_0025 = quantile(deaths, 0.025),
    d_0050 = quantile(deaths, 0.050),
    d_0250 = quantile(deaths, 0.250),
    d_0500 = quantile(deaths, 0.500),
    d_0750 = quantile(deaths, 0.750),
    d_0950 = quantile(deaths, 0.950),
    d_0975 = quantile(deaths, 0.975)
         ) %>% 
  ungroup() %>% 
  mutate(year = year + 1990) %>% 
  filter(age %in% c(0, 5, 25, 50, 60, 65, 70, 75, 80, 85)) %>%
  filter(year >= 2010) %>% 
  ggplot(., aes(x = year)) + 
  geom_ribbon(aes(ymin = d_0250, ymax = d_0750), alpha = 0.2) + 
  geom_ribbon(aes(ymin = d_0050, ymax = d_0950), alpha = 0.2) + 
  geom_ribbon(aes(ymin = d_0025, ymax = d_0975), alpha = 0.2) + 
  geom_line(aes(y = d_0500)) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  facet_grid(sex ~ age) +
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x= "Year", y = "Differences in deaths at age", 
       title = "Modelled 'excess' deaths at different ages\ncompared with observed deaths")



model_outputs %>% 
  select(sex, age, data, dif_deaths_stoch) %>% 
  mutate(year = map(data, ~.$year)) %>% 
  select(-data) %>% 
  mutate(sim_deaths = map2(year, dif_deaths_stoch, make_sims_into_df)) %>% 
  select(sex, age, sim_deaths) %>% 
  unnest() -> dif_deaths_double_stoch

dif_deaths_double_stoch %>% 
  group_by(sex, age, year) %>% 
  summarise(
    d_0025 = quantile(deaths, 0.025),
    d_0050 = quantile(deaths, 0.050),
    d_0250 = quantile(deaths, 0.250),
    d_0500 = quantile(deaths, 0.500),
    d_0750 = quantile(deaths, 0.750),
    d_0950 = quantile(deaths, 0.950),
    d_0975 = quantile(deaths, 0.975)
  ) %>% 
  ungroup() %>% 
  mutate(year = year + 1990) %>% 
  filter(age %in% c(0, 5, 25, 50, 60, 65, 70, 75, 80, 85)) %>%
  filter(year >= 2010) %>% 
  ggplot(., aes(x = year)) + 
  geom_ribbon(aes(ymin = d_0250, ymax = d_0750), alpha = 0.2) + 
  geom_ribbon(aes(ymin = d_0050, ymax = d_0950), alpha = 0.2) + 
  geom_ribbon(aes(ymin = d_0025, ymax = d_0975), alpha = 0.2) + 
  geom_line(aes(y = d_0500)) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  geom_vline(aes(xintercept = 2010), linetype = "dashed") + 
  facet_grid(sex ~ age) +
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x= "Year", y = "Differences in deaths at age", 
       title = "Modelled 'excess' deaths at different ages\ncompared with modelled deaths in base scenario")


###################################################################


model_outputs  %>% 
  select(sex, age, data, det_lmr_nl)  %>% 
  unnest() -> fitted_twoscenarios



# ggsave("figures/age_fitted_scenarios.png", height = 30, width = 25, units = "cm", dpi = 300)

# Differences between fitted and actual deaths at different ages 
f2 <- fitted_twoscenarios %>% 
  filter(age %in% seq(60, 85, by = 5)) %>% 
  mutate(age = factor(age)) %>%
  mutate(year = year + 1990) %>% 
  filter(year >= 1997)

fitted_twoscenarios %>% 
  filter(age %in% seq(60, 85, by = 5)) %>% 
  mutate(age = factor(age)) %>% 
  ggplot(., aes(x = year + 1990, shape = age, group = age, colour = age)) + 
  geom_point(aes(y = 10^lmr)) + 
  facet_wrap(~sex) + 
  labs(x = "Year", y = "Mortality risk at age") +
  geom_vline(xintercept = 1997, linetype = "dashed") + 
  geom_vline(xintercept = 2010, linetype = "dashed") +
  geom_line(aes(x = year, y = 10^det_lmr_nl), data = f2) -> g_fitted_identity

print(g_fitted_identity)
ggsave(filename = "figures/olderages_identity.png", width = 20, height = 20, units = "cm", dpi = 300)

# ggsave("figures/age_fitted_scenarios_identity.png", height = 30, width = 25, units = "cm", dpi = 300)

# Log and identity scale on same image




# Cumulative, actual vs projected

plot_actualprojected <- function(
  YEAR, XLIMS = c(0, 89), YFOCUS = c(5000, 250000), 
  RETURN = "graph", identity_scale = T
  ){
  tmp <- fitted_twoscenarios %>% 
    filter(year == YEAR - 1990) %>% 
    select(sex, age, lmr, det_lmr_nl)  
  
  
  dta  %>% 
    filter(year == YEAR)  %>% 
    filter(place == "ew")  %>% 
    select(sex, age, population, deaths)  %>% 
    right_join(tmp) %>%
    filter(age >= XLIMS[1], age <= XLIMS[2]) %>% 
    mutate(
      mrt_actual = deaths, 
      mrt_proj = population * 10^det_lmr_nl
    ) %>% 
    group_by(sex) %>% 
    arrange(age) %>% 
    mutate(
      cm_mrt_actual = cumsum(mrt_actual),
      cm_mrt_proj = cumsum(mrt_proj)
    ) %>% 
    filter(age >= XLIMS[1], age <= XLIMS[2]) -> tables 
  

  y_scale <- if(identity_scale){
    scale_y_continuous(limits = c(0, 250000), labels = comma)
  } else {
    scale_y_log10(limits = YFOCUS, breaks = c(1000, 2000, 5000, 10000, 20000, 50000, 100000, 200000), labels = comma)
  }
  tables %>% 
    ggplot(., aes(x =age, group = sex, shape = sex, linetype = sex)) +
    geom_point(aes(y = cm_mrt_actual)) +
    geom_line(aes(y = cm_mrt_proj)) +
    scale_x_continuous(limits = XLIMS, breaks = c(0, seq(10, 90, by = 10))) + 
    y_scale + 
    theme_minimal() + 
    labs(x = "Age in years", y = "Total actual and projected mortality by age", title = YEAR) -> output
  
  if(RETURN == "table"){output <- tables}
  return(output)
}

plot_grid(
  plot_actualprojected(2010), 
  plot_actualprojected(2011), 
  plot_actualprojected(2012), 
  plot_actualprojected(2013), 
  plot_actualprojected(2014), 
  plot_actualprojected(2015), 
  nrow = 2
) -> g
print(g)

ggsave("figures/onsonly_excess_deaths_2010_2015.png", height = 30, width = 40, dpi = 300, units = "cm")

plot_grid(
  plot_actualprojected(2010, identity_scale = F, XLIMS = c(50, 89)), 
  plot_actualprojected(2011, identity_scale = F, XLIMS = c(50, 89)), 
  plot_actualprojected(2012, identity_scale = F, XLIMS = c(50, 89)), 
  plot_actualprojected(2013, identity_scale = F, XLIMS = c(50, 89)), 
  plot_actualprojected(2014, identity_scale = F, XLIMS = c(50, 89)), 
  plot_actualprojected(2015, identity_scale = F, XLIMS = c(50, 89)), 
  nrow = 2
) -> g
print(g)

# Now to export this as a table 

# plot_actualprojected(2010, RETURN = "table") %>% mutate(year = 2010)-> t_10
# plot_actualprojected(2011, RETURN = "table") %>% mutate(year = 2011)-> t_11
# plot_actualprojected(2012, RETURN = "table") %>% mutate(year = 2012)-> t_12
# plot_actualprojected(2013, RETURN = "table") %>% mutate(year = 2013)-> t_13
# plot_actualprojected(2014, RETURN = "table") %>% mutate(year = 2014)-> t_14
# plot_actualprojected(2015, RETURN = "table") %>% mutate(year = 2015)-> t_15
# 
# t_all <- Reduce(bind_rows, list(t_10, t_11, t_12, t_13, t_14, t_15))
# 
# t_all %>% 
#   select(sex, year, age, population, lmr, pred_nl, 
#          mrt_actual, mrt_proj, cm_mrt_actual, cm_mrt_proj
#   ) -> t_all

# sheet_actualprojected <- createSheet(wb, sheetName = "actual_projected")
# addDataFrame(t_all, sheet_actualprojected)



# # For the numbers themselves 
# 
# draw_diffs(2010, return_table = T) -> d_10
# draw_diffs(2011, return_table = T) -> d_11
# draw_diffs(2012, return_table = T) -> d_12
# draw_diffs(2013, return_table = T) -> d_13
# draw_diffs(2014, return_table = T) -> d_14
# draw_diffs_extrapolate(2015, return_table = T) -> d_15
# 
# d_all <- Reduce(bind_rows, list(d_10, d_11, d_12, d_13, d_14, d_15))

# d_all %>% 
#   filter(!is.na(dif)) %>% 
#   mutate(dif = round(dif, 0)) %>% 
#   spread(year, dif) -> spread_diffs
# 
# sheet_differences <- createSheet(wb, sheetName = "differences_by_age_year")
# addDataFrame(spread_diffs, sheet_differences)

