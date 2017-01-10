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

run_model_25 <- function(dta){ 
  lm(lmr ~ lab * recession * (year + I(year^2) + I(year^3) + I(year^4)) , data = dta)
}

run_model_26 <- function(dta){ 
  lm(lmr ~ (year + I(year^2) + I(year^3) + I(year^4) + I(year^5)) , data = dta)
}

run_model_27 <- function(dta){ 
  lm(lmr ~ lab + (year + I(year^2) + I(year^3) + I(year^4) + I(year^5)) , data = dta)
}

run_model_28 <- function(dta){ 
  lm(lmr ~ recession + (year + I(year^2) + I(year^3) + I(year^4) + I(year^5)) , data = dta)
}

run_model_29 <- function(dta){ 
  lm(lmr ~ lab + recession + (year + I(year^2) + I(year^3) + I(year^4) + I(year^5)) , data = dta)
}

run_model_30 <- function(dta){ 
  lm(lmr ~ lab * recession + (year + I(year^2) + I(year^3) + I(year^4) + I(year^5)) , data = dta)
}

run_model_31 <- function(dta){ 
  lm(lmr ~ lab * recession * year + I(year^2) + I(year^3) + I(year^4) + I(year^5) , data = dta)
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

# The aim is to fit the models on data from 1961 to 2010, and use this to project to 
# 2010-2014, and then 2015 up to age 89 years

# This will mean a more consistent approach can be used if looking at models which only incorporate 
# year, and also allows two scenarios to be looked at for models including LAB and REC as terms. 



dta  %>% 
  filter(year <= 2010) %>% 
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
    mdl_24 = map(data, run_model_24),
    mdl_25 = map(data, run_model_25),
    mdl_26 = map(data, run_model_26),
    mdl_27 = map(data, run_model_27),
    mdl_28 = map(data, run_model_28),
    mdl_29 = map(data, run_model_29),
    mdl_30 = map(data, run_model_30)
    
    
  ) %>% 
  select(-data) %>% 
  gather(key = "mod_number", value = "model", mdl_01:mdl_30) %>% 
  mutate(mod_number = str_replace(mod_number, "mdl_", "")) -> all_models




# n_terms <- function(mdl){
#   mdl %>% coefficients %>% length
# }
# # Number of model terms 
# all_models_1961 %>% 
#   filter(sex == "female", age == 0) %>% 
#   mutate(num_terms = map_dbl(model, n_terms)) %>% 
#   View
# 
# AIC, BIC and Rsquared by age 

all_models %>% 
  mutate(
    aic = map_dbl(model, AIC),
    bic = map_dbl(model, BIC),
    rsq = map_dbl(model, function(x) {summary(x)$r.squared}),
    rsq_adj = map_dbl(model, function(x) {summary(x)$adj.r.squared})
    ) %>% 
  select(-model) %>% 
  mutate(start_year = 1961) %>% 
  select(mod_number, start_year, sex, age, aic:rsq_adj) -> mod_summaries



# Now to explore this 


mod_summaries %>% 
  filter(start_year == 1961) %>% 
  group_by(mod_number) %>% 
  summarise_each(funs(mean), aic:rsq_adj) %>%
  gather(key = "metric", value = "value", aic:rsq_adj) %>% 
  ggplot(., aes(x = mod_number, y = value)) + 
  geom_point() + facet_wrap(~ metric, nrow = 2, scales = "free_y") + 
  theme_minimal() +
  labs(title = "Comparison of mean model fits", x = "Model Number", y = "Fit")
ggsave("figures/model_fit_comparison.png", height = 15, width = 25, units = "cm", dpi = 300)


# Same, but for ages 0 and 50+

mod_summaries %>% 
  filter(start_year == 1961) %>% 
  filter(age %in% c(0, 50:89)) %>% 
  group_by(mod_number) %>% 
  summarise_each(funs(mean), aic:rsq_adj) %>%
  gather(key = "metric", value = "value", aic:rsq_adj) %>% 
  ggplot(., aes(x = mod_number, y = value)) + 
  geom_point() + facet_wrap(~ metric, nrow = 2, scales = "free_y") + 
  theme_minimal() 
ggsave("figures/model_fit_comparison_50plus.png", height = 15, width = 25, units = "cm", dpi = 300)




# Adjusted R-squared indicates model 11 for both start dates
# then model 8 for 1961 onwards, and model 2 for 1990 onwards


# I'm now looking at model 19

mod_summaries %>% 
  filter(mod_number == "19") %>% 
  filter(age <= 100) %>% 
  ggplot(., aes(x = age, colour = sex, y = rsq_adj)) + 
  geom_point() +
  scale_x_continuous("Age", breaks = c(0, seq(10, 100, 10))) + 
  scale_y_continuous("Adjusted R-squared", limits = c(0, 1), breaks = c(0, seq(0.2, 1.0, by = 0.2))) +
  theme_minimal() +
  labs(title = "Adjusted R-Squared by age/sex for Model 19")
ggsave("figures/adj_rsq_for_mod_19.png", dpi = 300, units = "cm", height = 15, width = 20)

mod_summaries %>% 
  filter(mod_number == "16") %>% 
  filter(age <= 100) %>% 
  ggplot(., aes(x = age, colour = sex, y = rsq_adj)) + 
  geom_point() +
  scale_x_continuous("Age", breaks = c(0, seq(10, 100, 10))) + 
  scale_y_continuous("Adjusted R-squared", limits = c(0, 1), breaks = c(0, seq(0.2, 1.0, by = 0.2))) +
  theme_minimal() +
  labs(title = "Adjusted R-Squared by age/sex for Model 16")
ggsave("figures/adj_rsq_for_mod_16.png", dpi = 300, units = "cm", height = 15, width = 20)

mod_summaries %>% 
  filter(mod_number == "25") %>% 
  filter(age <= 100) %>% 
  ggplot(., aes(x = age, colour = sex, y = rsq_adj)) + 
  geom_point() +
  scale_x_continuous("Age", breaks = c(0, seq(10, 100, 10))) + 
  scale_y_continuous("Adjusted R-squared", limits = c(0, 1), breaks = c(0, seq(0.2, 1.0, by = 0.2))) +
  theme_minimal() +
  labs(title = "Adjusted R-Squared by age/sex for Model 25")
ggsave("figures/adj_rsq_for_mod_16.png", dpi = 300, units = "cm", height = 15, width = 20)


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
  unnest() -> fitted_best_model

best_model_sims %>% 
  select(sex, age, data, sim_lmrs_lab) %>% 
  mutate(
    ul_lwr = map(sim_lmrs_lab, function(x) {map_dbl(x, quantile, probs = 0.025)}),
    ul_upr = map(sim_lmrs_lab, function(x) {map_dbl(x, quantile, probs = 0.975)})
  ) %>% 
  select(sex, age, data, ul_lwr, ul_upr)  %>% 
  unnest() -> mod_bands 

mod_bands <- fitted_best_model  %>% 
  select(sex, age, det_lmr_lab, place, year)  %>% 
  inner_join(mod_bands) 

make_valband <- function(ages, dta_start_year = 1961, identity_scale = F){
  
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
    mutate(year = year + dta_start_year) %>% 
    # mutate(
    #   ul_lwr = ifelse(year < 1997, NA, ul_lwr),
    #   ul_upr = ifelse(year < 1997, NA, ul_upr),
    #   det_lmr_lab = ifelse(year < 1997, NA, det_lmr_lab)
    # ) %>%
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



# As above, but transition to Thatcher

make_valband2 <- function(ages, min_year = 1961, max_year = 1985, dta_start_year = 1961, identity_scale = F){
  
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
    mutate(year = year + dta_start_year) %>% 
    mutate(
      ul_lwr = ifelse(year < 1974, NA, ul_lwr),
      ul_upr = ifelse(year < 1974, NA, ul_upr),
      det_lmr_lab = ifelse(year < 1974, NA, det_lmr_lab)
    ) %>%
    filter(age %in% ages) %>% 
    filter(year >= min_year, year <= max_year) %>% 
    mutate(age = factor(age)) %>% 
    ggplot(., aes(x = year, group = age)) + 
    geom_ribbon(aes(ymin = ul_lwr, ymax = ul_upr), alpha = 0.2) + 
    geom_line(aes(y = det_lmr_lab)) + 
    geom_point(aes(y = lmr, colour = age, shape = age)) + 
    facet_wrap(~sex) + 
    labs(x = "Year", y = ifelse(identity_scale, "Mortality risk at age", "Base 10 log mortality risk at age")) + 
    geom_vline(xintercept = 1974, linetype = "dashed") + 
    geom_vline(xintercept = 1979, linetype = "dashed")   
}


plot_grid(
  make_valband2(seq(60, 85, by = 5)),
  make_valband2(seq(60, 85, by = 5), identity_scale  = T),
  nrow = 2,
  labels = c("A", "B")
)

ggsave("figures/banded_plot_older_ages_thatcher.png", height = 25, width = 25, units = "cm", dpi = 300)

# As above, but transition to Heath

make_valband3 <- function(ages, min_year = 1961, max_year = 1974, dta_start_year = 1961, identity_scale = F){
  
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
    mutate(year = year + dta_start_year) %>% 
    mutate(
      ul_lwr = ifelse(year < 1961, NA, ul_lwr),
      ul_upr = ifelse(year < 1961, NA, ul_upr),
      det_lmr_lab = ifelse(year < 1961, NA, det_lmr_lab)
    ) %>%
    filter(age %in% ages) %>% 
    filter(year >= min_year, year <= max_year) %>% 
    mutate(age = factor(age)) %>% 
    ggplot(., aes(x = year, group = age)) + 
    geom_ribbon(aes(ymin = ul_lwr, ymax = ul_upr), alpha = 0.2) + 
    geom_line(aes(y = det_lmr_lab)) + 
    geom_point(aes(y = lmr, colour = age, shape = age)) + 
    facet_wrap(~sex) + 
    labs(x = "Year", y = ifelse(identity_scale, "Mortality risk at age", "Base 10 log mortality risk at age")) + 
    geom_vline(xintercept = 1970, linetype = "dashed")   
}


plot_grid(
  make_valband3(seq(60, 85, by = 5)),
  make_valband3(seq(60, 85, by = 5), identity_scale  = T),
  nrow = 2,
  labels = c("A", "B")
)

ggsave("figures/banded_plot_older_ages_heath.png", height = 25, width = 25, units = "cm", dpi = 300)



# Figure S1 - banded plots at younger ages  -------------------------------

plot_grid(
  make_valband(c(0, 15, 25, 35, 45, 55)),
  make_valband(c(0, 15, 25, 35, 45, 55), identity_scale  = T),
  nrow = 2,
  labels = c("A", "B")
)

ggsave("figures/banded_plot_younger.png", height = 25, width = 25, units = "cm", dpi = 300)


plot_grid(
  make_valband2(c(0, 15, 25, 35, 45)),
  make_valband2(c(0, 15, 25, 35, 45), identity_scale  = T),
  nrow = 2,
  labels = c("A", "B")
)

ggsave("figures/banded_plot_younger_thatcher.png", height = 25, width = 25, units = "cm", dpi = 300)


plot_grid(
  make_valband3(c(0, 15, 25, 35, 45)),
  make_valband3(c(0, 15, 25, 35, 45), identity_scale  = T),
  nrow = 2,
  labels = c("A", "B")
)

ggsave("figures/banded_plot_younger_heath.png", height = 25, width = 25, units = "cm", dpi = 300)


# Figure 3 - total excess mortality up to age 89 years  -------------------


draw_diffs <- function(dta, YEAR, XLIMS = c(0, 89), YLIMS = c(-20000, 20000), return_table = F){
  dta  %>% 
    mutate(year = year + 1961)  %>% 
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


draw_diffs(fitted_best_model, 2010, XLIMS = c(0, 89)) -> dd_2010
draw_diffs(fitted_best_model, 2011, XLIMS = c(0, 89)) -> dd_2011
draw_diffs(fitted_best_model, 2012, XLIMS = c(0, 89)) -> dd_2012
draw_diffs(fitted_best_model, 2013, XLIMS = c(0, 89)) -> dd_2013
draw_diffs(fitted_best_model, 2014, XLIMS = c(0, 89)) -> dd_2014
draw_diffs(fitted_best_model, 2015, XLIMS = c(0, 89)) -> dd_2015


plot_grid(
  dd_2010, dd_2011, dd_2012,
  dd_2013, dd_2014, dd_2015, 
  nrow = 2
) -> g
print(g)

ggsave("figures/excess_deaths_2010_2015.png", height = 30, width = 40, dpi = 300, units = "cm")


# Same again, but up for years 1979-84





draw_diffs(fitted_best_model, 1979, XLIMS = c(0, 89)) -> dd_1979
draw_diffs(fitted_best_model, 1980, XLIMS = c(0, 89)) -> dd_1980
draw_diffs(fitted_best_model, 1981, XLIMS = c(0, 89)) -> dd_1981
draw_diffs(fitted_best_model, 1982, XLIMS = c(0, 89)) -> dd_1982
draw_diffs(fitted_best_model, 1983, XLIMS = c(0, 89)) -> dd_1983
draw_diffs(fitted_best_model, 1984, XLIMS = c(0, 89)) -> dd_1984


plot_grid(
  dd_1979, dd_1980, dd_1981,
  dd_1982, dd_1983, dd_1984, 
  nrow = 2
) -> g
print(g)

ggsave("figures/excess_deaths_1979_1984.png", height = 30, width = 40, dpi = 300, units = "cm")

# Now for 1970-1974

draw_diffs(fitted_best_model, 1970, XLIMS = c(0, 89)) -> dd_1970
draw_diffs(fitted_best_model, 1971, XLIMS = c(0, 89)) -> dd_1971
draw_diffs(fitted_best_model, 1972, XLIMS = c(0, 89)) -> dd_1972
draw_diffs(fitted_best_model, 1973, XLIMS = c(0, 89)) -> dd_1973
draw_diffs(fitted_best_model, 1974, XLIMS = c(0, 89)) -> dd_1974


plot_grid(
  dd_1970, dd_1971, dd_1972,
  dd_1973, dd_1974, 
  nrow = 2
) -> g
print(g)

ggsave("figures/excess_deaths_1970_1974.png", height = 30, width = 40, dpi = 300, units = "cm")




# 'excess' deaths by particular ages  -------------------------------------
# 
draw_diffs(fitted_best_model, 2010, XLIMS = c(0, 89), return_table = T) -> t_2010
draw_diffs(fitted_best_model, 2011, XLIMS = c(0, 89), return_table = T) -> t_2011
draw_diffs(fitted_best_model, 2012, XLIMS = c(0, 89), return_table = T) -> t_2012
draw_diffs(fitted_best_model, 2013, XLIMS = c(0, 89), return_table = T) -> t_2013
draw_diffs(fitted_best_model, 2014, XLIMS = c(0, 89), return_table = T) -> t_2014
draw_diffs(fitted_best_model, 2015, XLIMS = c(0, 89), return_table = T) -> t_2015

t_all <- reduce(list(t_2010, t_2011, t_2012, t_2013, t_2014, t_2015), bind_rows) %>%
  select(sex, age, year, cm_mrt_actual, cm_mrt_counter, dif)

t_all %>% filter(
  age %in% c(0, 15, 25, 35, 50, 60, 70, 80, 89)
) %>%
  mutate(
    cm_mrt_counter = round(cm_mrt_counter, 0),
    dif = round(dif, 0)
         ) %>% write.csv("clipboard")

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

ggsave("figures/ons_only_coefficients_with_age.png", height =40, width = 25, dpi = 300, units = "cm")



draw_diffs(fitted_best_model, 2010, XLIMS = c(0, 89), return_table = T) -> t_2010
draw_diffs(fitted_best_model, 2011, XLIMS = c(0, 89), return_table = T) -> t_2011
draw_diffs(fitted_best_model, 2012, XLIMS = c(0, 89), return_table = T) -> t_2012
draw_diffs(fitted_best_model, 2013, XLIMS = c(0, 89), return_table = T) -> t_2013
draw_diffs(fitted_best_model, 2014, XLIMS = c(0, 89), return_table = T) -> t_2014
draw_diffs(fitted_best_model, 2015, XLIMS = c(0, 89), return_table = T) -> t_2015

t_all <- reduce(list(t_2010, t_2011, t_2012, t_2013, t_2014, t_2015), bind_rows) %>%
  select(sex, age, year, cm_mrt_actual, cm_mrt_counter, dif)

t_all %>% filter(
  age %in% c(0, 15, 25, 35, 50, 60, 70, 80, 89)
) %>%
  mutate(
    cm_mrt_counter = round(cm_mrt_counter, 0),
    dif = round(dif, 0)
         ) %>% write.csv("clipboard")





###################################################################


model_outputs  %>% 
  select(sex, age, data, det_lmr_lab)  %>% 
  unnest() -> fitted_best_model



# ggsave("figures/age_fitted_scenarios.png", height = 30, width = 25, units = "cm", dpi = 300)

# Differences between fitted and actual deaths at different ages 
f2 <- best_model_sims %>% 
  filter(age %in% seq(60, 85, by = 5)) %>% 
  mutate(age = factor(age)) %>%
  mutate(year = year + 1961) %>% 
  filter(year >= 1997)

fitted_best_model %>% 
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




# # Cumulative, actual vs projected
# 
# plot_actualprojected <- function(
#   YEAR, XLIMS = c(0, 89), YFOCUS = c(5000, 250000), 
#   RETURN = "graph", identity_scale = T
#   ){
#   tmp <- fitted_best_model %>% 
#     filter(year == YEAR - 1961) %>% 
#     select(sex, age, lmr, det_lmr_lab)  
#   
#   
#   dta  %>% 
#     filter(year == YEAR)  %>% 
#     filter(place == "ew")  %>% 
#     select(sex, age, population, deaths)  %>% 
#     right_join(tmp) %>%
#     filter(age >= XLIMS[1], age <= XLIMS[2]) %>% 
#     mutate(
#       mrt_actual = deaths, 
#       mrt_proj = population * 10^det_lmr_lab
#     ) %>% 
#     group_by(sex) %>% 
#     arrange(age) %>% 
#     mutate(
#       cm_mrt_actual = cumsum(mrt_actual),
#       cm_mrt_proj = cumsum(mrt_proj)
#     ) %>% 
#     filter(age >= XLIMS[1], age <= XLIMS[2]) -> tables 
#   
# 
#   y_scale <- if(identity_scale){
#     scale_y_continuous(limits = c(0, 250000), labels = comma)
#   } else {
#     scale_y_log10(limits = YFOCUS, breaks = c(1000, 2000, 5000, 10000, 20000, 50000, 100000, 200000), labels = comma)
#   }
#   tables %>% 
#     ggplot(., aes(x =age, group = sex, shape = sex, linetype = sex)) +
#     geom_point(aes(y = cm_mrt_actual)) +
#     geom_line(aes(y = cm_mrt_proj)) +
#     scale_x_continuous(limits = XLIMS, breaks = c(0, seq(10, 90, by = 10))) + 
#     y_scale + 
#     theme_minimal() + 
#     labs(x = "Age in years", y = "Total actual and projected mortality by age", title = YEAR) -> output
#   
#   if(RETURN == "table"){output <- tables}
#   return(output)
# }
# 
# plot_grid(
#   plot_actualprojected(2010), 
#   plot_actualprojected(2011), 
#   plot_actualprojected(2012), 
#   plot_actualprojected(2013), 
#   plot_actualprojected(2014), 
#   plot_actualprojected(2015), 
#   nrow = 2
# ) -> g
# print(g)
# 
# ggsave("figures/onsonly_excess_deaths_2010_2015.png", height = 30, width = 40, dpi = 300, units = "cm")
# 
# plot_grid(
#   plot_actualprojected(2010, identity_scale = F, XLIMS = c(50, 89)), 
#   plot_actualprojected(2011, identity_scale = F, XLIMS = c(50, 89)), 
#   plot_actualprojected(2012, identity_scale = F, XLIMS = c(50, 89)), 
#   plot_actualprojected(2013, identity_scale = F, XLIMS = c(50, 89)), 
#   plot_actualprojected(2014, identity_scale = F, XLIMS = c(50, 89)), 
#   plot_actualprojected(2015, identity_scale = F, XLIMS = c(50, 89)), 
#   nrow = 2
# ) -> g
# print(g)
# 
# # Now to export this as a table 
# 
#  plot_actualprojected(2010, RETURN = "table") %>% mutate(year = 2010)-> t_10
#  plot_actualprojected(2011, RETURN = "table") %>% mutate(year = 2011)-> t_11
#  plot_actualprojected(2012, RETURN = "table") %>% mutate(year = 2012)-> t_12
#  plot_actualprojected(2013, RETURN = "table") %>% mutate(year = 2013)-> t_13
#  plot_actualprojected(2014, RETURN = "table") %>% mutate(year = 2014)-> t_14
#  plot_actualprojected(2015, RETURN = "table") %>% mutate(year = 2015)-> t_15
#  
#  t_all <- Reduce(bind_rows, list(t_10, t_11, t_12, t_13, t_14, t_15))
#  
#  t_all %>% 
#    select(sex, year, age, population, lmr, det_lmr_lab, 
#           mrt_actual, mrt_proj, cm_mrt_actual, cm_mrt_proj
#    ) -> t_all
# 

 
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

