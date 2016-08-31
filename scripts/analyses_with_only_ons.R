# Analyses with ONS data only and simpler method

rm(list = ls())

require(pacman)
pacman::p_load(
  
  readr, readxl, xlsx,
  purrr, tidyr, dplyr, 
  broom,
  ggplot2, cowplot, scales
)


source("scripts/extract_combined_ons.R")




# Modelling ---------------------------------------------------------------

# What I want to do: for each variable in each model, produce 10 000 draws of distributions 

# First, get this right for one model
run_regression <- function(dta){
  lm(lmr ~ year * (newlab + recession), data = dta)
}

extract_coeffs <- function(mdl){
  mdl %>% coefficients() 
}

extract_vcov <- function(mdl){
  mdl %>% vcov()
}

sim_betas <- function(cfs, vcv, n_sim = 1000){
  MASS::mvrnorm(n = n_sim, mu = cfs, Sigma = vcv)
}

vectorise_predictors <- function(dta, set_nl = F){
  # The output should be a list of vectors, one for each year
  out <- list()
  for (i in 1:nrow(dta)){
    yr <- as.numeric(dta[i, "year"])
    nl <- ifelse(set_nl, 1, as.numeric(dta[i, "newlab"]))
    rc <- as.numeric(dta[i, "recession"])
    this_vec <- c(1, yr, nl, rc, yr * nl, yr * rc)
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


dta  %>% 
  filter(age <= 95)  %>% 
  filter(year >= 1990)  %>% 
  filter(place == "ew") %>% 
  mutate(lmr = log(deaths/population, 10)) %>% 
  mutate(
    newlab = year >= 1997 & year <= 2010, 
    recession = year %in% 2008:2009
  )  %>% 
  mutate(year = year - min(year)) %>% 
  group_by(sex, age)  %>% 
  nest()  %>%
  mutate(
    mdl = map(data, run_regression),
    coeffs  = map(mdl, extract_coeffs),
    vcv = map(mdl, extract_vcov),
    betas = map2(coeffs, vcv, sim_betas),
    prd_vec = map(data, vectorise_predictors),
    prd_vec_nl = map(data, vectorise_predictors, set_nl = T),
    det_lmr_nl = map2(prd_vec_nl, coeffs, make_deterministic_lmrs),
    sim_lmrs = map2(prd_vec, betas, make_stochastic_lmrs),
    sim_lmrs_nl = map2(prd_vec_nl, betas, make_stochastic_lmrs),
    sim_deaths = map2(data, sim_lmrs, predict_deaths),
    sim_deaths_nl = map2(data, sim_lmrs_nl, predict_deaths),
    dif_deaths_nl = map2(map(data, ~ .$deaths), sim_deaths_nl, compare_actual_w_counter),
    dif_deaths_stoch = map2(sim_deaths, sim_deaths_nl, compare_model_w_countermodel)
  ) -> model_outputs


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
  filter(year >= 1997) %>% 
  ggplot(., aes(x = year)) + 
  geom_ribbon(aes(ymin = d_0250, ymax = d_0750), alpha = 0.2) + 
  geom_ribbon(aes(ymin = d_0050, ymax = d_0950), alpha = 0.2) + 
  geom_ribbon(aes(ymin = d_0025, ymax = d_0975), alpha = 0.2) + 
  geom_line(aes(y = d_0500)) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  geom_vline(aes(xintercept = 2010), linetype = "dashed") + 
  facet_grid(sex ~ age) + 
  coord_flip()


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
  theme(axis.text.x = element_text(angle = 90))


###################################################################


model_outputs  %>% 
  select(sex, age, data, det_lmr_nl)  %>% 
  unnest() -> fitted_twoscenarios

fitted_twoscenarios %>% 
  filter(age %in% seq(60, 85, by = 5)) %>% 
  mutate(age = factor(age)) %>% 
  ggplot(., aes(x = year + 1990, shape = age, group = age, colour = age)) + 
  geom_point(aes(y = lmr)) + 
  geom_line(aes(y = det_lmr_nl)) + 
  facet_wrap(~sex) + 
  labs(x = "Year", y = "Base 10 log mortality risk at age") + 
  geom_vline(xintercept = 1997, linetype = "dashed") + 
  geom_vline(xintercept = 2010, linetype = "dashed") -> g_fitted_log

print(g_fitted_log)
# ggsave("figures/age_fitted_scenarios.png", height = 30, width = 25, units = "cm", dpi = 300)

# As above but actual not log

fitted_twoscenarios %>% 
  filter(age %in% seq(60, 85, by = 5)) %>% 
  mutate(age = factor(age)) %>% 
  ggplot(., aes(x = year + 1990, shape = age, group = age, colour = age)) + 
  geom_point(aes(y = 10^lmr)) + 
  geom_line(aes(y = 10^det_lmr_nl)) + 
  facet_wrap(~sex) + 
  labs(x = "Year", y = "Mortality risk at age") +
  geom_vline(xintercept = 1997, linetype = "dashed") + 
  geom_vline(xintercept = 2010, linetype = "dashed") -> g_fitted_identity

print(g_fitted_identity)
# ggsave("figures/age_fitted_scenarios_identity.png", height = 30, width = 25, units = "cm", dpi = 300)

# Log and identity scale on same image

plot_grid(
  g_fitted_log, g_fitted_identity, labels = c("A", "B"),
  ncol = 1
) 
ggsave(filename = "figures/olderages_composite.png", width = 20, height = 20, units = "cm", dpi = 300)


# As above, but with credible intervals 

model_outputs %>% 
  select(sex, age, data, sim_lmrs_nl) %>% 
  mutate(
    ul_lwr = map(sim_lmrs_nl, function(x) {map_dbl(x, quantile, probs = 0.025)}),
    ul_upr = map(sim_lmrs_nl, function(x) {map_dbl(x, quantile, probs = 0.975)})
  ) %>% 
  select(sex, age, data, ul_lwr, ul_upr)  %>% 
  unnest() -> mod_bands 

make_valband <- function(ages){
  mod_bands %>% 
    filter(age %in% ages) %>% 
    mutate(age = factor(age)) %>% 
    ggplot(., aes(x = year + 1990, group = age)) + 
    geom_ribbon(aes(ymin = ul_lwr, ymax = ul_upr), alpha = 0.2) + 
    geom_point(aes(y = lmr, colour = age, shape = age)) + 
    facet_wrap(~sex) + 
    labs(x = "Year", y = "Base 10 log mortality risk at age") + 
    geom_vline(xintercept = 1997, linetype = "dashed") + 
    geom_vline(xintercept = 2010, linetype = "dashed")   
}

make_valband(seq(0, 25, by = 5))
make_valband(seq(30, 55, by = 5))
make_valband(seq(60, 85, by = 5))

plot_grid(
  make_valband(seq(0, 25, by = 5)),
  make_valband(seq(30, 55, by = 5)),
  make_valband(seq(60, 85, by = 5)),
  nrow = 3
)



# Cumulative, actual vs projected

plot_actualprojected <- function(YEAR, XLIMS = c(0, 95), RETURN = "graph"){
  tmp <- fitted_twoscenarios %>% 
    filter(year == YEAR - 1990) %>% 
    select(sex, age, lmr, det_lmr_nl)  
  
  
  dta  %>% 
    filter(year == YEAR)  %>% 
    filter(place == "ew")  %>% 
    select(sex, age, population)  %>% 
    right_join(tmp) %>%
    filter(age >= XLIMS[1], age <= XLIMS[2]) %>% 
    mutate(
      mrt_actual = population * 10^lmr, 
      mrt_proj = population * 10^pred_nl
    ) %>% 
    group_by(sex) %>% 
    arrange(age) %>% 
    mutate(
      cm_mrt_actual = cumsum(mrt_actual),
      cm_mrt_proj = cumsum(mrt_proj)
    ) -> tables 
  
  
  tables %>% 
    ggplot(., aes(x =age, group = sex, shape = sex, linetype = sex)) +
    geom_point(aes(y = cm_mrt_actual)) +
    geom_line(aes(y = cm_mrt_proj)) +
    scale_x_continuous(limits = c(0, 95), breaks = c(0, seq(10, 90, by = 10))) + 
    scale_y_continuous(limits = c(0, 250000), labels = comma) +
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
  plot_actualprojected(2015, XLIMS = c(0, 89)), 
  nrow = 2
) -> g
print(g)

ggsave("figures/onsonly_excess_deaths_2010_2015.png", height = 30, width = 40, dpi = 300, units = "cm")

# Now to export this as a table 

plot_actualprojected(2010, RETURN = "table") %>% mutate(year = 2010)-> t_10
plot_actualprojected(2011, RETURN = "table") %>% mutate(year = 2011)-> t_11
plot_actualprojected(2012, RETURN = "table") %>% mutate(year = 2012)-> t_12
plot_actualprojected(2013, RETURN = "table") %>% mutate(year = 2013)-> t_13
plot_actualprojected(2014, RETURN = "table") %>% mutate(year = 2014)-> t_14
plot_actualprojected(2015, RETURN = "table") %>% mutate(year = 2015)-> t_15

t_all <- Reduce(bind_rows, list(t_10, t_11, t_12, t_13, t_14, t_15))

t_all %>% 
  select(sex, year, age, population, lmr, pred_nl, 
         mrt_actual, mrt_proj, cm_mrt_actual, cm_mrt_proj
  ) -> t_all

# sheet_actualprojected <- createSheet(wb, sheetName = "actual_projected")
# addDataFrame(t_all, sheet_actualprojected)



# As above, but log scale
plot_actualprojected_log <- function(
  YEAR, XLIMS = c(0, 95), XFOCUS = c(0, 95), YFOCUS = c(500, 250000)
){
  tmp <- fitted_twoscenarios %>% 
    filter(year == YEAR - 1990) %>% 
    select(sex, age, lmr, pred_nl)  
  
  
  dta  %>% 
    filter(year == YEAR)  %>% 
    filter(place == "ew")  %>% 
    select(sex, age, population)  %>% 
    right_join(tmp) %>%
    filter(age >= XLIMS[1], age <= XLIMS[2]) %>% 
    mutate(
      mrt_actual = population * 10^lmr, 
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
    scale_x_continuous(limits = XFOCUS, breaks = c(0, seq(10, 90, by = 10))) + 
    scale_y_log10(limits = YFOCUS, breaks = c(1000, 2000, 5000, 10000, 20000, 50000, 100000, 200000), labels = comma) +
    theme_minimal() + 
    labs(x = "Age in years", y = "Total actual and projected mortality by age", title = YEAR) -> output
  return(output)
}

plot_grid(
  plot_actualprojected_log(2010), 
  plot_actualprojected_log(2011), 
  plot_actualprojected_log(2012), 
  plot_actualprojected_log(2013), 
  plot_actualprojected_log(2014), 
  plot_actualprojected_log(2015, XLIMS = c(0, 89)), 
  nrow = 2
) -> g

print(g)

ggsave("figures/ons_only_total_actual_and_projected_2010_2015_logscale.png", height = 30, width = 40, dpi = 300, units = "cm")

# As above, but zoomed into older ages 

plot_grid(
  plot_actualprojected_log(2010, XFOCUS = c(60, 95), YFOCUS = c(20000, 250000)), 
  plot_actualprojected_log(2011, XFOCUS = c(60, 95), YFOCUS = c(20000, 250000)), 
  plot_actualprojected_log(2012, XFOCUS = c(60, 95), YFOCUS = c(20000, 250000)), 
  plot_actualprojected_log(2013, XFOCUS = c(60, 95), YFOCUS = c(20000, 250000)), 
  plot_actualprojected_log(2014, XFOCUS = c(60, 95), YFOCUS = c(20000, 250000)), 
  plot_actualprojected_log(2015, XFOCUS = c(60, 95), YFOCUS = c(20000, 250000), XLIMS = c(0, 89)),
  nrow = 2 ) -> g
print(g)

ggsave("figures/ons_only_total_actual_and_projected_2010_2015_logscale_olderages.png", height = 30, width = 40, dpi = 300, units = "cm")




draw_diffs <- function(dta, YEAR, XLIMS = c(-12000, 20000), return_table = F){
  dta  %>% 
    mutate(year = year + 1990)  %>% 
    filter(year == YEAR) %>% 
    mutate(deaths_counterfactual = population * 10 ^ det_lmr_nl)  %>% 
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
    scale_x_continuous(limits = c(0, 95), breaks = c(0, seq(10, 90, by = 10))) + 
    scale_y_continuous(limits = XLIMS, breaks = seq(XLIMS[1], XLIMS[2], by = 2000), labels = comma) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 65, linetype = "dashed") + 
    theme_minimal() + 
    labs(x = "Age in years", y = "Total additional deaths by\n age on x axis", title = YEAR)
}

draw_diffs(fitted_twoscenarios, 2010)
draw_diffs(fitted_twoscenarios, 2011)
draw_diffs(fitted_twoscenarios, 2012)
draw_diffs(fitted_twoscenarios, 2013)
draw_diffs(fitted_twoscenarios, 2014)
draw_diffs(fitted_twoscenarios, 2015)


draw_diffs_extrapolate <- function(dta, YEAR, XLIMS = c(-12000, 20000), return_table = F){
  dta  %>% 
    mutate(year = year + 1990)  %>% 
    filter(year == YEAR) %>% 
    mutate(deaths_counterfactual = population * 10 ^ det_lmr_nl)  %>% 
    group_by(sex)  %>% 
    arrange(age)  %>% 
    mutate(
      cm_mrt_actual = cumsum(deaths), 
      cm_mrt_counter = cumsum(deaths_counterfactual),
      dif = cm_mrt_actual - cm_mrt_counter
    ) -> diffs_estimates
  
  male_increment <- diffs_estimates  %>% 
    filter(sex == "male")  %>% 
    filter(age %in% 84:89)  %>% 
    lm(dif ~ age, data = .)  %>% 
    coefficients()  %>% .["age"]
  
  female_increment <- diffs_estimates  %>% 
    filter(sex == "female")  %>% 
    filter(age %in% 84:89)  %>% 
    lm(dif ~ age, data = .)  %>% 
    coefficients()  %>% .["age"]
  
  diffs_estimates  %>% select(sex, age, dif) -> simple_diffs 
  
  female_dif_89 <- simple_diffs %>% filter(sex == "female", age == 89) %>% .$dif
  male_dif_89 <- simple_diffs %>% filter(sex == "male", age == 89) %>% .$dif
  
  female_extrapolate_difs <- data_frame(
    sex = "female", age = 89:95, 
    dif = c(female_dif_89, NA, NA, NA, NA, NA, NA)
  ) 
  male_extrapolate_difs <- data_frame(
    sex = "male", age = 89:95, 
    dif = c(male_dif_89, NA, NA, NA, NA, NA, NA)
  ) 
  
  for (i in 2:7) { 
    female_extrapolate_difs$dif[i] <- female_extrapolate_difs$dif[i-1] + female_increment
    male_extrapolate_difs$dif[i] <- male_extrapolate_difs$dif[i-1] + male_increment
  }
  
  female_extrapolate_difs <- female_extrapolate_difs %>% filter(age != 89)
  male_extrapolate_difs <- male_extrapolate_difs %>% filter(age != 89)
  
  simple_diffs <- bind_rows(simple_diffs, female_extrapolate_difs, male_extrapolate_difs)  %>%
    mutate(year = YEAR) %>% 
    arrange(sex, age) %>% 
    select(sex, year, age, dif)
  
  
  simple_diffs %>% 
    ggplot(., aes(x =age, group = sex, shape = sex, linetype = sex)) +
    geom_line(aes(y = dif)) +
    scale_x_continuous(limits = c(0, 95), breaks = c(0, seq(10, 90, by = 10))) + 
    scale_y_continuous(limits = XLIMS, breaks = seq(XLIMS[1], XLIMS[2], by = 2000), labels = comma) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 65, linetype = "dashed") + 
    theme_minimal() + 
    labs(x = "Age in years", y = "Total additional deaths by\n age on x axis", title = YEAR) 
}

# Special function to extrapolate ages 90 to 95 



plot_grid(
  draw_diffs(2010, XLIMS = c(-12000, 16000)), 
  draw_diffs(2011, XLIMS = c(-12000, 16000)),
  draw_diffs(2012, XLIMS = c(-12000, 16000)),
  draw_diffs(2013, XLIMS = c(-12000, 16000)),
  draw_diffs(2014, XLIMS = c(-12000, 16000)),
  draw_diffs_extrapolate(2015, XLIMS = c(-12000, 16000)),
  nrow = 2
) -> g
print(g)

ggsave("figures/ons_only_total_excess_deaths_2010_2015_upto16k_extrapolated.png", height = 30, width = 40, dpi = 300, units = "cm")




# difference between actual and simulated deaths 

compare_actual_against_predicted <- function(dta, sim_dth){
  
  
}

model_outputs %>% 
  select(sex, age, data, sim_deaths_nl) %>% 
  mutate(dta2 = by_row(data), ~.)





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



# Coefficients?

model_outputs %>% 
  select(sex, age, mdl) %>% 
  mutate(coef_tidy = map(mdl, tidy)) %>% 
  select(-mdl) %>% 
  unnest() %>% 
  mutate(term = car::recode(
    term, 
    "
      '(Intercept)' = 'Intercept';
       'year' = 'Trend';
       'newlabTRUE' = 'NL Intercept';
       'recessionTRUE' = 'GFC Intercept'
      "
  )) %>% 
  mutate(
    term = ifelse(term == "year:newlabTRUE", "NL Trend", term),
    term = ifelse(term == "year:recessionTRUE", "GFC Trend", term)
  ) %>% 
  mutate(term = factor(term, levels = c("Intercept", "Trend", "NL Intercept", "NL Trend", "GFC Intercept", "GFC Trend"))
  ) %>% 
  mutate(upper = estimate + 1.96 * std.error, lower = estimate - 1.96 * std.error) %>% 
  ggplot(., aes(x = age)) + 
  facet_grid(term ~ sex, scales = "free_y") + 
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightgrey") + 
  geom_line(aes(y = estimate)) + 
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = c(0, seq(10, 90, by = 10))) + 
  geom_vline(xintercept = 65, linetype = "dashed")

ggsave("figures/ons_only_coefficients_with_age.png", height =30, width = 20, dpi = 300, units = "cm")

