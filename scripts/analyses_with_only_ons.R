# Analyses with ONS data only and simpler method

rm(list = ls())

require(pacman)
pacman::p_load(
  Zelig,
  readr, readxl, xlsx,
  purrr, tidyr, dplyr, 
  broom,
  ggplot2, cowplot, scales

)


source("scripts/extract_combined_ons.R")




# Modelling ---------------------------------------------------------------

# What I want to do: for each variable in each model, produce 10 000 draws of distributions 

# First, get this right for one model
zelig_ops <- function(dta){
  dta <- dta %>% mutate(newlab = as.numeric(newlab), recession = as.numeric(recession))
  dta <- data.frame(dta)
  z_base <- zls$new()
  z_base$zelig(lmr ~ year * (newlab + recession), data = dta)
  z_base$setx()
  z_base$setx1(newlab = 1)
  z_base$sim(num = 10000)
  z_base
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
    model_zel = map(data, zelig_ops),
    ev_basic = map(model_zel, ~.[[1]]$sim.out$x$ev),
    ev_nl = map(model_zel, ~.[[1]]$sim.out$x1$ev)
    ) -> tmp

  mutate(
    model_nl = map(
      data, 
      function(x) { 
        out <- lm(lmr ~ year * (newlab + recession), data = x ); 
        return(out)}
    )
  ) %>% 
  mutate(coef_draws = map(
    model_nl,
    ~ mvrnorm(n= 10000, mu = coef(.), Sigma = vcov(.))
    )
  ) %>% .[["data"]] %>% .[[1]]
%>% 
  mutate(
    pred_vec 
    
  )

%>% .[["coef_draws"]] %>% .[[60]] -> some_coeffs

# A predictor vector 

pred_vec <- t(c(
  1, 0, 0, 0, 0, 0
))

pred_var %*% some_coeffs





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
    model_nl = map(
      data, 
      function(x) { 
        out <- lm(lmr ~ year * (newlab + recession), data = x ); 
        return(out)}
    )
  ) %>% 
  mutate(
    dta2 = map(
      data, 
      function(input) {
        output <- input 
        output$newlab <- TRUE
        return(output)
      }
    )
  )  %>% 
  mutate(
    pred_nl = map2(
      model_nl, dta2, 
      function(x, y){
        out <- predict(x, newdata = y, type = "response")
        return(out)
      })
  ) %>% 
  select(sex, age, data, pred_nl) %>% 
  unnest() %>% 
  select(sex, year, age, lmr, pred_nl) -> fitted_twoscenarios 



fitted_twoscenarios %>% 
  filter(age %in% seq(60, 85, by = 5)) %>% 
  mutate(age = factor(age)) %>% 
  ggplot(., aes(x = year + 1990, shape = age, group = age, colour = age)) + 
  geom_point(aes(y = lmr)) + 
  geom_line(aes(y = pred_nl)) + 
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
  geom_line(aes(y = 10^pred_nl)) + 
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


# Cumulative, actual vs projected

plot_actualprojected <- function(YEAR, XLIMS = c(0, 95), RETURN = "graph"){
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




draw_diffs <- function(YEAR, XLIMS = c(-12000, 20000), RETURN = "graph"){
  tmp <- fitted_twoscenarios %>% mutate(year = year + 1990)
  
  dta  %>% 
    filter(year == YEAR)  %>% 
    filter(place == "ew")  %>% 
    select(sex, age, population)  %>% 
    right_join(tmp) %>% 
    filter(year == YEAR) %>% 
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
    mutate(dif = cm_mrt_actual - cm_mrt_proj) %>% 
    select(sex, year, age, dif) -> simple_diffs 
    simple_diffs %>% 
    ggplot(., aes(x =age, group = sex, shape = sex, linetype = sex)) +
    geom_line(aes(y = dif)) +
    scale_x_continuous(limits = c(0, 95), breaks = c(0, seq(10, 90, by = 10))) + 
    scale_y_continuous(limits = XLIMS, breaks = seq(XLIMS[1], XLIMS[2], by = 2000), labels = comma) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 65, linetype = "dashed") + 
    theme_minimal() + 
    labs(x = "Age in years", y = "Total additional deaths by\n age on x axis", title = YEAR) -> output
  
  if (RETURN == "table") {output <- simple_diffs}
  return(output)
}

draw_diffs_extrapolate <- function(YEAR, XLIMS = c(-12000, 20000), RETURN = "graph"){
  tmp <- fitted_twoscenarios %>% mutate(year = year + 1990)
  
  dta  %>% 
    filter(year == YEAR)  %>% 
    filter(place == "ew")  %>% 
    filter(age < 90) %>% 
    select(sex, age, population)  %>% 
    right_join(tmp) %>% 
    filter(year == YEAR) %>% 
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
    mutate(dif = cm_mrt_actual - cm_mrt_proj) -> diffs 
  
    male_increment <- diffs  %>% 
      filter(sex == "male")  %>% 
      filter(age %in% 84:89)  %>% 
      lm(dif ~ age, data = .)  %>% 
      coefficients()  %>% .["age"]
      
    female_increment <- diffs  %>% 
      filter(sex == "female")  %>% 
      filter(age %in% 84:89)  %>% 
      lm(dif ~ age, data = .)  %>% 
      coefficients()  %>% .["age"]
    
    diffs  %>% select(sex, age, dif) -> simple_diffs 
    
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
    labs(x = "Age in years", y = "Total additional deaths by\n age on x axis", title = YEAR) -> output
  
    if(RETURN == "table") {output <- simple_diffs}
  return(output)
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

# For the numbers themselves 

draw_diffs(2010, RETURN = "table") -> d_10
draw_diffs(2011, RETURN = "table") -> d_11
draw_diffs(2012, RETURN = "table") -> d_12
draw_diffs(2013, RETURN = "table") -> d_13
draw_diffs(2014, RETURN = "table") -> d_14
draw_diffs_extrapolate(2015, RETURN = "table") -> d_15

d_all <- Reduce(bind_rows, list(d_10, d_11, d_12, d_13, d_14, d_15))

d_all %>% 
  filter(!is.na(dif)) %>% 
  mutate(dif = round(dif, 0)) %>% 
  spread(year, dif) -> spread_diffs

sheet_differences <- createSheet(wb, sheetName = "differences_by_age_year")
addDataFrame(spread_diffs, sheet_differences)



# Coefficients?


# What about the coefficients?

dta  %>% 
  filter(year >= 1990) %>% 
  filter(age <= 95)  %>% 
  mutate(lmr = log(deaths / population, 10)) %>% 
  mutate(
    newlab = year >= 1997 & year <= 2010, 
    recession = year %in% 2008:2009
  )  %>% 
  mutate(year = year - 1990) %>% 
  group_by(sex, age)  %>% 
  nest()  %>% 
  mutate(
    model = map(
      data, 
      function(x) { 
        out <- lm(lmr ~ year * (newlab + recession), data = x ); 
        return(out)}
    )
  )  %>% 
  mutate(m2 = map(model, tidy))  %>% 
  select(-data, -model)   %>% 
  unnest() -> all_coeffs



sheet_coeffs <- createSheet(wb, sheetName = "coefficients")
addDataFrame(all_coeffs, sheet_coeffs)

  all_coeffs %>%   
  mutate(est_lwr = estimate - 2 * std.error, est_upr = estimate + 2 * std.error)  %>% 
  ggplot(., aes(x = age)) + 
  facet_grid(term ~ sex, scales = "free_y") + 
  geom_ribbon(aes(ymin = est_lwr, ymax = est_upr), fill = "lightgrey") + 
  geom_line(aes(y = estimate)) + 
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = c(0, seq(10, 90, by = 10))) + 
  geom_vline(xintercept = 65, linetype = "dashed")
  


ggsave("figures/ons_only_coefficients_with_age.png", height =30, width = 20, dpi = 300, units = "cm")


# Summary statistics only?
dta  %>% 
  filter(year >= 1990) %>% 
  filter(age <= 95)  %>% 
  mutate(lmr = log(deaths / population, 10)) %>% 
  mutate(
    newlab = year >= 1997 & year <= 2010, 
    recession = year %in% 2008:2009
  )  %>% 
  mutate(year = year - 1990) %>% 
  group_by(sex, age)  %>% 
  nest()  %>% 
  mutate(
    model = map(
      data, 
      function(x) { 
        out <- lm(lmr ~ year * (newlab + recession), data = x ); 
        return(out)}
    )
  )  %>% 
  mutate(m2 = map(model, glance))  %>% 
  select(-data, -model)   %>% 
  unnest() -> all_summaries

sheet_summaries <- createSheet(wb, sheetName = "model_summaries")
addDataFrame(all_summaries, sheet_summaries)

all_summaries %>% 
  ggplot(., aes(x = age, y = r.squared, group = sex, shape = sex, colour = sex)) + 
  geom_point() + 
  stat_smooth(aes(linetype = sex), method = "loess", span = 0.25, se = F) + 
  scale_x_continuous(limits = c(0, 95), breaks = c(0, seq(10, 90, by = 10))) + 
  geom_vline(xintercept = 65, linetype = "dashed") + 
  geom_hline(yintercept =1 ) + 
  labs(x = "Age in years", y = "R-squared of model fit") + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, seq(0.1, 1, by = 0.1))) + 
  theme_minimal()



ggsave("figures/regressions_by_age.png", height =15, width = 15, dpi = 300, units = "cm")


saveWorkbook(wb, "tables/all_supplementary_results.xlsx")
