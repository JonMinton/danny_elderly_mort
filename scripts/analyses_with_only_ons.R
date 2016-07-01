# Analyses with ONS data only and simpler method


require(pacman)
pacman::p_load(
  readr, readxl, xlsx,
  purrr, tidyr, dplyr, 
  broom,
  ggplot2, cowplot, scales
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

dta <- dta %>% mutate(year = as.numeric(year))

# Now to start to add new data from ons 

ew_new_2015 <- read_csv(
  "workbook/ukmye2015/MYEB2_detailed_components_of_change_series_EW_(0215).csv"
)

ew_new_2015  %>% 
  select(lad2014_code, sex, age, population = population_2015, deaths = deaths_2015)  %>% 
  group_by(sex, age)  %>% 
  summarise(deaths = sum(deaths), population = sum(population) ) %>% 
  mutate(year = 2015)  %>%
  mutate(place = "ew") %>% 
  select(sex, place, year, age, deaths, population)  %>% 
  ungroup()  %>% 
  mutate(sex = ifelse(sex == 1, "male", ifelse(sex == 2, "female", NA))) -> dta_2015

dta <- bind_rows(dta, dta_2015)

dta  %>% 
  filter(age <= 95)  %>% 
  filter(year >= 1990)  %>% 
  filter(place == "ew") %>% 
  mutate(lmr = log(deaths/population, 10)) %>% 
  mutate(
    newlab = year >= 1997 & year <= 2010, 
    recession = year %in% 2008:2009
  )  %>% 
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
  ggplot(., aes(x = year, shape = age, group = age, colour = age)) + 
  geom_point(aes(y = lmr)) + 
  geom_line(aes(y = pred_nl)) + 
  facet_wrap(~sex) + 
  labs(x = "Year", y = "Base 10 Log mortality risk at age") 

ggsave("figures/age_fitted_scenarios.png", height = 30, width = 25, units = "cm", dpi = 300)

# As above but actual not log

fitted_twoscenarios %>% 
  filter(age %in% seq(60, 85, by = 5)) %>% 
  mutate(age = factor(age)) %>% 
  ggplot(., aes(x = year, shape = age, group = age, colour = age)) + 
  geom_point(aes(y = 10^lmr)) + 
  geom_line(aes(y = 10^pred_nl)) + 
  facet_wrap(~sex) + 
  labs(x = "Year", y = "Mortality risk at age") 

ggsave("figures/age_fitted_scenarios_identity.png", height = 30, width = 25, units = "cm", dpi = 300)


# Cumulative, actual vs projected

plot_actualprojected <- function(YEAR, XLIMS = c(0, 95), RETURN = "graph"){
  tmp <- fitted_twoscenarios %>% 
    filter(year == YEAR) %>% 
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

write.xlsx(t_all, "tables/tables.xlsx", "cumulative_projected_actual_mort")
# As above, but log scale
plot_actualprojected_log <- function(YEAR, XLIMS = c(0, 95), XFOCUS = c(0, 95), YFOCUS = c(500, 250000)){
  tmp <- fitted_twoscenarios %>% 
    filter(year == YEAR) %>% 
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
  dta  %>% 
    filter(year == YEAR)  %>% 
    filter(place == "ew")  %>% 
    select(sex, age, population)  %>% 
    right_join(fitted_twoscenarios) %>% 
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
  dta  %>% 
    filter(year == YEAR)  %>% 
    filter(place == "ew")  %>% 
    filter(age < 90) %>% 
    select(sex, age, population)  %>% 
    right_join(fitted_twoscenarios) %>% 
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




# Coefficients?


# What about the coefficients?

dta  %>% 
  filter(age <= 95)  %>% 
  mutate(lmr = log(deaths / population, 10)) %>% 
  mutate(
    newlab = year >= 1997 & year <= 2010, 
    recession = year %in% 2008:2009
  )  %>% 
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
  unnest()  %>% 
  mutate(est_lwr = estimate - 2 * std.error, est_upr = estimate + 2 * std.error)  %>% 
  ggplot(., aes(x = age)) + 
  facet_grid(term ~ sex, scales = "free_y") + 
  geom_ribbon(aes(ymin = est_lwr, ymax = est_upr), fill = "lightgrey") + 
  geom_line(aes(y = estimate)) + 
  geom_hline(yintercept = 0)


ggsave("figures/ons_only_coefficients_with_age.png", height =30, width = 20, dpi = 300, units = "cm")

