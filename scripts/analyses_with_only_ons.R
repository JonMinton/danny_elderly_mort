# Analyses with ONS data only and simpler method


require(pacman)
pacman::p_load(
  readr, readxl, 
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
  mutate(lmr = log(deaths/population, 10)) %>% 
  mutate(
    newlab = year >= 1997 & year <= 2010, 
    thatcher = year >= 1979 & year <= 1989,
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
    ),
    model_nl_th = map(
      data, 
      function(x) {
        out <- lm(lmr ~ year * (newlab + thatcher + recession), data = x)
        return(out)
      }
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
      model_nl, dta2, 
      function(x, y){
        out <- predict(x, newdata = y, type = "response")
        return(out)
      }),
    pred_nl_norec = map2(
      model_nl, dta3, 
      function(x, y){
        out <- predict(x, newdata = y, type = "response")
        return(out)
      }),
    pred_nl_th = map2(
      model_nl_th, dta2,
      function(x, y){
        out <- predict(x, newdata = y, type = "response")
        return(out)
      }),
    pred_nl_th_norec = map2(
      model_nl_th, dta3,
      function(x, y){
        out <- predict(x, newdata = y, type = "response")
        return(out)
      })
  ) %>% 
  select(sex, age, data, pred_nl, pred_nl_norec, pred_nl_th, pred_nl_th_norec) %>% 
  unnest() %>% 
  select(sex, year, age, lmr, pred_nl, pred_nl_norec, pred_nl_th, pred_nl_th_norec) -> fitted_twoscenarios 



fitted_twoscenarios %>% 
  filter(age %in% c(0, seq(5, 95, by = 5))) %>% 
  ggplot(., aes(x = year, group = factor(age), colour = factor(age))) + 
  geom_point(aes(y = lmr)) + 
  geom_line(aes(y = pred_nl)) + 
  geom_line(aes(y = pred_nl_norec), linetype = "dashed") + 
  facet_wrap(~sex)

ggsave("figures/age_fitted_scenarios.png", height = 30, width = 25, units = "cm", dpi = 300)

fitted_twoscenarios %>% 
  filter(age %in% c(0, seq(5, 95, by = 5))) %>% 
  ggplot(., aes(x = year, group = factor(age), colour = factor(age))) + 
  geom_point(aes(y = lmr)) + 
  geom_line(aes(y = pred_nl_th)) + 
  geom_line(aes(y = pred_nl_th_norec), linetype = "dashed") + 
  facet_wrap(~sex)


# Cumulative, actual vs projected

plot_actualprojected <- function(YEAR){
  dta  %>% 
    filter(year == YEAR)  %>% 
    filter(place == "ew")  %>% 
    mutate(year = YEAR)  %>% 
    select(sex, age, population)  %>% 
    right_join(fitted_twoscenarios) %>% 
    filter(year == max(year)) %>% 
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
    scale_x_continuous(breaks = c(0, seq(10, 90, by = 10))) + 
    scale_y_continuous(limits = c(0, 250000), labels = comma) +
    theme_minimal() + 
    labs(x = "Age in years", y = "Total actual and projected mortality by age", title = YEAR) -> output
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

# As above, but log scale
plot_actualprojected_log <- function(YEAR){
  dta  %>% 
    filter(year == YEAR)  %>% 
    filter(place == "ew")  %>% 
    mutate(year = YEAR)  %>% 
    select(sex, age, population)  %>% 
    right_join(fitted_twoscenarios) %>% 
    filter(year == max(year)) %>% 
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
    scale_x_continuous(breaks = c(0, seq(10, 90, by = 10))) + 
    scale_y_log10(limits = c(500, 250000), breaks = c(1000, 2000, 5000, 10000, 20000, 50000, 100000, 200000), labels = comma) +
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
  plot_actualprojected_log(2015), 
  nrow = 2
) -> g

print(g)

ggsave("figures/ons_only_total_actual_and_projected_2010_2015.png", height = 30, width = 40, dpi = 300, units = "cm")


draw_diffs <- function(YEAR, XLIMS = c(-12000, 20000)){
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
    ggplot(., aes(x =age, group = sex, shape = sex, linetype = sex)) +
    geom_line(aes(y = dif)) +
    scale_x_continuous(limits = c(0, 95), breaks = c(0, seq(10, 90, by = 10))) + 
    scale_y_continuous(limits = XLIMS, breaks = seq(XLIMS[1], XLIMS[2], by = 2000), labels = comma) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 65, linetype = "dashed") + 
    theme_minimal() + 
    labs(x = "Age in years", y = "Total additional deaths by\n age on x axis", title = YEAR) -> output
  
  return(output)
}

draw_diffs_extrapolate <- function(YEAR, XLIMS = c(-12000, 20000)){
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
     arrange(sex, age)
   
      
    simple_diffs %>% 
    ggplot(., aes(x =age, group = sex, shape = sex, linetype = sex)) +
    geom_line(aes(y = dif)) +
    scale_x_continuous(limits = c(0, 95), breaks = c(0, seq(10, 90, by = 10))) + 
    scale_y_continuous(limits = XLIMS, breaks = seq(XLIMS[1], XLIMS[2], by = 2000), labels = comma) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 65, linetype = "dashed") + 
    theme_minimal() + 
    labs(x = "Age in years", y = "Total additional deaths by\n age on x axis", title = YEAR) -> output
  
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

