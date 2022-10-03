# PROJECT:  Last Mile Analytics
# AUTHOR:   J.Hoehner | USAID/PHI
# PURPOSE:  Exploration of KP Atlas data
# REF ID:   d1836592
# LICENSE:  MIT
# DATE CREATED: 2022-07-21
# DATE UPDATED: 2022-10-03

# dependencies -----------------------------------------------------------------

library(glamr)
library(glitr)
library(readr)
library(janitor)
library(tidyverse)
library(stringr)
library(forcats)
library(readxl)
library(googlesheets4)
library(extrafont)
library(gophr)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(lubridate)

load_secrets()

# global variables -------------------------------------------------------------

ref_id <- "d1836592"
options(scipen = 999)

# inputs and outputs -----------------------------------------------------------

outputs <- list(
kp_findings = "lastmile/Scripts/global_story/outputs/kp_findings.csv")
ymax = "2021"
goal = 95

populate_sparse_df_notPLHIV <- function(df, indicator){
  
  complete_totals <- df %>%
    filter(indicator_new == indicator) %>%
    select(time_period, area, population, data_value) %>%
    distinct() %>%
    group_by(time_period, area, population) %>%
    select(time_period, area, population) %>%
    distinct()

  complete_totals_tab <- tabyl(complete_totals, 
                               time_period, area, population)
  
  long_by_ou_msm <- pivot_longer(
    as.data.frame(complete_totals_tab[["men who have sex with men"]]), 
    !time_period,
    names_to = "OU", 
    values_to = "n_ests") %>%
    mutate(population = "MSM") %>%
    filter(time_period %in% c("2016", "2017", "2018", 
                              "2019", "2020"),
          OU %in% complete_totals$area) %>%
    group_by(OU, population) %>%
    summarize(n_ests = sum(n_ests)) %>%
    # does the OU have at least 1 estimate in the previous 5 years?
    mutate(has_est = as.numeric(
      if_else(as.numeric(n_ests) > 0, TRUE,FALSE))) %>%
    select(OU, population, has_est)

  long_by_ou_pwid <- pivot_longer(
    as.data.frame(complete_totals_tab[["people who inject drugs"]]), 
    !time_period,
    names_to = "OU", 
    values_to = "n_ests") %>%
    mutate(population = "PWID") %>%
    filter(time_period %in% c("2016", "2017", "2018", 
                              "2019", "2020"),
           OU %in% complete_totals$area)%>%
    group_by(OU, population) %>%
    summarize(n_ests = sum(n_ests)) %>%
    # does the OU have at least 1 estimate in the previous 5 years?
    mutate(has_est = as.numeric(
      if_else(as.numeric(n_ests) > 0, TRUE,FALSE))) %>%
    select(OU, population, has_est)
  
  long_by_ou_prisoners <- pivot_longer(
    as.data.frame(complete_totals_tab[["prisoners"]]), 
    !time_period,
    names_to = "OU", 
    values_to = "n_ests") %>%
    mutate(population = "Prisoners") %>%
    filter(time_period %in% c("2016", "2017", "2018", 
                              "2019", "2020"),
           OU %in% complete_totals$area)%>%
    group_by(OU, population) %>%
    summarize(n_ests = sum(n_ests)) %>%
    # does the OU have at least 1 estimate in the previous 5 years?
    mutate(has_est = as.numeric(
      if_else(as.numeric(n_ests) > 0, TRUE,FALSE))) %>%
    select(OU, population, has_est)
  
  long_by_ou_sw <- pivot_longer(
    as.data.frame(complete_totals_tab[["sex workers"]]), 
    !time_period,
    names_to = "OU", 
    values_to = "n_ests") %>%
    mutate(population = "SW") %>%
    filter(time_period %in% c("2016", "2017", "2018", 
                              "2019", "2020"),
           OU %in% complete_totals$area)%>%
    group_by(OU, population) %>%
    summarize(n_ests = sum(n_ests)) %>%
    # does the OU have at least 1 estimate in the previous 5 years?
    mutate(has_est = as.numeric(
      if_else(as.numeric(n_ests) > 0, TRUE,FALSE))) %>%
    select(OU, population, has_est)
  
  long_by_ou_tp <- pivot_longer(
    as.data.frame(complete_totals_tab[["transgender people"]]), 
    !time_period,
    names_to = "OU", 
    values_to = "n_ests") %>%
    mutate(population = "TP") %>%
    filter(time_period %in% c("2016", "2017", "2018", 
                              "2019", "2020"),
           OU %in% complete_totals$area)%>%
    group_by(OU, population) %>%
    summarize(n_ests = sum(n_ests)) %>%
    # does the OU have at least 1 estimate in the previous 5 years?
    mutate(has_est = as.numeric(
      if_else(as.numeric(n_ests) > 0, TRUE,FALSE))) %>%
    select(OU, population, has_est)
  
  full_sparse_df <- long_by_ou_msm %>%
    full_join(., long_by_ou_pwid, 
              by = c("OU",
                     "population", 
                     "has_est")) %>%
    full_join(., long_by_ou_prisoners, 
              by = c("OU",
                     "population", 
                     "has_est")) %>%
    full_join(., long_by_ou_sw, 
              by = c("OU",
                     "population", 
                     "has_est")) %>%
    full_join(., long_by_ou_tp, 
              by = c("OU",
                     "population", 
                     "has_est")) %>%

  return(full_sparse_df)
}

populate_sparse_df_notPLHIV_pnr <- function(df, indicator){
  
  complete_totals <- df %>%
    filter(indicator_new == indicator) %>%
    select(time_period, area, population, data_value) %>%
    distinct() %>%
    group_by(time_period, area, population) %>%
    select(time_period, area, population) %>%
    distinct()
  
  complete_totals_tab <- tabyl(complete_totals, 
                               time_period, area, population)
  long_by_ou_msm <- pivot_longer(
    as.data.frame(complete_totals_tab[["men who have sex with men"]]), 
    !time_period,
    names_to = "OU", 
    values_to = "n_ests") %>%
    mutate(population = "MSM") %>%
    filter(time_period %in% c("2016", "2017", "2018", 
                              "2019", "2020"),
           OU %in% complete_totals$area)%>%
    group_by(OU, population) %>%
    summarize(n_ests = sum(n_ests)) %>%
    # does the OU have at least 1 estimate in the previous 5 years?
    mutate(has_est = as.numeric(
      if_else(as.numeric(n_ests) > 0, TRUE,FALSE))) %>%
    select(OU, population, has_est)
  
  long_by_ou_pwid <- pivot_longer(
    as.data.frame(complete_totals_tab[["people who inject drugs"]]), 
    !time_period,
    names_to = "OU", 
    values_to = "n_ests") %>%
    mutate(population = "PWID") %>%
    filter(time_period %in% c("2016", "2017", "2018", 
                              "2019", "2020"),
           OU %in% complete_totals$area)%>%
    group_by(OU, population) %>%
    summarize(n_ests = sum(n_ests)) %>%
    # does the OU have at least 1 estimate in the previous 5 years?
    mutate(has_est = as.numeric(
      if_else(as.numeric(n_ests) > 0, TRUE,FALSE))) %>%
    select(OU, population, has_est)
  
  long_by_ou_sw <- pivot_longer(
    as.data.frame(complete_totals_tab[["sex workers"]]), 
    !time_period,
    names_to = "OU", 
    values_to = "n_ests") %>%
    mutate(population = "SW") %>%
    filter(time_period %in% c("2016", "2017", "2018", 
                              "2019", "2020"),
           OU %in% complete_totals$area)%>%
    group_by(OU, population) %>%
    summarize(n_ests = sum(n_ests)) %>%
    # does the OU have at least 1 estimate in the previous 5 years?
    mutate(has_est = as.numeric(
      if_else(as.numeric(n_ests) > 0, TRUE,FALSE))) %>%
    select(OU, population, has_est)
  
  long_by_ou_tp <- pivot_longer(
    as.data.frame(complete_totals_tab[["transgender people"]]), 
    !time_period,
    names_to = "OU", 
    values_to = "n_ests") %>%
    mutate(population = "TP") %>%
    filter(time_period %in% c("2016", "2017", "2018", 
                              "2019", "2020"),
           OU %in% complete_totals$area)%>%
    group_by(OU, population) %>%
    summarize(n_ests = sum(n_ests)) %>%
    # does the OU have at least 1 estimate in the previous 5 years?
    mutate(has_est = as.numeric(
      if_else(as.numeric(n_ests) > 0, TRUE,FALSE))) %>%
    select(OU, population, has_est)
  
  full_sparse_df <- long_by_ou_msm %>%
    full_join(., long_by_ou_pwid, 
              by = c("OU",
                     "population", 
                     "has_est")) %>%
    full_join(., long_by_ou_sw, 
              by = c("OU",
                     "population", 
                     "has_est")) %>%
    full_join(., long_by_ou_tp, 
              by = c("OU",
                     "population", 
                     "has_est")) %>%
  
  return(full_sparse_df)
}

populate_sparse_df_PLHIV <- function(df, indicator){
  
  complete_totals <- df %>%
    filter(indicator_new == indicator) %>%
    select(time_period, area, population, data_value) %>%
    distinct() %>%
    group_by(time_period, area, population) %>%
    select(time_period, area, population) %>%
    distinct()
  
  complete_totals_tab <- tabyl(complete_totals, 
                               time_period, area, population)
  long_by_ou_msm <- pivot_longer(
    as.data.frame(complete_totals_tab[["men who have sex with men living with HIV"]]), 
    !time_period,
    names_to = "OU", 
    values_to = "n_ests") %>%
    mutate(population = "MSM") %>%
    filter(time_period %in% c("2016", "2017", "2018", 
                              "2019", "2020"),
           OU %in% complete_totals$area)%>%
    group_by(OU, population) %>%
    summarize(n_ests = sum(n_ests)) %>%
    # does the OU have at least 1 estimate in the previous 5 years?
    mutate(has_est = as.numeric(
      if_else(as.numeric(n_ests) > 0, TRUE,FALSE))) %>%
    select(OU, population, has_est)
  
  long_by_ou_pwid <- pivot_longer(
    as.data.frame(complete_totals_tab[["people who inject drugs"]]), 
    !time_period,
    names_to = "OU", 
    values_to = "n_ests") %>%
    mutate(population = "PWID") %>%
    filter(time_period %in% c("2016", "2017", "2018", 
                              "2019", "2020"),
           OU %in% complete_totals$area)%>%
    group_by(OU, population) %>%
    summarize(n_ests = sum(n_ests)) %>%
    # does the OU have at least 1 estimate in the previous 5 years?
    mutate(has_est = as.numeric(
      if_else(as.numeric(n_ests) > 0, TRUE,FALSE))) %>%
    select(OU, population, has_est)
  
  long_by_ou_prisoners <- pivot_longer(
    as.data.frame(complete_totals_tab[["prisoners living with HIV"]]), 
    !time_period,
    names_to = "OU", 
    values_to = "n_ests") %>%
    mutate(population = "Prisoners") %>%
    filter(time_period %in% c("2016", "2017", "2018", 
                              "2019", "2020"),
           OU %in% complete_totals$area)%>%
    group_by(OU, population) %>%
    summarize(n_ests = sum(n_ests)) %>%
    # does the OU have at least 1 estimate in the previous 5 years?
    mutate(has_est = as.numeric(
      if_else(as.numeric(n_ests) > 0, TRUE,FALSE))) %>%
    select(OU, population, has_est)
  
  long_by_ou_sw <- pivot_longer(
    as.data.frame(complete_totals_tab[["sex workers"]]), 
    !time_period,
    names_to = "OU", 
    values_to = "n_ests") %>%
    mutate(population = "SW") %>%
    filter(time_period %in% c("2016", "2017", "2018", 
                              "2019", "2020"),
           OU %in% complete_totals$area)%>%
    group_by(OU, population) %>%
    summarize(n_ests = sum(n_ests)) %>%
    # does the OU have at least 1 estimate in the previous 5 years?
    mutate(has_est = as.numeric(
      if_else(as.numeric(n_ests) > 0, TRUE,FALSE))) %>%
    select(OU, population, has_est)
  
  long_by_ou_tp <- pivot_longer(
    as.data.frame(complete_totals_tab[["transgender people"]]), 
    !time_period,
    names_to = "OU", 
    values_to = "n_ests") %>%
    mutate(population = "TP") %>%
    filter(time_period %in% c("2016", "2017", "2018", 
                              "2019", "2020"),
           OU %in% complete_totals$area)%>%
    group_by(OU, population) %>%
    summarize(n_ests = sum(n_ests)) %>%
    # does the OU have at least 1 estimate in the previous 5 years?
    mutate(has_est = as.numeric(
      if_else(as.numeric(n_ests) > 0, TRUE,FALSE))) %>%
    select(OU, population, has_est)
  
  full_sparse_df <- long_by_ou_msm %>%
    full_join(., long_by_ou_pwid, 
              by = c("OU",
                     "population", 
                     "has_est")) %>%
    full_join(., long_by_ou_prisoners, 
              by = c("OU",
                     "population", 
                     "has_est")) %>%
    full_join(., long_by_ou_sw, 
              by = c("OU",
                     "population", 
                     "has_est")) %>%
    full_join(., long_by_ou_tp, 
              by = c("OU",
                     "population", 
                     "has_est")) %>%
  
  return(full_sparse_df)
}

missing_data_heatmap <- function(df, title){
  
  sparse_df_pse %>%
    group_by(OU, population) %>%
    mutate(sum = sum(has_est)) %>%
    ggplot(aes(fct_reorder(population, sum, .desc = TRUE), 
               fct_reorder(OU, sum))) +
    geom_tile(aes(fill = has_est), color = usaid_lightgrey, alpha = .4, 
              show.legend = FALSE) + 
    scale_fill_gradient(position = "top") +
    viridis::scale_fill_viridis(option = "D") +
    theme(legend.position = "none") +
    labs(x = NULL, y = NULL, fill = NULL,
         color = NULL,
         fill = NULL,
         title = title %>% toupper,
         caption = glue("J. Hoehner, SI Analytics | UNAIDS KP Atlas Database 2021 | {ref_id} "),
         subtitle = "A yellow box indicates an available estimate from the previous 5 years 
         (2016 - 2020) while a purple box indicates no estimate is available for the previous 5 years") +
    si_style_nolines() +
    theme(panel.spacing = unit(.4, "picas"),
          strip.placement = "outside",
          strip.text.y = element_blank(),
          axis.text = element_text(size = 8))
  
}

# munge ------------------------------------------------------------------------

# read in indicator crosswalk data to further tidy data used
indicator_crosswalk <- read_sheet("1iv5aBHXSqO2Ky4d6zEORl2KwL_7_08w_D0CSTKgoq0A") %>%
  clean_names()

# pepfar countries 

pepfar_countries <- pepfar_country_list

# - KP data
kp_data <- read_sheet("1KORmN23RjAKDz9yroJA5yn9csBOGBKDHKmK45JIs7tY")
  
kp_tidier <- kp_data %>%
  clean_names() %>%
  select(!starts_with("x")) %>%
  mutate(
    across(.cols = indicator:source, ~ as_factor(.))) %>%
  filter(indicator %in% indicator_crosswalk$indicator, 
         area_id %in% pepfar_countries$country_iso) %>%
  # join cols by indicator with crosswalk file to separate population and 
  # indicator
  left_join(., indicator_crosswalk, 
            by = c("indicator")) %>%
  mutate(
    # fix labeling in MSM, for some reason "Total" is called "All ages"
    # only for this KP, I confirmed this by looking at COD, HIV prev. in MSM in 2019,
    # the estimate reported as "Total" was the same as the estimate reported in this
    # data as "All ages"
    subgroup = as_factor(if_else(population == "men who have sex with men" 
                                    & subgroup == "All ages", 
                                    "Total",
                                    as.character(subgroup))),
    # create a new col to keep track of sources of data
    source = str_c(source.x, source.y, sep = "|"), 
    # create a new col to tell if area provided is National or SubNational
    geo_level = as.character(
      if_else(str_length(area_id) == 3, 
              "National", "Subnational"))) %>%
  filter(geo_level == "National",
         time_period %in% c("2016", "2017", "2018","2019", "2020"),
         indicator_new %in% c("Population Size Estimate", 
                              "Coverage of HIV prevention programmes",
                              "Antiretroviral therapy coverage",
                              "HIV testing and status awareness",
                              "HIV prevalence")) %>%
  mutate(data_value = as.numeric(data_value)) %>%
  # rearrange the columns, keep only cols of interest
  select(bucket, indicator_new, population, subgroup, 
         geo_level, area, area_id, data_value, unit, 
         time_period, source)

write_sheet(kp_tidier, "1Jxj2PlJKSr_LrU2uNs-DBtl-6hwmzaha6Gg6wDQuAxw", 
            "kp_tidier")

# population size estimate
sparse_df_pse <- populate_sparse_df_notPLHIV(df = kp_tidier, 
                                             indicator = "Population Size Estimate")

# EDA --------------------------------------------------------------------------

# of the population size ests and perecent coverage indicators, 
# which OUs have complete data for all KPs for 2020? 
# focusing especially on PEPFAR supported OUs

# How many unique national OUs are in the data?
unique_ous <- kp_tidier %>%
  select(area, area_id) %>%
  distinct() 

# Which ous have complete PSE data for all KPs?
complete_totals_2020_pse <- kp_tidier %>%
  filter(time_period == "2020",
         indicator_new == "Population Size Estimate")
  
ou_table_2020_pse <- as.data.frame(tabyl(complete_totals_2020_pse, area, population)) %>%
  pivot_longer(!area, names_to = "population", values_to = "n_estimates") %>%
  filter(n_estimates > 0) %>%
  group_by(area) %>%
  summarize(n_kps = sum(n_estimates))

# which OUS have complete coverage data for all KPs in 2020?
complete_totals_2020_cov <- complete_totals_2020 %>%
  filter(time_period == "2020",
         indicator_new == "Coverage of HIV prevention programmes")

ou_table_2020_cov <- as.data.frame(tabyl(complete_totals_2020_pse, area, population)) %>%
  pivot_longer(!area, names_to = "population", values_to = "n_estimates") %>%
  filter(n_estimates > 0) %>%
  group_by(area) %>%
  summarize(n_kps = sum(n_estimates))

# KP figures ----------------------------------------------------------------

# population size estimate
sparse_df_pse <- populate_sparse_df_notPLHIV(df = kp_tidier, 
                                             indicator = "Population Size Estimate")
sparse_df_pse %>% 
  missing_data_heatmap(., 
                       "How Many Poulation Size Estimates Exist From PEPFAR supported OUs
 with available KP data in the previous 5 years?")

si_save(glue("thathill/Images/KPAtlasfindings_PopulationSizeEst_{Sys.Date()}.svg"),
        height = 9, width = 16)

# HIV prevalence
sparse_df_hivprev <- populate_sparse_df_notPLHIV(df = kp_tidier, 
                                                 indicator = "HIV prevalence")

sparse_df_hivprev %>% 
  missing_data_heatmap(., 
                       "How Many HIV Prevalence Estimates Exist From PEPFAR supported OUs
with available KP data in the previous 5 years?")

#export
si_save(glue("Images/KPAtlasfindings_HIV prevalence_{Sys.Date()}.png"),
        height = 10, width = 10)

# Antiretroviral therapy coverage
sparse_df_artcov <- populate_sparse_df_PLHIV(df = kp_tidier, 
                                             indicator = "Antiretroviral therapy coverage")
sparse_df_artcov %>% 
  missing_data_heatmap(., 
                       "How Many Antiretroviral therapy coverage Estimates Exist 
From PEPFAR supported OUs with available KP data in the previous 5 years?")

#export
si_save(glue("Images/KPAtlasfindings_ARTcoverage_{Sys.Date()}.png"),
        height = 10, width = 10)

# coverage of HIV prevention programs
sparse_df_hivcov <- populate_sparse_df_notPLHIV_pnr(df = kp_tidier, 
                                                    indicator = "Coverage of HIV prevention programmes")

sparse_df_hivcov %>% 
  missing_data_heatmap(., 
                       "How Many Estimates Exist for Coverage of HIV prevention programmes
From PEPFAR supported OUs with available KP data in the previous 5 years?")

#export
si_save(glue("Images/KPAtlasfindings_HIVprogcoverage_{Sys.Date()}.png"),
        height = 10, width = 10)

# HIV testing and status awareness

sparse_df_1st90 <- populate_sparse_df_notPLHIV_pnr(df = kp_tidier, 
                                                   indicator = "HIV testing and status awareness")
sparse_df_1st90 %>% 
  missing_data_heatmap(., 
                       "How Many HIV testing and status awareness Estimates Exist 
From PEPFAR supported OUs with available KP data in the previous 5 years?")

#export
si_save(glue("Images/KPAtlasfindings_1st90_{Sys.Date()}.png"),
        height = 10, width = 10)
  
# What if we look at prevention in the UNAIDS data? ----------------------------
unaids_prev <- read_sheet("1yrgS_ZbA3Q8diICkQnl9CdF-77hF8PKuQ9KGP7r6vGg")

unaids_prev_all <- unaids_prev %>%
  clean_names() %>%
  filter(age == "all",
         sex == "all",
         pepfar == "TRUE") %>%
  group_by(country, year, indicator)

# How has prevalence changed over time in OUs with highest prev? 

highest_prev_p_ous <- c("Botswana","Lesotho",
                        "Namibia","South Africa", 
                        "Zimbabwe","Eswatini","Zambia", 
                        "Nigeria", "Ethiopia") 

highest_inc_ous <- c("Botswana","Lesotho",
                     "Namibia","South Africa", 
                     "Zimbabwe","Eswatini","Zambia", 
                     "Nigeria", "Ethiopia") 

highest_inc_ous <- unaids_prev_all %>%
  filter(year == 2021, 
         indicator == "Incidence Per 1000") %>%
  arrange(desc(estimate))

unaids_prev_wide <- unaids_prev_all %>% 
  filter(country %in% highest_prev_ous | 
           country %in% highest_inc_ous) %>%
  pivot_wider(names_from = indicator,
              names_glue ="{indicator}_{.value}",
              values_from = c(estimate, lower_bound, upper_bound)) %>%
  clean_names() %>%
  mutate(
    across(starts_with(c("adult_hiv_prevalence_percent_", 
                         "incidence_per_1000_")), 
           ~as.numeric(.)), 
    ou_order_val_prev = case_when(year == max(year) ~ 
                                    adult_hiv_prevalence_percent_estimate),
    ou_order_val_inc = case_when(year == max(year) ~ 
                                   adult_hiv_prevalence_percent_estimate))

unaids_prev_wide %>%
  ggplot(aes(x = year)) +
  geom_ribbon(aes(ymax = adult_hiv_prevalence_percent_lower_bound, 
                  ymin = adult_hiv_prevalence_percent_upper_bound), 
              fill = denim_light, alpha = 1) +
  geom_line(aes(y = adult_hiv_prevalence_percent_estimate), 
            color = denim, 
            size = 1) +
  geom_area(aes(y = adult_hiv_prevalence_percent_lower_bound),
            alpha = 0.1, 
            fill = denim_light) +
  geom_vline(xintercept = 2003, 
             colour= denim, 
             linetype = "longdash", alpha = 0.5) +
  scale_x_continuous(breaks = seq(1990, 2020, 5)) +
  scale_y_continuous(limits = c(0, 40),
                     breaks = seq(0, 40, 10)) +
  facet_wrap(~ reorder(country, ou_order_val_prev), 
             ncol = 3) +
  si_style_ygrid() +
  theme(legend.position = "none") +
  labs( 
    x = NULL,
    y = NULL,
    fill = NULL, 
    title = "HIV PREVALENCE (%) OVER TIME",
    subtitle = "Nigeria and Ethiopia shown for comparison to OUs with >=10% HIV prevalence reported in 2021",
    caption = glue::glue("Source: UNAIDS 2022 | {ref_id}"))

si_save("thathill/Graphics/prevention_hivprev.svg")

# How has incidence per 100k changed over time in OUs with highest rates?

unaids_prev_wide %>%
  ggplot(aes(x = year)) +
  geom_ribbon(aes(ymax = incidence_per_1000_lower_bound, 
                  ymin = incidence_per_1000_upper_bound), 
              fill = denim_light, alpha = 1) +
  geom_line(aes(y = incidence_per_1000_estimate), 
            color = denim, 
            size = 1) +
  geom_area(aes(y = incidence_per_1000_lower_bound),
            alpha = 0.1, 
            fill = denim_light) +
  geom_vline(xintercept = 2003, 
             colour= denim, 
             linetype = "longdash", alpha = 0.5) +
  scale_x_continuous(breaks = seq(1990, 2020, 5)) +
  scale_y_continuous(limits = c(0, 40),
                     breaks = seq(0, 40, 10)) +
  facet_wrap(~ reorder(country, ou_order_val_inc), 
             ncol = 3) +
  si_style_ygrid() +
  theme(legend.position = "none") +
  labs( 
    x = NULL,
    y = NULL,
    fill = NULL, 
    title = "HIV INCIDENCE PER 1000 PEOPLE OVER TIME",
    subtitle = "Nigeria and Ethiopia shown for comparison to OUs with HIV incidence per 1000 people > 1.5 reported in 2021",
    caption = glue::glue("Source: UNAIDS 2022 | {ref_id}"))

si_save("thathill/Graphics/prevention_hivincper1000.svg")

# are there any years in which there are any OUs which have complete data for all 
# kps?
# heatmap of kp data by ous and year

