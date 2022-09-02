# PROJECT:  Last Mile Analytics
# AUTHOR:   J.Hoehner | USAID/PHI
# PURPOSE:  Exploration of KP Atlas data
# REF ID:   d1836592
# LICENSE:  MIT
# DATE CREATED: 2022-07-21
# DATE UPDATED: 2022-09-02

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
          OU %in% complete_totals$area)
  
  long_by_ou_pwid <- pivot_longer(
    as.data.frame(complete_totals_tab[["people who inject drugs"]]), 
    !time_period,
    names_to = "OU", 
    values_to = "n_ests") %>%
    mutate(population = "PWID") %>%
    filter(time_period %in% c("2016", "2017", "2018", 
                              "2019", "2020"),
           OU %in% complete_totals$area)
  
  long_by_ou_prisoners <- pivot_longer(
    as.data.frame(complete_totals_tab[["prisoners"]]), 
    !time_period,
    names_to = "OU", 
    values_to = "n_ests") %>%
    mutate(population = "Prisoners") %>%
    filter(time_period %in% c("2016", "2017", "2018", 
                              "2019", "2020"),
           OU %in% complete_totals$area)
  
  long_by_ou_sw <- pivot_longer(
    as.data.frame(complete_totals_tab[["sex workers"]]), 
    !time_period,
    names_to = "OU", 
    values_to = "n_ests") %>%
    mutate(population = "SW") %>%
    filter(time_period %in% c("2016", "2017", "2018", 
                              "2019", "2020"),
           OU %in% complete_totals$area)
  
  long_by_ou_tp <- pivot_longer(
    as.data.frame(complete_totals_tab[["transgender people"]]), 
    !time_period,
    names_to = "OU", 
    values_to = "n_ests") %>%
    mutate(population = "TP") %>%
    filter(time_period %in% c("2016", "2017", "2018", 
                              "2019", "2020"),
           OU %in% complete_totals$area)
  
  full_sparse_df <- long_by_ou_msm %>%
    full_join(., long_by_ou_pwid, 
              by = c("time_period", 
                     "OU",
                     "population", 
                     "n_ests")) %>%
    full_join(., long_by_ou_prisoners, 
              by = c("time_period", 
                     "OU",
                     "population", 
                     "n_ests")) %>%
    full_join(., long_by_ou_sw, 
              by = c("time_period", 
                     "OU",
                     "population", 
                     "n_ests")) %>%
    full_join(., long_by_ou_tp, 
              by = c("time_period", 
                     "OU",
                     "population", 
                     "n_ests"))

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
           OU %in% complete_totals$area)
  
  long_by_ou_pwid <- pivot_longer(
    as.data.frame(complete_totals_tab[["people who inject drugs"]]), 
    !time_period,
    names_to = "OU", 
    values_to = "n_ests") %>%
    mutate(population = "PWID") %>%
    filter(time_period %in% c("2016", "2017", "2018", 
                              "2019", "2020"),
           OU %in% complete_totals$area)
  
  long_by_ou_sw <- pivot_longer(
    as.data.frame(complete_totals_tab[["sex workers"]]), 
    !time_period,
    names_to = "OU", 
    values_to = "n_ests") %>%
    mutate(population = "SW") %>%
    filter(time_period %in% c("2016", "2017", "2018", 
                              "2019", "2020"),
           OU %in% complete_totals$area)
  
  long_by_ou_tp <- pivot_longer(
    as.data.frame(complete_totals_tab[["transgender people"]]), 
    !time_period,
    names_to = "OU", 
    values_to = "n_ests") %>%
    mutate(population = "TP") %>%
    filter(time_period %in% c("2016", "2017", "2018", 
                              "2019", "2020"),
           OU %in% complete_totals$area)
  
  full_sparse_df <- long_by_ou_msm %>%
    full_join(., long_by_ou_pwid, 
              by = c("time_period", 
                     "OU",
                     "population", 
                     "n_ests")) %>%
    full_join(., long_by_ou_sw, 
              by = c("time_period", 
                     "OU",
                     "population", 
                     "n_ests")) %>%
    full_join(., long_by_ou_tp, 
              by = c("time_period", 
                     "OU",
                     "population", 
                     "n_ests"))
  
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
           OU %in% complete_totals$area)
  
  long_by_ou_pwid <- pivot_longer(
    as.data.frame(complete_totals_tab[["people who inject drugs"]]), 
    !time_period,
    names_to = "OU", 
    values_to = "n_ests") %>%
    mutate(population = "PWID") %>%
    filter(time_period %in% c("2016", "2017", "2018", 
                              "2019", "2020"),
           OU %in% complete_totals$area)
  
  long_by_ou_prisoners <- pivot_longer(
    as.data.frame(complete_totals_tab[["prisoners living with HIV"]]), 
    !time_period,
    names_to = "OU", 
    values_to = "n_ests") %>%
    mutate(population = "Prisoners") %>%
    filter(time_period %in% c("2016", "2017", "2018", 
                              "2019", "2020"),
           OU %in% complete_totals$area)
  
  long_by_ou_sw <- pivot_longer(
    as.data.frame(complete_totals_tab[["sex workers"]]), 
    !time_period,
    names_to = "OU", 
    values_to = "n_ests") %>%
    mutate(population = "SW") %>%
    filter(time_period %in% c("2016", "2017", "2018", 
                              "2019", "2020"),
           OU %in% complete_totals$area)
  
  long_by_ou_tp <- pivot_longer(
    as.data.frame(complete_totals_tab[["transgender people"]]), 
    !time_period,
    names_to = "OU", 
    values_to = "n_ests") %>%
    mutate(population = "TP") %>%
    filter(time_period %in% c("2016", "2017", "2018", 
                              "2019", "2020"),
           OU %in% complete_totals$area)
  
  full_sparse_df <- long_by_ou_msm %>%
    full_join(., long_by_ou_pwid, 
              by = c("time_period", 
                     "OU",
                     "population", 
                     "n_ests")) %>%
    full_join(., long_by_ou_prisoners, 
              by = c("time_period", 
                     "OU",
                     "population", 
                     "n_ests")) %>%
    full_join(., long_by_ou_sw, 
              by = c("time_period", 
                     "OU",
                     "population", 
                     "n_ests")) %>%
    full_join(., long_by_ou_tp, 
              by = c("time_period", 
                     "OU",
                     "population", 
                     "n_ests"))
  
  return(full_sparse_df)
}

missing_data_heatmap <- function(df, title){
  
  df %>%
    group_by(OU, population) %>%
    mutate(sum = sum(n_ests)) %>%
    ggplot(aes(time_period, fct_reorder(OU, sum))) +
    geom_tile(aes(fill = n_ests), color = "white", alpha = .4, 
              show.legend = FALSE) + 
    facet_grid(~fct_reorder(population, sum, .desc = TRUE)) +
    scale_x_discrete(position = "top") +
    viridis::scale_fill_viridis(option = "D") +
    theme(legend.position = "none") +
    labs(x = NULL, y = NULL, fill = NULL,
         color = NULL,
         fill = NULL,
         title = title %>% toupper,
         caption = glue("J. Hoehner, Office of HIV/AIDS | UNAIDS KP Atlas Database 2021 | {ref_id} "),
         subtitle = "A yellow box indicates an available estimate while a purple box indicates no estimate is available") +
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

# EDA --------------------------------------------------------------------------

# of the population size ests and perecent coverage indicators, 
# which OUs have complete data for all KPs for 2020? 
# focusing especially on PEPFAR supported OUs

# How many unique national OUs are in the data?

unique_ous <- kp_tidier %>%
  select(area, area_id) %>%
  distinct() 
# 159

# how many pepfar supported OUs?
pepfarunique_ous <- unique_ous %>%
  drop_na(operatingunit)
# 55

# for PSE

ou_table_pse <- as.data.frame(tabyl(kp_tidier, time_period, population)) %>%
  pivot_longer(!time_period, 
               names_to = "population", values_to = "n_estimates") %>%
  filter(n_estimates > 0) %>%
  group_by(time_period, population)

ggplot(ou_table_pse, 
       aes(x = time_period, 
           y = n_estimates, 
           group = population, 
           fill = population)) + 
  geom_col() +
  facet_wrap(~population) +
  scale_fill_manual(values = c(old_rose_light, denim_light, golden_sand_light, 
                                genoa_light, moody_blue_light)) +
  si_style_ygrid() +
  labs(
    y = "Number of OUs with Population Size Estimate")

# we can see that there is only at least one estimate available for any 
# KP between 2011-2020
# the maximum number of OUs represented for a given KP 
# in a given year is 50 and this only occurs for 2016 and 2019 for PSEs for sex workers
# In most years, for most KPs there are fewer than 30 OUs with PSE data for a
# given year and KP so for most years we are only seeing PSE data for a given KP
# for 30/159 = 19% of all OUs represented in the data and if we assume all 30 were 
# pepfar supported OUs, that would still only be data from 54% of all pepfar 
# supported OUs present in the data.

# for coverage

complete_totals_cov <- kp_tidier %>%
  filter(geo_level == "National",
         indicator_new %in% c("Coverage of HIV prevention programmes"))

# Which ous have complete PSE data for all KPs?
complete_totals_2020_pse <- complete_totals_2020 %>%
  filter(indicator_new == "Population Size Estimate")

ou_table_2020_pse <- as.data.frame(tabyl(complete_totals_2020_pse, area, population)) %>%
  pivot_longer(!area, names_to = "population", values_to = "n_estimates") %>%
  filter(n_estimates > 0) %>%
  group_by(area) %>%
  summarize(n_kps = sum(n_estimates)) %>%
  filter(n_kps >= 5)

complete_totals_2020 <- kp_tidier %>%
  filter(time_period == "2020", 
         geo_level == "National",
         subgroup %in% c("Total", "estimate"),
         indicator_new %in% c("Population Size Estimate", 
                              "Coverage of HIV prevention programmes"))

# Which ous have complete PSE data for all KPs?
complete_totals_2020_pse <- complete_totals_2020 %>%
  filter(indicator_new == "Population Size Estimate")
  
ou_table_2020_pse <- as.data.frame(tabyl(complete_totals_2020_pse, area, population)) %>%
  pivot_longer(!area, names_to = "population", values_to = "n_estimates") %>%
  filter(n_estimates > 0) %>%
  group_by(area) %>%
  summarize(n_kps = sum(n_estimates)) %>%
  filter(n_kps >= 5)

# no oUs have complete data for all KPs in 2020 (otherwise, n_kps would be >= 5 
# since there are 5 KPs in this dataset,
# the most any OU has is 4 and those are ony available for Nicaragua, 
# Guatemala, Philippines, Côte d'Ivoire, Zambia 

# which OUS have complete coverage data for all KPs in 2020?
complete_totals_2020_cov <- complete_totals_2020 %>%
  filter(indicator_new == "Coverage of HIV prevention programmes")

ou_table_2020_cov <- as.data.frame(tabyl(complete_totals_2020_pse, area, population)) %>%
  pivot_longer(!area, names_to = "population", values_to = "n_estimates") %>%
  filter(n_estimates > 0) %>%
  group_by(area) %>%
  summarize(n_kps = sum(n_estimates)) %>%
  filter(n_kps >= 5)

# no oUs have complete data for all KPs in 2020 (otherwise, n_kps would be >= 5 
# since there are 5 KPs in this dataset,
# the most any OU has is 4 and those are ony available for Nicaragua, 
# Guatemala, Philippines, Côte d'Ivoire, Zambia 

# What about 2019?
complete_totals_2019 <- kp_tidier %>%
  filter(time_period == "2019", 
         geo_level == "National",
         subgroup %in% c("Total", "estimate"),
         indicator_new %in% c("Population Size Estimate", 
                              "Coverage of HIV prevention programmes"))

# Which ous have complete PSE data for all KPs?
complete_totals_2019_pse <- complete_totals_2019 %>%
  filter(indicator_new == "Population Size Estimate")

ou_table_2019_pse <- as.data.frame(tabyl(complete_totals_2019_pse, area, population)) %>%
  pivot_longer(!area, names_to = "population", values_to = "n_estimates") %>%
  filter(n_estimates > 0) %>%
  group_by(area) %>%
  summarize(n_kps = sum(n_estimates)) %>%
  filter(n_kps == 1)

# no oUs have complete data for all KPs in 2019 (otherwise, n_kps would be >= 5 
# since there are 5 KPs in this dataset,
# the most any OU has is 4 and those are only available for
# Guatemala, Philippines, Singapore, Lao People's Democratic Republic,
# Dominican Republic, Mexico

# Let's say we want to make a scorecard for COD
# What data are most recently available for that?

kp_cod_recent <- kp_tidier %>%
  filter(area_id == "COD", 
         time_period == "2020")

kp_table_1 <- tabyl(kp_cod_recent, population, indicator_new)

# For the year 2020 in COD, we have data from the following KPs: 

# "men who have sex with men", 5/6 indicators
# "men who have sex with men living with HIV", 1/ 6 indicators
# "people who inject drugs", 6/6 indicators
# "prisoners", 2/6 indicators
# "prisoners living with HIV"  , 1/ 6 indicators               
# "sex workers", 6/6 indicators                              
# "transgender people", 6/6 indicators

# on the following indicators: 

# Antiretroviral therapy coverage`, 5/7 pops listed above
# Avoidance of health care because of stigma and discrimination`, 4/7 pops listed above
# Coverage of HIV prevention programmes`, 4/7 pops listed above
# HIV prevalence`, 5/7 pops listed above
# HIV testing and status awareness`, 4/7 pops listed above
# Population Size Estimate`, 5/7 pops listed above

# Is this information available for all years?

kp_cod_allyears <- kp_tidier %>%
  filter(area_id == "COD")

kp_table_2 <- tabyl(kp_cod_allyears, population, indicator_new, time_period)

# data only available between 2011 and 2020, 
# specifically only 2011, 2012, 2013, 2015, 2016, 2018, 2019, and 2020
# available indicators also vary from year to year with data becoming 
# more and more sparse as the years progress

# draft figures ----------------------------------------------------------------
  
# population sizes: 
# which years have population size estimates for each KP?
cod_pop_data_allyrs <- kp_cod_allyears %>%
  filter(indicator_new == "Population Size Estimate")

cod_table <- tabyl(cod_pop_data_allyrs, time_period, population)
# We only have data for 3 KPs for 3 of the same years 

ggplot(cod_pop_data_allyrs, 
       aes(y = data_value, 
           x = time_period, 
           fill = population)) +
  geom_col() +
  facet_wrap(~population) +
  si_style_ygrid() +
  scale_fill_manual(values = 
  c("men who have sex with men" = moody_blue_light, 
    "people who inject drugs" = scooter_light, 
    "prisoners" = denim_light, 
    "sex workers" = genoa_light), 
    labels = NULL) +
  theme(legend.position = "none") +
  labs( 
    x = NULL,
    y = NULL,
    fill = NULL, 
    title = "Key population (KP) size estimates are unavailable or inconsistent from year to year",
    subtitle = "Estimates are not available for all KPs of interest for all years, including most recent.",
    caption = "Source: UNAIDS Key Population Atlas Database 2021 | refid: d1836592")

si_save(here::here("thathill/Graphics/pop_size_ests_draft.png"))

# prevention: 

# what data do we have for prevention indicators?

# How much data do we have in the prevention bucket?

cod_prev_data <- kp_cod_allyears %>%
  filter(bucket == "prevention",
         subgroup %in% c("Total")) %>%
  mutate(data_value = as.numeric(data_value)) %>%
  filter(data_value > 0)

# HIV prevalence, 2015 and 2019
# only have matching estimates from 2015 for the groups shown, 
# 2019 is the most recent year available for all groups
# total or all ages, COD, 
# only have data from 2020 for MSM, only have All ages data from 2019, no age breakdowns
# don't have 2015 data for sex workers

ggplot(cod_prev_data, 
       aes(x = time_period, 
           y = data_value, 
           fill = population)) +
  geom_col() +
  facet_wrap(~population) +
  si_style_ygrid() +
  scale_fill_manual(values = 
                      c("men who have sex with men" = moody_blue_light, 
                        "people who inject drugs" = scooter_light, 
                        "prisoners" = denim_light, 
                        "sex workers" = genoa_light), 
                    labels = NULL) +
  theme(legend.position = "none") +
  labs( 
    x = NULL,
    y = NULL,
    fill = NULL, 
    title = "HIV prevalence estimates are unavailable or inconsistent from year to year",
    subtitle = "Estimates are not available for all KPs of interest for all years, including most recent.",
    caption = "Source: UNAIDS Key Population Atlas Database 2021 | refid: d1836592")

si_save(here::here("thathill/Graphics/hiv_prev_ests_draft.png"))

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

# population size estimate
sparse_df_pse <- populate_sparse_df_notPLHIV(df = kp_tidier, 
                                             indicator = "Population Size Estimate")
sparse_df_pse %>% 
  missing_data_heatmap(., 
                       "How Many Poulation Size Estimates Exist From PEPFAR supported OUs
 with available KP data in the previous 5 years?")

si_save(glue("Images/KPAtlasfindings_PopulationSizeEst_{Sys.Date()}.png"),
        height = 10, width = 10)

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
  
