# PROJECT:  Last Mile Analytics
# AUTHOR:   J.Hoehner | USAID/PHI
# PURPOSE:  Exploration of KP Atlas data
# REF ID:   d1836592
# LICENSE:  MIT
# DATE CREATED: 2022-07-21
# DATE UPDATED: 2022-08-15

# dependencies -----------------------------------------------------------------

library(glamr)
library(glitr)
library(readr)
library(janitor)
library(tidyverse)
library(tidytext)
library(skimr)
library(stringr)
library(forcats)
library(readxl)
library(googlesheets4)
library(extrafont)

load_secrets()

# global variables -------------------------------------------------------------

ref_id <- "d1836592"
options(scipen = 999)

# inputs and outputs -----------------------------------------------------------

outputs <- list(
kp_findings = "lastmile/Scripts/global_story/outputs/kp_findings.csv")
ymax = "2021"
goal = 95

# munge ------------------------------------------------------------------------

# read in indicator crosswalk data to further tidy data used
indicator_crosswalk <- read_sheet("1P2D8_nUpONqQeg_cp9lLvlC6jXEySOwRK1RJ57_xRAc") %>%
  clean_names()

# - KP data
kp_data <- read_sheet("1KORmN23RjAKDz9yroJA5yn9csBOGBKDHKmK45JIs7tY")


kp_tidier <- kp_data %>%
  clean_names() %>%
  select(!starts_with("x")) %>%
  mutate(
    across(.cols = indicator:source, ~ as_factor(.))) %>%
  # keep only indicators of interest from crosswalk file
  filter(indicator %in% indicator_crosswalk$indicator) %>%
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
  # rearrange the columns, keep only cols of interest
  select(bucket, indicator_new, population, subgroup, 
         geo_level, area, area_id, data_value, unit, 
         time_period, source)

# EDA --------------------------------------------------------------------------

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
  
