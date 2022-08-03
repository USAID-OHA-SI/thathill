# PROJECT:  Last Mile Analytics
# AUTHOR:   J.Hoehner | USAID/PHI
# PURPOSE:  Exploration of KP Atlas data
# REF ID:   d1836592
# LICENSE:  MIT
# DATE CREATED: 2022-07-21
# DATE UPDATED: 2022-08-01

# dependencies -----------------------------------------------------------------

library(readr)
library(janitor)
library(tidyverse)
library(skimr)
library(stringr)
library(forcats)
library(readxl)
library(googlesheets4)

# global variables -------------------------------------------------------------

ref_id <- "d1836592"

# inputs and outputs -----------------------------------------------------------

outputs <- list(
kp_findings = "lastmile/Scripts/global_story/outputs/kp_findings.csv")

# munge ------------------------------------------------------------------------

# read in indicator crosswalk data to further tidy data used
indicator_crosswalk <- read_sheet("1P2D8_nUpONqQeg_cp9lLvlC6jXEySOwRK1RJ57_xRAc")

# - UNAIDS - old
kp_data <- read_sheet("1KORmN23RjAKDz9yroJA5yn9csBOGBKDHKmK45JIs7tY") %>%
  clean_names() %>%
  mutate(
    across(.cols = indicator:source, ~ as_factor(.))) %>%
  # keep only indicators of interest from crosswalk file
  filter(indicator %in% indicator_crosswalk$indicator) %>%
  # join cols by indicator with crosswalk file to seperate population and 
  # indicator
  left_join(., indicator_crosswalk, 
            by = c("indicator")) %>%
  mutate(
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

# UNAIDS 2021 


# 14GBwcmAuGHETKknbJjeN1JkWSunpHcT4DvM3q4TlRIM

# Estimates by Year
HIV20212_year <- read_sheet("1YRs60-EInZXq0sfruiaR2xow1Bda4eN1LXCndacrbE4",
                          sheet = "HIV2022Estimates_ByYear") %>%
  clean_names() 

# we may likely want complete population estimates by year for the national 
# level estimates to get %s

# draft figures

# prevention: 

# what does prevention look like in the KPs we have so far?

mini_prev_data <- kp_data %>%
  filter(bucket == "prevention", 
         geo_level == "National", 
         # subgroup %in% c("Total", "All ages"), 
         area_id %in% c("COD", "SSD"))

# subgroup == "Total"
# people who inject drugs 
# prisoners
# sex workers 
# transgender people

# subgroup == "All ages" - can we change this to Total?
# men who have sex with men

# also have age breakdowns for some groups, sex breakdowns for other groups

# what is the prevalence of HIV among Key Pops in COD and SSD?

ggplot(mini_prev_data, 
       aes(x = population, 
           y = data_value)) +
  geom_col() +
  facet_wrap(~area)

# may not have all subgroups for all indicators for all populations 
# in all geographies
# Ex: Some areas may have "Total" or "All ages" data for some KPs and not others
# do we want to weight these national estimates by population?
# do we want to only keep countries in analysis which are USAID supported?



