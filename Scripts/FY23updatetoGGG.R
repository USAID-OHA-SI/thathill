# PROJECT:  thathill
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  FY23 update to Global Goals and Gaps dashboard
# REF ID:   371be8h5
# LICENSE:  GPL v3 +
# DATE:     2023-08-21

# dependencies -----------------------------------------------------------------

library(tidyverse)
library(readr)
library(gagglr)
library(glue)
library(scales)
library(extrafont)
library(tidytext)
library(googlesheets4)
library(janitor)
library(mindthegap)
library(stringr)
library(forcats)
library(readxl)
library(ggtext)
library(glue)
library(lubridate)
library(assertr)

# global vars ------------------------------------------------------------------

# SI specific paths/functions
load_secrets()

ref_id <- "371be8h5"

# import -----------------------------------------------------------------------

# Tidied UNAIDS data 
# for cascade data
df_hiv_ests <- pull_unaids(data_type = "HIV Estimates", pepfar_only = TRUE)

# for rest of indicators
df_tt <- pull_unaids(data_type = "HIV Test & Treat", pepfar_only = TRUE)

# Data for "Subnational Cascades" page comes from UNAIDS EDMS pull
# "Nat + Subnat Cascade for G^3 dashboard"
# need ""Percent Known Status of PLHIV", "Percent on ART with Known Status", and "Percent VLS on ART"
df_cscd_subnat <- read_sheet("1poDM5ZSA2-QBo5oxzzZot532wS4ipbllcPgPBGPXhE4")

# Read in Desired Indicators/names/groupings from unaids_indicator_dataset_xwalk ----

un_indic_cw <- read_sheet("1Hd2UQwsxkmnifHmfeO5kGGxHybRUK6HsYp38lXUFH4w", 
                          sheet = "Indicators") %>%
  clean_names()

# Load PEPFAR names so countries can be tagged in full dataset
pepfar_ous <- glamr::pepfar_country_list %>%
  dplyr::select(country, iso = country_iso) %>%
  dplyr::rename(countryname = country)

# munge ------------------------------------------------------------------------

  # Munge Data for Tableau-----
  
  # Join in the preferred names for indicators from google drive
  # indicators have different names in the tidy UNAIDS data than 
  # they do in EDMS and the dashboard so we need to join on the tidy name to 
  # match them
  
  df_g3_cscd <- df_tt %>%
    left_join(., un_indic_cw %>% select(tidy_name, dataset, age, sex), 
              by = c("indicator" = "tidy_name", "age", "sex"))
  
  df_g3_hiv <- df_hiv_ests %>%
    left_join(., un_indic_cw %>% select(tidy_name, dataset, age, sex), 
              by = c("indicator" = "tidy_name", "age", "sex"))

  # Cascade output for LMA/GGG dashboard----
  
  max_yr <- max(df_g3_cscd$year)
  
  # for verifying that all cascade variables needed are in data
  cscd_inds <-   un_indic_cw %>%
    filter(dataset == "cascade") %>%
    select(tidy_name) %>%
    distinct()
  
  df_cscd <- df_g3_cscd %>%
    mutate(cascade_mkr = substr(indicator, 3, 8)) %>%
    select(region, iso, country, year, estimate, lower_bound, upper_bound, 
           indicator, cascade_mkr, sex, age, pepfar, country, dataset) %>%
    filter(dataset == "cascade",
           pepfar == T,
           year >= max_yr-3) %>%
    distinct() %>%
    # verify that all indicators needed are in the data
    verify(cscd_inds$tidy_name %in% indicator)
  
  # Data for "Subnational Cascades" page comes from UNAIDS EDMS pull
  # "Nat + Subnat Cascade for G^3 dashboard"
  # need ""Percent Known Status of PLHIV", "Percent on ART with Known Status", and "Percent VLS on ART"
  df_cscd_subnat_clean <- df_cscd_subnat %>%
    clean_names() %>%
    # split the e_ind column into "Code" and "Indicator"
    # code denotes the year, 2023 estimates have a code of "N"
    separate(e_ind,
             into = c("code", "indicator"),
             sep = "- ",
             extra = "merge",
             remove = F) %>%
    # split the indicator column again to create a column for "bound"
    # to show the upper and lower bounds of each estimate
    separate(indicator,
             into = c("indicator", "bound"),
             sep = "; (?=U|L)|- (?=U|L)") %>%
    # split the indicator column again to pull out the sex information
    # UNAIDS provides a "sex" column but we would prefer to format
    # this column differently
    separate(indicator,
             into = c("indicator", "sex_usaid"),
             sep = "; (?=\\()|(?=Female|Male)| (?=\\(\\d)",
             extra = "merge") %>%
    rename(
      country = e_count,
      geo_level = type,
      year = time) %>%
    mutate(
      bound = case_when(
        bound == "Lower bound" ~ "lower bound",
        bound == "Upper bound" ~ "upper bound",
        is.na(bound) ~ "estimate"),
      indicator = trimws(indicator),
      sex_usaid = if_else(sex_usaid == "Male+Female", "all",
                          sex_usaid %>% tolower()),
      age = case_when(
        str_detect(age, "allAges") ~ "all",
        is.na(age) ~ "all",
        # if the string has 0 length, replace with all
        nchar(age) == 0 ~ "all",
        TRUE ~ age),
      dataset = if_else(stringr::str_detect(indicator, "Total"), "total_deaths_hiv_pop",
                        "infections_deaths_averted")) %>%
    select(region, iso2, iso3, country,
           geo_level, year, indicator,
           sex_usaid, age, value, bound, dataset) %>%
    pivot_wider(names_from = "bound",
                values_from = "value") %>%
    rename(sex = sex_usaid)
  
  # EPI DATA -------
  
  epi_inds <-   un_indic_cw %>%
    filter(dataset == "epi control") %>%
    select(tidy_name)%>%
    distinct()
  
  df_epi_control <- df_g3_hiv %>%
    select(region, iso, country, indicator, year, indicator, estimate, 
           lower_bound, upper_bound, sex, age, pepfar, country, dataset) %>%
    filter(dataset == "epi control") %>%
    mutate(estimate = round(estimate, 0)) %>%
    distinct() 
  
  # new HIV infections
  
  df_new <- df_epi_control %>%
    filter(indicator == "Number New HIV Infections")
  
  # Prev and Incid indicators
  
  prev_inds <- un_indic_cw %>%
    filter(dataset == "prevention") %>%
    select(tidy_name)%>%
    distinct()
  
  # Need "Incidence Per 1000"               
    
  df_prev <- df_g3_hiv %>%
    filter(dataset == "prevention") %>%
    distinct()
  
  # Infections averted by PMTCT and Number deaths averted by ART ----
  # Most data only available for a given span of time -- Starts in 2004
  
  averted_inds <- un_indic_cw %>%
    select(tidy_name) %>%
    distinct()
  
  df_averted <- df_g3_hiv %>%
    filter(str_detect(indicator, "averted by PMTCT|averted by ART"),
           year > 2003) %>%
    distinct()

# visualize --------------------------------------------------------------------

# Export data for Tableau ---
  
  # All in 2023 Custom Pull/output/GGG dashboard

# Cascade data
  
df_cscd %>%
  write_sheet(., ss = "1SqFLAzKXrvdDAARErBRx76DZPL0jg0-VC7aRk4xxRPs",
              sheet = "unaids_cascade")

# df_cscd_subnat %>%
#   write_sheet(., ss = "",
#               sheet = "unaids_cascade_subnat")

# Epi control data
# 
# df_epi %>%
#   write_sheet(., ss = "1uj3Fr8Z8vgnFW98QBO6xZdp05-pqzrHiV_4bnA-KBck",
#               sheet = "unaids_epi")

df_epi_control %>%
  write_sheet(., ss = "19gLMfHliFQgHzsCXIFEFEiXZD6P9iUS-CfbEckveUX4",
              sheet = "unaids_epi_control")

# Prevalence and Incidence data

df_prev %>%
  write_sheet(., ss = "11eB_fR3Zhpv_dIacJb_gLtMH55RC7UZrjIMLSH75YZE",
              sheet = "unaids_incidence_prevalence")

# Infections and Deaths Averted data
df_averted %>%
  write_sheet(., ss = "1YQKV-2CRlDLBiXSv3U2lgTCgCP1PtOqQoCTrVdkFjUM",
              sheet = "unaids_averted")

# New infections
df_new %>%
  write_sheet(., ss = "1nr-IXbpn7No18zTpJE348TnWmcBCscVa9i9cGMwKNUU",
              sheet = "unaids_new_hiv_infections")
