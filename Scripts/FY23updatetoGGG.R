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
df_cscd_subnat <- read_sheet("1poDM5ZSA2-QBo5oxzzZot532wS4ipbllcPgPBGPXhE4") %>%
  clean_names()

# Read in Desired Indicators/names/groupings from unaids_indicator_dataset_xwalk ----

un_indic_cw <- read_sheet("1Hd2UQwsxkmnifHmfeO5kGGxHybRUK6HsYp38lXUFH4w", 
                          sheet = "Indicators") %>%
  clean_names()

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
  
  # Data for "Subnational Cascades" page comes from UNAIDS EDMS pull -document this-
  # "Nat + Subnat Cascade for G^3 dashboard"
  # need ""Percent Known Status of PLHIV", "Percent on ART with Known Status", and "Percent VLS on ART"
  
  df_cscd_subnat_clean <- df_cscd_subnat %>%
    clean_names() %>%
    # keep only subnational data from PEPFAR supported countries, 
    # in previous years we've only seen data from Ethiopia, Kenya, Moldova, and Zimbabwe
    # using the pepfar_ous list in glamr doesn't catch the iso3 codes in subnational
    # data so you may want to check manually which countries have submitted
    # subnational estimates
    filter(type == "Sub1",
          !region %in% c("Republic of Moldova", "Moldova")) %>%
    # select only desired cols to clean
    select(region, iso2, iso3, e_count, time, 
           e_ind, sex, age, rounded, other) %>%
    rename(
      country = e_count,
      indicator = e_ind,
      year = time,
      bound = other, 
      estimate = rounded) %>%
    # splitting the country column to create the admin1 column
    separate(country,
      into = c("country", "admin1"), sep = " - ") %>%
    # split the e_ind column into "Code" and "Indicator"
    # code denotes the year, 2023 estimates have a code of "N"
    separate(indicator,
      into = c("code", "indicator"),
      sep = "- ",
      extra = "merge",
      remove = F) %>%
    # split indicator again to remove age/sex/bound information
    # there will be NA ests for indicators which do not have this extra info
    separate(indicator,
      into = c("indicator", "extra_bound"),
      sep = "; (?=U|L)|- (?=U|L)") %>%
    # split the indicator column again to pull out the age/sex information
    separate(indicator,
      into = c("indicator", "extra_age_sex"),
      sep = "; ",
      extra = "merge",
      remove = F) %>%
    mutate(
      estimate = as.double(estimate),
      admin1 = str_replace(admin1, " \\s*\\([^\\)]+\\)", "") %>% trimws(),
      country = str_replace(country, " States", ""),
      bound = case_when(
        bound == "lb" ~ "lower_bound",
        bound == "ub" ~ "upper_bound",
        is.na(bound) ~ "estimate"),
      indicator = trimws(indicator),
      sex = case_when(
        sex == "M+F"~ "all",
        sex == "M"~ "male",
        sex == "F"~ "female"),
      age = case_when(
        str_detect(age, "allAges") ~ "all",
        is.na(age) ~ "all",
        # if the string has 0 length, replace with all
        nchar(age) == 0 ~ "all",
        TRUE ~ age),
      # this will cause an error if the name of the indicator from EDMS has changed
      # so the "seperate" functions don't create the column correctly
      indicator = case_match(indicator,
          "Among people living with HIV- the percent who know their status" ~ "Percent Known Status of PLHIV",
          "Among people who know their HIV status- the percent on ART" ~ "Percent on ART with Known Status",
          "Among people on ART- the percent with suppressed viral load" ~ "Percent VLS on ART"),
      dataset = "cascade") %>%
    # this will cause an error if the name of the indicator from EDMS has changed
    # so the "seperate" functions don't create the column correctly
    case_match(indicator,
      "Among people living with HIV- the percent who know their status" ~ "Percent Known Status of PLHIV",
      "Among people who know their HIV status- the percent on ART" ~ "Percent on ART with Known Status",
      "Among people on ART- the percent with suppressed viral load" ~ "Percent VLS on ART") %>%
    pivot_wider(names_from = "bound",
                values_from = "estimate") %>%
    # fill in estimates where missing after pivot, this makes sure we 
    # can use arrange to correctly line up the data for filling the 
    # lower and upper bound values which are also missing after the pivot
    fill(estimate, .direction = "down") %>%
    arrange(year, region, country, admin1, age, sex, indicator, estimate) %>%
    # fill in the estimates for the bounds and keep only the rows where the 
    # estimates (not the bounds) were originally since these were filled 
    # correctly
    # Note that "updown" fills in incorrect estimates for the rows where the "upper bounds"
    # were originally and the filter step keeps only the correct estimates matched 
    # with their correct bounds 
    fill(lower_bound, .direction = "updown") %>%
    fill(upper_bound, .direction = "updown") %>%
    filter(is.na(extra_bound)) %>%
    # select only columns needed for the dashboard
    select(region, iso2, iso3, country, admin1, year, indicator, sex, age, 
           estimate, lower_bound, upper_bound)
  
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
  
  # National
  
df_cscd %>%
  write_sheet(., ss = "1SqFLAzKXrvdDAARErBRx76DZPL0jg0-VC7aRk4xxRPs",
              sheet = "unaids_cascade")

  # Subnational
  
df_cscd_subnat_clean %>%
   write_sheet(., ss = "1Zb8NHwTF7uDNr5wPCmTTUczPGVtgtHG_Yq25nhxHSmY",
               sheet = "unaids_cascade_subnat")

# Epi control data

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
