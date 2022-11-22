# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  explore MER Peds VLS rates
# REF ID:   e410ba62 
# LICENSE:  MIT
# DATE:     2022-11-22
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(glamr)
  library(tidyverse)
  library(glitr)
  library(gophr)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(readxl)
  library(googlesheets4)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()
  
  # store Data Folder
  data_folder <- "Data/"
  
  #get filepath
  msd_path <- data_folder %>% 
    return_latest("Genie-SiteByIMs-Uganda-Daily-2022-11-22")

  
  #store metadata
  get_metadata(msd_path)
  

  ref_id <- "e410ba62"

# IMPORT ------------------------------------------------------------------
  
  
  #read MSD
  df_msd <- msd_path %>% 
    read_msd()

# MUNGE -------------------------------------------------------------------
  
  
  df_msd %>% 
    clean_indicator() %>% 
    clean_agency() %>% 
    filter(
      fiscal_year != metadata$curr_fy + 1,
      funding_agency == "USAID",
      indicator %in% c("TX_CURR", "TX_PVLS_D", "TX_PVLS"),
      standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/Indication/HIVStatus")
    ) %>% 
    group_by(fiscal_year, funding_agency, snu1, indicator, trendscoarse) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
    reshape_msd() %>% 
    select(-period_type) %>% 
    pivot_wider(names_from = indicator, values_from = value) %>% 
    group_by(funding_agency, snu1, trendscoarse) %>% 
    mutate(TX_CURR_LAG2 = lag(TX_CURR, 2, order_by = period)) %>% 
    relocate(TX_CURR_LAG2, .before = TX_CURR) %>% 
    filter(!is.na(TX_PVLS_D), TX_PVLS_D > 0) %>% 
    group_by(period, funding_agency, snu1, trendscoarse) %>% 
    summarise(across(starts_with("TX"), sum, na.rm = T), .groups = "drop") %>% 
    mutate(VLC = TX_PVLS_D / TX_CURR_LAG2,
           VLS = TX_PVLS/TX_PVLS_D) %>% 
    filter(period == metadata$curr_pd,
           trendscoarse == "<15")
  
  
  
  
