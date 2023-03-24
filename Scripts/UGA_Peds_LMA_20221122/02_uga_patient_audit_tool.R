# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  reshape Uganda CLHIV Audit Tool
# REF ID:   2072783d 
# LICENSE:  MIT
# DATE:     2022-11-23
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

  
  ref_id <- "2072783d"
  
  data_folder <- "Data/"
  
  #get filepath
  msd_path <- data_folder %>% 
    return_latest("Genie-SiteByIMs-Uganda-Daily-2022-11-22")
  
  #store metadata
  get_metadata(msd_path, caption_note = "Uganda October 2022 Patient Audit Tool
              Created by: OHA SI Team
             LMA Presentation ATD Meeting 11-28-2022")
  
  
  g_id <- '1y7r9BHJrpsbTYZuoTspIpLOIoD35CJUMFLp6QWv1Blc'

# IMPORT ------------------------------------------------------------------
  
df_tool <- data_folder %>% 
    return_latest("CALHIV_ Oct data") %>% 
    read_xlsx(sheet = "Merged Clean datasheet") %>% 
    janitor::clean_names()
  
mfl <- data_folder %>% 
  return_latest("USAID Only") %>% 
  read_xlsx(skip = 2)

df_crosswalk <- read_sheet(g_id) %>% 
  select(1:2)


#read MSD
df_msd <- msd_path %>% 
  read_msd()

# MUNGE -------------------------------------------------------------------

#First, let's try to join the MFL to the Tool to bring in the DATIM UID
  # here is the list of facilities that dont match

misaligned_facilities <- df_tool %>% 
  left_join(mfl %>% select(`DATIM HF Name`, `DATIM ID`, `DATIM Region`, `DATIM District`, `DATIM Subcounty`),
            by = c("facility_name" = "DATIM HF Name")) %>% 
  filter(is.na(`DATIM ID`)) %>% 
  distinct(facility_name)
  
  
  df_merge <- df_tool %>% 
    left_join(df_crosswalk, by = c("facility_name" = "tool_facility_name")) %>% 
    mutate(new_name = ifelse(is.na(mfl_facility_name), facility_name, mfl_facility_name)) %>% 
    left_join(mfl %>% select(`DATIM HF Name`, `DATIM ID`, `DATIM Region`, `DATIM District`, `DATIM Subcounty`),
              by = c("new_name" = "DATIM HF Name")) %>%  
  filter(!is.na(`DATIM ID`)) %>% 
    select(region, district, facility_name, age_dependent_on_column_k, vls, new_name, starts_with("DATIM"))
  
  #get distinct TX_CURR
  
  df_tool %>% 
    left_join(df_crosswalk, by = c("facility_name" = "tool_facility_name")) %>% 
    mutate(new_name = ifelse(is.na(mfl_facility_name), facility_name, mfl_facility_name)) %>% 
    left_join(mfl %>% select(`DATIM HF Name`, `DATIM ID`, `DATIM Region`, `DATIM District`, `DATIM Subcounty`),
              by = c("new_name" = "DATIM HF Name")) %>%  
    filter(!is.na(`DATIM ID`)) %>% 
    distinct(art_hiv_clinic_no, sex, age_dependent_on_column_k, weight_kg, date_of_birth)
  
  df_snu_audit <- df_merge %>%
    filter(vls %in% c("N", "Y")) %>% 
    count(`DATIM Region`, vls) %>% 
    pivot_wider(names_from = vls, values_from = n) %>% 
    mutate(total = N + Y,
           audit_vls = Y/total)
  
  df_age_audit <- df_merge %>%
    rename(age_2019 = age_dependent_on_column_k) %>% 
    mutate(age_band = case_when(age_2019 < 1 ~ "<01",
                                age_2019 >= 1 & age_2019 < 5 ~ "01-04",
                                age_2019 >= 5 & age_2019 < 10 ~ "05-09",
                                age_2019 >= 10 & age_2019 < 15 ~ "10-14",
                                age_2019 >= 15 & age_2019 < 20 ~ "15-19")) %>% 
    filter(vls %in% c("N", "Y")) %>% 
    count(age_band, vls) %>% 
    pivot_wider(names_from = vls, values_from = n) %>% 
    mutate(total = N + Y,
           audit_vls = Y/total)
  
  
# MSD ----------------------------------------------------------------
  
  # df_msd %>% 
  #   clean_indicator() %>% 
  #   clean_agency() %>% 
  #   filter(
  #     # fiscal_year == metadata$curr_fy,
  #     funding_agency == "USAID",
  #     indicator %in% c("TX_CURR"),
  #     standardizeddisaggregate %in% c("Age/Sex/HIVStatus")
  #   ) %>% 
  #   filter(fiscal_year == metadata$curr_fy) %>% 
  #   filter(
  #     #period == metadata$curr_pd,
  #     age_2019 %in% c("<01", "01-04", "05-09", "10-14", "15-19")) %>% 
  #   group_by(fiscal_year, funding_agency, indicator) %>% 
  #   summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  #   reshape_msd() %>% 
  #   select(-period_type)
    
  
  #MSD BY SNU
  df_viz_snu <- df_msd %>% 
    clean_indicator() %>% 
    clean_agency() %>% 
    filter(
      # fiscal_year == metadata$curr_fy,
      funding_agency == "USAID",
      indicator %in% c("TX_CURR", "TX_PVLS_D", "TX_PVLS"),
      standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/Indication/HIVStatus")
    ) %>% 
    filter(fiscal_year == metadata$curr_fy) %>% 
    group_by(fiscal_year, funding_agency, snu1, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
    reshape_msd() %>% 
    select(-period_type) %>% 
    pivot_wider(names_from = indicator, values_from = value) %>% 
    group_by(funding_agency, snu1) %>% 
    mutate(TX_CURR_LAG2 = lag(TX_CURR, 2, order_by = period)) %>% 
    relocate(TX_CURR_LAG2, .before = TX_CURR) %>% 
    filter(!is.na(TX_PVLS_D), TX_PVLS_D > 0) %>% 
    group_by(period, funding_agency, snu1) %>% 
    summarise(across(starts_with("TX"), sum, na.rm = T), .groups = "drop") %>% 
    mutate(VLC = TX_PVLS_D / TX_CURR_LAG2,
           VLS = TX_PVLS/TX_PVLS_D,
           endpoints = case_when(period %in% c(max(period), min(period))~VLS)) 
  
  #MSD BY AGE
  df_age_viz <- df_msd %>% 
    clean_indicator() %>% 
    clean_agency() %>% 
    filter(
      fiscal_year == metadata$curr_fy,
      funding_agency == "USAID",
      indicator %in% c("TX_CURR", "TX_PVLS_D", "TX_PVLS"),
      standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/Indication/HIVStatus")
    ) %>% 
    group_by(fiscal_year, funding_agency, indicator, age_2019) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
    reshape_msd() %>% 
    select(-period_type) %>% 
    pivot_wider(names_from = indicator, values_from = value) %>% 
    group_by(funding_agency, age_2019) %>% 
    mutate(TX_CURR_LAG2 = lag(TX_CURR, 2, order_by = period)) %>% 
    relocate(TX_CURR_LAG2, .before = TX_CURR) %>% 
    filter(!is.na(TX_PVLS_D), TX_PVLS_D > 0) %>% 
    group_by(period, funding_agency, age_2019) %>% 
    summarise(across(starts_with("TX"), sum, na.rm = T), .groups = "drop") %>% 
    mutate(VLC = TX_PVLS_D / TX_CURR_LAG2,
           VLS = TX_PVLS/TX_PVLS_D) %>% 
    filter(
      #period == metadata$curr_pd,
      age_2019 %in% c("<01", "01-04", "05-09", "10-14", "15-19"))
  
  
# VIZ -----------------------------------------------------------
  
  # VIZ by SNU with audit tool
  
  df_viz_snu %>% 
    left_join(df_snu_audit %>% select(`DATIM Region`, audit_vls), by = c("snu1" = "DATIM Region")) %>% 
    mutate(audit_vls = ifelse(period %in% c("FY22Q1", "FY22Q2"), NA, audit_vls)) %>% 
    ggplot(aes(period, VLS, group = snu1, color = scooter_med, fill = scooter_med)) +
    geom_area(alpha = .4, size = .9, position = "identity") +
    geom_area(aes(y = audit_vls), fill = "#8AB75E", color = "#8AB75E", alpha = .4) +
    geom_point(aes(y = endpoints), na.rm = TRUE) +
    geom_text(aes(y = audit_vls, label = percent(audit_vls, 1)), color = "#8AB75E", na.rm = TRUE,
              hjust = -.2, vjust = -0.7,family = "Source Sans Pro") +
    geom_text(aes(label = percent(VLS, 1)), na.rm = TRUE,
              hjust = -.2, vjust = -0.7,family = "Source Sans Pro") +
    facet_wrap(~fct_reorder2(snu1, period, VLS, .desc = TRUE)) +
    scale_fill_identity() +
    scale_y_continuous(label = percent, 
                       breaks = seq(0, 1, .25)) +
    scale_color_identity() +
    si_style_ygrid() +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         title = glue("USAID has seen declines in viral load suppression among CLHIV across most SNUs" %>% toupper()),
         subtitle = "CHLIV from <1 to 19 years of age",
         caption = glue("{metadata$caption}"))
  
  si_save("Graphics/01_vls_patient_tool_snu.svg")

  df_age_viz %>% 
    left_join(df_age_audit %>% select(age_band, audit_vls), by = c("age_2019" = "age_band")) %>%
    mutate(audit_vls = ifelse(period %in% c("FY22Q1", "FY22Q2"), NA, audit_vls)) %>% 
    ggplot(aes(period, VLS, group = age_2019, color = scooter_med, fill = scooter_med)) +
    geom_area(alpha = .4, size = .9, position = "identity") +
    geom_area(aes(y = audit_vls), fill = "#8AB75E", color = "#8AB75E", alpha = .4) +
    # geom_point(aes(y = endpoints), na.rm = TRUE) +
    geom_text(aes(label = percent(VLS, 1)), na.rm = TRUE,
              hjust = -.2, vjust = -0.7,family = "Source Sans Pro") +
    geom_text(aes(y = audit_vls, label = percent(audit_vls, 1)), color = "#8AB75E", na.rm = TRUE,
              hjust = -.2, vjust = -0.7,family = "Source Sans Pro") +
    facet_wrap(~fct_reorder2(age_2019, period, VLS, .desc = FALSE)) +
    scale_fill_identity() +
    scale_y_continuous(label = percent, 
                       breaks = seq(0, 1, .25)) +
    scale_color_identity() +
    si_style_ygrid() +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         title = glue("USAID has seen declines in viral load suppression in FY22 among CLHIV, with particular fluctuation in the <1 age band" %>% toupper()),
         subtitle = "CHLIV from <1 to 19 years of age",
         caption = glue("{metadata$caption}"))

  si_save("Graphics/02_vls_patient_tool_age.svg")
  
  
