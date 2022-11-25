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
  
  
  g_id <- '1y7r9BHJrpsbTYZuoTspIpLOIoD35CJUMFLp6QWv1Blc'

# IMPORT ------------------------------------------------------------------
  
df_tool <- data_folder %>% 
    return_latest("CALHIV") %>% 
    read_xlsx(sheet = "Merged Clean datasheet") %>% 
    janitor::clean_names()
  
mfl <- data_folder %>% 
  return_latest("USAID Only") %>% 
  read_xlsx(skip = 2)

df_crosswalk <- read_sheet(g_id) %>% 
  select(1:2)

# MUNGE -------------------------------------------------------------------

#First, let's try to join the MFL to the Tool to bring in the DATIM UID
  # here is the list of facilities that dont match

misaligned_facilities <- df_tool %>% 
  left_join(mfl %>% select(`DATIM HF Name`, `DATIM ID`, `DATIM Region`, `DATIM District`, `DATIM Subcounty`),
            by = c("facility_name" = "DATIM HF Name")) %>% view() 
  filter(is.na(`DATIM ID`)) %>% 
  distinct(facility_name)
  
  
  df_merge <- df_tool %>% 
    left_join(df_crosswalk, by = c("facility_name" = "tool_facility_name")) %>% 
    mutate(new_name = ifelse(is.na(mfl_facility_name), facility_name, mfl_facility_name)) %>% 
    left_join(mfl %>% select(`DATIM HF Name`, `DATIM ID`, `DATIM Region`, `DATIM District`, `DATIM Subcounty`),
              by = c("new_name" = "DATIM HF Name")) %>%  
  filter(!is.na(`DATIM ID`)) %>% 
    select(region, district, facility_name, age_dependent_on_column_k, vls, new_name, starts_with("DATIM"))
  
  df_merge %>% count(vls)
  
  
