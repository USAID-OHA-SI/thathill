# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  
# REF ID:   37dd8f05 
# LICENSE:  MIT
# DATE:     2023-03-22
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
  library(gt)
  

# GLOBAL VARIABLES --------------------------------------------------------

# SI specific paths/functions  
load_secrets()

ref_id <- "37dd8f05"

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

mfl <- data_folder %>% 
  return_latest("USAID Only") %>% 
  read_xlsx(skip = 2) %>% 
  janitor::clean_names()

site_tab <- mfl %>% 
  mutate(dhis2_hf_type = str_replace(dhis2_hf_type, "HC", "Health Center")) %>% 
  mutate(dhis2_hf_type = str_replace(dhis2_hf_type, "RRH", "Regional Referral Hospital")) %>% 
  
# mutate(dhis2_hf_type = recode(dhis2_hf_type, "Health Center" = "HC")) %>% 
  count(dhis2_hf_type) %>% 
  arrange(desc(n)) %>% 
  rename(`Facility Type` = dhis2_hf_type,
          `Frequency` = n)

site_tab %>% 
  gt() %>% 
  tab_header(
    title = "FY22 USAID/UGANDA PARTNER SUPPORTED SITE BREAKDOWN",
    subtitle = glue("Based on COP21 Targets and IP Submission")
  )




