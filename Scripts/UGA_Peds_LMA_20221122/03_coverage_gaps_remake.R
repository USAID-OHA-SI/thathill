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
             Note: NA's excluded from denominator, while all other answer types were included")


g_id <- '1y7r9BHJrpsbTYZuoTspIpLOIoD35CJUMFLp6QWv1Blc'

# IMPORT ------------------------------------------------------------------

df_tool <- data_folder %>% 
  return_latest("CALHIV _ Audit tool Raw data_Aug 29") %>% 
  read_xlsx() %>% 
  janitor::clean_names()


# Mutate to the table format for service layering

# filtering out NAs from total, but leaving NE/ND in the total

str(df_tool)

df_viz <- df_tool %>% 
  mutate(region = case_when(region %in% c("JInja_Region", "jinja_Region") ~ "Jinja_Region",
                            region %in% c("lira_Region") ~ "Lira_Region",
                            region %in% c("mbale_Region") ~ "Mbale_Region",
                            region %in% c("mbarara_Region") ~ "Mbarara_Region",
                            region %in% c("moroto_Region") ~ "Moroto_Region",
                            region %in% c("uPMB_LSDA_Region", "upMB_LSDA_Region", "upmB_LSDA_Region") ~ "UPMB_LSDA_Region",
                            TRUE ~ region)) %>% 
  select(-c(district, facility_name, art_no, sex, age, missing_data, all_services)) %>% 
  pivot_longer(cols = c(indext_testing_children_siblings:vls_1000), names_to = "indicator", values_to = 'answer') %>%
  mutate(answer = ifelse(answer == "y", "Y", answer)) %>% 
  count(region, indicator, answer) %>% 
  filter(answer != "NA") %>% 
  group_by(region, indicator) %>% 
  mutate(total = sum(n)) %>% 
  mutate(pct = n / total) %>% 
  ungroup() %>% 
  filter(answer == "Y") %>% 
  distinct() %>% 
  select(-c(answer, n, total)) %>% 
  pivot_wider(names_from = "region", values_from = "pct")


df_viz %>% 
  mutate(indicator = fct_relevel(indicator, c("community_contacting", "chw_attachment", "ovc_enrolement",
                                              "ovc_screening", "appt_keeping", "mmd_3", "tb_screening", "tpt",
                                              "iac_initiation", "dtg", "vls_1000","vl_bleeding", "vl_coverage", 
                                              "index_testing_sexual_partners", "indext_testing_children_siblings"))) %>%
  ggplot(aes(x = region, 
             y = indicator, 
             fill = pct)) +
  geom_tile(color = "white", 
            size = 0.9) +
  geom_text(aes(label = percent(pct, 1),
                color = if_else(pct <= 0.25, grey90k, "white")),
            size = 3) +
  scale_x_discrete(position = "top", 
                   guide = guide_axis(n.dodge = 2)) +
  scale_fill_si(palette = "scooters", discrete = FALSE) +
  scale_color_identity() +
  si_style_nolines() +
  theme(panel.background = element_rect(fill = "#f6f6f6", color ="white"),
        legend.position = "none") +
  labs(x = NULL, 
       y = NULL,
       title = "SERVICE COVERAGE GAPS ACROSS REGIONS",
       subtitle = "From August 2023 patient audit tool",
       caption = metadata$caption) 

si_save("Graphics/03_service_gap_remake.svg")
  
