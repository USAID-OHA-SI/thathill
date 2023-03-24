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

df_service <- data_folder %>% 
  return_latest("CALHIV Tools") %>% 
  read_xlsx(sheet = "Service Layering") %>% 
  janitor::clean_names()

 # MUNGE -------------------------------------------------------------------

df_tidy <- df_service %>% 
  rename(service_type = ...1,
         data_type = xxxx) %>% 
  filter(data_type == "%") %>% 
    mutate(`G2G_  Mbarara` = as.numeric(`G2G_  Mbarara`),
           `G2G_  Moroto` = as.numeric(`G2G_  Moroto`)) %>%
pivot_longer(cols = 3:18, names_to = "region", values_to = "coverage") %>% 
  mutate(region = str_replace(region, "_ ", ""))


# VIZ ---------------------------------------------------------------------

df_tidy %>% 
  ggplot(aes(x = region, 
             y = service_type, 
             fill = coverage)) +
  geom_tile(color = "white", 
            size = 0.9) +
  geom_text(aes(label = percent(coverage, 1),
                color = if_else(coverage <= 0.25, grey90k, "white")),
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
       subtitle = "From November patient audit tool",
       caption = metadata$caption) 

si_save("Graphics/04_heatmap.svg")
s