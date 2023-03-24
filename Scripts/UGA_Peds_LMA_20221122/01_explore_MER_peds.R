# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  explore MER Peds VLS rates
# REF ID:   e410ba62 
# LICENSE:  MIT
# DATE:     2022-11-22
# UPDATED:
# Note: Genie UGA filters to TX_CURR and PVLS for USAID

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
  return_latest("MER_Structured_Datasets_PSNU_IM_FY21-23_20230210_v1_1_Uganda")


#store metadata
get_metadata(msd_path, caption_note = "Created by: OHA SI Team
             LMA Presentation ATD Meeting 11-28-2022")


ref_id <- "e410ba62"

# IMPORT ------------------------------------------------------------------


#read MSD
df_msd <- msd_path %>% 
  read_psd()

# MUNGE -------------------------------------------------------------------

df_viz_snu <- df_msd %>% 
  clean_indicator() %>% 
  clean_agency() %>% 
  filter(
   # fiscal_year == metadata$curr_fy,
    funding_agency == "USAID",
    indicator %in% c("TX_CURR", "TX_PVLS_D", "TX_PVLS"),
    standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/Indication/HIVStatus")
  ) %>% 
 # filter(fiscal_year == metadata$curr_fy) %>% 
  group_by(fiscal_year, funding_agency, snu1, indicator) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  filter(fiscal_year != 2021) %>% 
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


df_age_viz <- df_msd %>% 
  clean_indicator() %>% 
  clean_agency() %>% 
  filter(
    #fiscal_year == metadata$curr_fy,
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


df_all <- df_msd %>% 
  clean_indicator() %>% 
  clean_agency() %>% 
  filter(
    fiscal_year %in% c(2022, 2023),
    funding_agency == "USAID",
    indicator %in% c("TX_CURR", "TX_PVLS_D", "TX_PVLS"),
    standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/Indication/HIVStatus")
  ) %>% 
  group_by(fiscal_year, funding_agency, snu1, indicator, age_2019) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
  reshape_msd() %>% 
  select(-period_type) %>% 
  pivot_wider(names_from = indicator, values_from = value) %>% 
  group_by(funding_agency, snu1, age_2019) %>% 
  mutate(TX_CURR_LAG2 = lag(TX_CURR, 2, order_by = period)) %>% 
  relocate(TX_CURR_LAG2, .before = TX_CURR) %>% 
  filter(!is.na(TX_PVLS_D), TX_PVLS_D > 0) %>% 
  group_by(period, funding_agency, snu1, age_2019) %>% 
  summarise(across(starts_with("TX"), sum, na.rm = T), .groups = "drop") %>% 
  mutate(VLC = TX_PVLS_D / TX_CURR_LAG2,
         VLS = TX_PVLS/TX_PVLS_D) %>% 
  filter(
    #period == metadata$curr_pd,
    age_2019 %in% c("<01", "01-04", "05-09", "10-14", "15-19"))


# VIZ -------------------------------------------------------------


# Small multiples 

# snu VLS over time

df_viz_snu %>% 
  ggplot(aes(period, VLS, group = snu1, color = scooter_med, fill = scooter_med)) +
  geom_area(alpha = .4, size = .9, position = "identity") +
  geom_point(aes(y = endpoints), na.rm = TRUE) +
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


# VLS by Age over time 

df_age_viz %>% 
  ggplot(aes(period, VLS, group = age_2019, color = scooter_med, fill = scooter_med)) +
  geom_area(alpha = .4, size = .9, position = "identity") +
 # geom_point(aes(y = endpoints), na.rm = TRUE) +
  geom_text(aes(label = percent(VLS, 1)), na.rm = TRUE,
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

# dumbbells for comparison by Q3 and q4 across age and SNU

pvls_num <- df_all %>% 
  filter(period %in% c("FY23Q1", "FY22Q4")) %>% 
  select(c(period, funding_agency, snu1, age_2019, TX_PVLS)) 

df_all %>% 
  filter(period %in% c("FY23Q1", "FY22Q4"),
         age_2019 != "<01") %>% 
  #mutate(VLS = ifelse(age_2019 == "<01", NA, VLS)) %>% 
  select(c(period, funding_agency, snu1, age_2019, VLS)) %>% 
  pivot_wider(names_from= period, values_from = VLS) %>% 
  mutate(fill_color = ifelse(`FY23Q1` < `FY22Q4`, "#DD052A", "#009EE3")) %>%
  pivot_longer(`FY22Q4`:`FY23Q1`, names_to = "period", values_to = "VLS") %>% 
  mutate(fill_color = ifelse(period == "FY22Q4", trolley_grey_light, fill_color)) %>% 
  left_join(pvls_num, by = c("period", "funding_agency", "snu1", "age_2019")) %>% 
  ggplot(aes(VLS, age_2019)) +
  geom_path(color = "gray50") +
  geom_point(aes(size = TX_PVLS), fill = "white") +
  geom_point(aes(size = TX_PVLS, fill = fill_color, color = "white"), shape = 21) +
  geom_text(aes(label = percent(VLS, 1)), family = "Source Sans Pro", 
            color = trolley_grey, hjust = 0, vjust = -1.2, na.rm = TRUE) +
  facet_wrap(~fct_reorder2(snu1, period, VLS, .desc = TRUE)) +
  scale_color_identity() +
  scale_fill_identity() +
  # scale_fill_manual(values = c("FY22Q3" = trolley_grey_light, "FY22Q4" = "#8AB75E")) +
  # scale_color_manual(values = c("FY22Q3" = trolley_grey_light, "FY22Q4" = "#8AB75E")) +
 # scale_color_identity() +
  scale_x_continuous(label = percent) +
  #scale_size_area() +
  scale_size_continuous(range = c(2,8)) +
  expand_limits(x = 1) +
  si_style_xgrid() +
  labs(y = NULL,
       title = glue("Across most SNUs and age groups, viral load suppression decreased for CLHIV from FY22Q3 to FY22Q$" %>% toupper()),
       #subtitle = "Uganda COP22 DSD Analysis | USAID",
       caption = glue("{metadata$caption}")
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    #axis.text.x = element_blank(),
    plot.title = element_markdown()
  )

si_save("Graphics/03_vls_by_age_snu_FY23Q1.svg")  
  


# _--------------------------------------------

# # Creates  Small multiples by SNU 
# ggplot(data = df_all) +
#   geom_point(mapping = aes(x = age_2019, y = VLS ,col= period)) +
#   facet_wrap(~snu1) +
#   si_style_xgrid() +
#   labs(y = NULL,
#        title = "USAID Uganda TX_ PVLS Trends in FY22",
#        subtitle = "TX_PVLS declined <1 - 19 age bands",
#        caption = " TX_ PVLS Trends
#            Source: MSD FY22Q4, 23 Nov 2022")
# 
# ggplot(data = df_all) +
#   geom_point(mapping = aes(x = period, y = VLS ,col= age_2019)) +
#   facet_wrap(~snu1) +
#   si_style_xgrid() +
#   labs(y = NULL,
#        title = "USAID Uganda TX_ PVLS Trends in FY22",
#        subtitle = "TX_PVLS declined <1 - 19 age bands",
#        caption = " TX_ PVLS Trends
#            Source: MSD FY22Q4, 23 Nov 2022")
# 
# 
# ggplot(data = df_all,
#        mapping = aes(x = period, y = VLS, col = age_2019)) +
#   geom_line(mapping = aes(group = age_2019),
#             alpha =.9)+
#   #geom_point(alpha = .9) 
#   facet_wrap(~snu1) +
#   #si_style_xgrid() +
#   labs(y = NULL,
#        title = "USAID Uganda TX_ PVLS Trends in FY22",
#        subtitle = "TX_PVLS declined <1 - 19 age bands",
#        caption = " TX_ PVLS Trends
#            Source: MSD FY22Q4, 23 Nov 2022")
  
  
  