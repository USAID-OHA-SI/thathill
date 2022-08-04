# PROJECT: thathill
# AUTHOR: J. Hoehner | USAID
# PURPOSE: To mune and visualize achievement and gaps in UNAIDS 2021 data 
# REF ID:   15cbf641 
# LICENSE: MIT
# DATE: 2022-08-03
# NOTES:

# setup ========================================================================

# Libraries
library(glamr)
library(glitr)
library(tidyverse)
library(janitor)
library(ggplot2)
library(googlesheets4)
library(assertr)
library(extrafont)

# Set paths  
data   <- "Data"
dataout <- "Dataout"
images  <- "Images"
graphs  <- "Graphics"
goal <- 95
pepfar_countries <- pepfar_country_list
loadfonts()
load_secrets("email")

# load data ====================================================================  

unaids <- read_sheet("1ZfxOScjuLnoGiXcsmfNnWn4EsqtxyFrvql8ERqdWSjk") %>%
  clean_names() 

# verify that there are no differences in our team pepfar country 
# list and data we have incldued as pepfar supported here
peps_unaids <- unaids %>%
  filter(pepfar == "TRUE") %>%
  select(country, iso)

# i expect that all countries listed as pepfar in the unaids data 
# will be listed in pepfar_countries
diff <- as.character(setdiff(peps_unaids$country, 
                             pepfar_countries$country))

verify(diff, length(diff) == 0)

# munge ========================================================================

# filter for year, age, sex, pepfar status, indicator, ind. type
unaids_filt <- unaids %>%
  filter(year %in% c("2020", "2021"), 
         # PLHIV base
         indicator %in% c("KNOWN_STATUS", "KNOWN_STATUS_ON_ART", "ON_ART_VLS"),
         age == "All",
         sex == "All",
         pepfar == "TRUE", 
         indic_type == "Percent")

# where are estimates for the 95s missing?
missing_ests <- unaids_filt %>%
  filter(is.na(estimate == TRUE))

tab <- tabyl(missing_ests, country, indicator)

# add new variables
unaids_newvars <- unaids_filt %>%
  select(year, iso, country, indicator, age, sex, estimate) %>%
  group_by(country,year,indicator, age, sex) %>%
  mutate(
    indicator = as_factor(indicator), 
    country = as_factor(country),
    estimate = as.numeric(estimate),
    # col for achievement
    achieved = estimate >= goal,
    # Col for gap from achievment for OUs which have not met 95
    gap_from_goal = as.numeric(
      if_else(achieved == FALSE,
              goal-estimate, 0)),
    # new cols to indicate achievement on each 95
    achieved_1 = as.numeric(if_else(
      indicator == "KNOWN_STATUS" & achieved == TRUE, 
      1, 0, missing = 999)),
    achieved_2 = as.numeric(if_else(
      indicator == "KNOWN_STATUS_ON_ART" & achieved == TRUE, 
      1, 0, missing = 999)),
    achieved_3 = as.numeric(if_else(
      indicator == "ON_ART_VLS" & achieved == TRUE, 
      1, 0, missing = 999)), 
    # new cols to indicate achievement of more than 1 95
    achieved_1_2 = as.numeric(if_else(
      (indicator == "KNOWN_STATUS" & achieved_1 == 1) & 
        (indicator == "KNOWN_STATUS_ON_ART" & achieved_2 == 1), 
      1, 0, missing = 999)),
    achieved_2_3 = as.numeric(if_else(
      (indicator == "KNOWN_STATUS_ON_ART" & achieved_2 == 1) & 
        (indicator == "ON_ART_VLS" & achieved_3 == 1), 
      1, 0, missing = 999)),
    achieved_1_3 = as.numeric(if_else(
      (indicator == "KNOWN_STATUS" & achieved_1 == 1) & 
        (indicator == "ON_ART_VLS" &  achieved_3 == 1), 
      1, 0, missing = 999)))

# viz ==========================================================================

# in 2021, which ous met which goals?

# 2021
ggplot(unaids_newvars %>%
         filter(year == 2021) %>%
         arrange(indicator, estimate) %>%
         unite("ind_country", indicator, country, sep = "", remove = FALSE) %>%
         data.frame() %>%
         mutate(
           ind_country = factor(ind_country, levels = ind_country),
           indicator = factor(indicator, labels = c("KNOWN_STATUS" = "Know Their HIV Status", 
                                                    "KNOWN_STATUS_ON_ART" = "On HIV Treatment", 
                                                    "ON_ART_VLS" = "Virally Suppressed"))) %>%
         drop_na(estimate), 
       aes(y = ind_country, x = estimate, fill = indicator)) +
  geom_col() +
  geom_text(aes(label = if_else(estimate >= 95.0, 
                                as.character(estimate), "")),
            position = position_stack(vjust = 0.5),
            size = 3, 
            color = "#FFFFFF") +
  facet_wrap(~indicator, 
             scales = "free") +
  si_style_xgrid() +
  scale_fill_manual(
    values = c("#002e24", "#0D6C5F", "#5CAC9E"), 
    labels = NULL) +
  theme(strip.background = element_blank(), 
        legend.position = "none") +
  labs( 
    x = NULL,
    y = NULL,
    color = NULL)

# what were the gaps from the goal in each OU in 2020 and 2021?
# x = year, y = gap_from_goal, group (facet?) = country

ggplot(unaids_newvars %>%
         filter(year == 2021) %>%
         arrange(indicator, gap_from_goal) %>%
         unite("ind_country", indicator, country, sep = "", remove = FALSE) %>%
         data.frame() %>%
         mutate(
           # add column with row number for correct sorting sort by and use names from country
           # num = row_number(),
           ind_country = factor(ind_country, levels = ind_country),
           indicator = factor(indicator, labels = c("KNOWN_STATUS" = "Know Their HIV Status", 
                                                    "KNOWN_STATUS_ON_ART" = "On HIV Treatment", 
                                                    "ON_ART_VLS" = "Virally Suppressed"))) %>%
         drop_na(estimate), 
       aes(y = ind_country, x = gap_from_goal, fill = indicator)) +
  geom_col() +
  geom_text(aes(label = if_else(gap_from_goal >= 50.0, 
                                as.character(gap_from_goal), "")),
            position = position_stack(vjust = 0.5),
            size = 3, 
            color = "#FFFFFF") +
  facet_wrap(~indicator, 
             scales = "free_y") +
  si_style_nolines() +
  scale_fill_manual(
    values = c("#01564B", "#2D8073", "#5CAC9E"), 
    labels = NULL) +
  theme(strip.background = element_blank(), 
        legend.position = "none") +
  labs( 
    x = NULL,
    y = NULL,
    color = NULL)























