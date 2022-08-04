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

# Set paths  
data   <- "Data"
dataout <- "Dataout"
images  <- "Images"
graphs  <- "Graphics"
goal <- 95
pepfar_countries <- pepfar_country_list

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

# in 2020, which ous met which goals?

# 2020
ggplot(unaids_newvars %>%
         filter(year == 2021,) %>%
         group_by(indicator) %>%
         arrange(indicator, estimate) %>%
         unite("ind_country", indicator, country, sep = "_", remove = FALSE) %>%
         data.frame() %>%
         mutate(
           ind_country = factor(ind_country, levels = ind_country)) %>%
         drop_na(estimate), 
       aes(y = ind_country, x = estimate, fill = indicator)) +
  geom_col() +
  geom_vline(xintercept = 95, 
             color = usaid_black,
             linetype = "dotted", 
             alpha = 0.9) +
  annotate("text",
            x = 95, y = 5,
            label = "95%",
            size = 3,
            color = usaid_black) +
  geom_text(aes(y = ind_country, x = estimate,
                label = if_else(estimate >= 95.0, as.character(estimate), "")),
            hjust = -0.4, vjust = 0.3,
            position = position_jitter(width = -0.3),
            size = 3) +
  facet_wrap(~indicator, 
             scales = "free") +
  scale_fill_manual(
    values = c(old_rose_light, burnt_sienna_light, genoa_light), 
    labels = NULL) +
  theme(strip.background = element_blank(), 
        legend.position = "none") +
  labs( 
    x = NULL,
    y = NULL,
    color = NULL)


# 2021
ggplot(unaids_newvars %>%
         filter(year == 2021), 
       aes(x = estimate, y = country, group = indicator, fill = indicator)) +
  geom_col() +
  facet_wrap(~indicator, 
             scales = "free_y")

# what were the gaps from the goal in each OU in 2020 and 2021?
# x = year, y = gap_from_goal, group (facet?) = country























