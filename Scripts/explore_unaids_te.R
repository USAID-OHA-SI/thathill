# PROJECT: thathill
# AUTHOR: J. Hoehner & T. Essam | USAID
# PURPOSE: To munge and visualize achievement and gaps in UNAIDS 2021 data 
# REF ID:   15cbf641 
# LICENSE: MIT
# DATE: 2022-08-04
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
load_secrets()

# REF ID for plots
ref_id <- "251b5696"

# load data ====================================================================  

unaids <- read_sheet("1ZfxOScjuLnoGiXcsmfNnWn4EsqtxyFrvql8ERqdWSjk") %>%
  clean_names() 

  ymax <- 2021
  ymin <- 2020

# MUNGE and FILTER --------------------------------------------------------

  # Focusing on 2020 and 2021 data
unaids %>% 
  count(year) %>% 
  slice_tail(n = 2)

df_viz <- unaids %>% 
  filter(year %in% c(ymin, ymax), 
         indicator %in% c("KNOWN_STATUS", "KNOWN_STATUS_ON_ART", "ON_ART_VLS"),
         age == "All",
         sex == "All",
         pepfar == "TRUE", 
         indic_type == "Percent") %>% 
  rename(est = estimate, 
         lb = lower_bound,
         ub = upper_bound)


# Want to create a strip plot showing how OUs have moved over the last year
# in terms of the 95s

# things to consider for facets - how should the 95 labels be ordered?
# tag any ou missing at least one of the values
# tag the positive and negative movers, 
# make some bars connecting plots across years

df_viz <- 
  df_viz %>% 
  group_by(country, year) %>% 
  mutate(full_cascade_count = sum(!is.na(est)),
         full_cascade = ifelse(full_cascade_count == 3, 1, 0),
         ou_order_val = case_when(
           indicator == "KNOWN_STATUS" & year == ymax ~ est
           )
         ) %>% 
  group_by(country) %>% 
  fill(ou_order_val, .direction = "downup") %>% 
  ungroup() %>% 
  group_by(country, indicator) %>% 
  mutate(delta = est - lag(est),
         gain = case_when(
           delta > 0 & year == ymax ~ "gain",
           delta < 0 & year == ymax ~ "loss",
           TRUE ~ "base"
         ),
         ind_facet = str_replace_all(indicator, "_", " ")) %>% 
  ungroup()



# Viz --------------------------------------------------------------------------

  # Dumbell plot look at things (need to group the OUs by gain/loss)
  df_viz %>% 
    filter(full_cascade == 1) %>% 
    # Make sure factor is set after sort o/wise it jacks up order
    mutate(ou_order = fct_reorder(country, ou_order_val)) %>% 
    ggplot(aes(y = ou_order, x = est, group = year, color = factor(year), shape = gain)) +
    geom_vline(xintercept = 95) +
    geom_line(aes(group = country)) +
    geom_point(size = 3) +
    facet_wrap(~ind_facet) +
    si_style_xgrid() +
    scale_x_continuous(limits = c(60, 100)) +
    scale_color_manual(values = c("2020" = old_rose, "2021" = scooter))

si_save("Graphics/cascade_delta_mockup.svg")

  # Let's move to a heatmapish type view
df_viz %>% 
  clean_countries("country") %>% 
  filter(year == ymax,
         full_cascade == 1) %>% 
  mutate(ou_order = fct_reorder(country, ou_order_val)) %>% 
  ggplot(aes(y = ou_order, x = ind_facet)) +
  geom_tile(color = "white", aes(fill = est)) +
  geom_text(aes(label = est, color = ifelse(est > 70, "white", grey90k)), 
            size = 8/.pt,
            family = "Source Sans Pro") +
  scale_fill_viridis_c(option = "D", direction = -1, na.value = grey10k) +
  si_style_nolines() +
  scale_color_identity() +
  scale_x_discrete(position = "top") +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "none") 
  #toggle for gains/loss
  #facet_wrap(~gain)

# Strip plots with CIs
# Let's move to a heatmapish type view
df_viz %>% 
  clean_countries("country") %>% 
  filter(year == ymax,
         full_cascade == 1) %>% 
  mutate(ou_order = fct_reorder(country, ou_order_val)) %>% 
  ggplot(aes(y = ou_order)) +
  geom_rect(aes(ymin = -Inf, ymax = Inf, xmin = 95, xmax = 100), fill = grey10k, alpha = 0.05) +
  geom_vline(xintercept = 95, linetype = "dotted") +
  geom_segment(aes(x = lb, xend = ub, yend = ou_order), size = 1, lineend = "butt",
               color = grey50k) +
  #geom_point(aes(x = est)) +
  facet_wrap(~ind_facet) +
  si_style_xgrid()


