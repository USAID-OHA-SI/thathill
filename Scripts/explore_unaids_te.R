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
library(scales)

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

# Cascade threshold
thresh <- 95

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
# Reflect axis on RHS
ou_order_rhs <- df_viz %>% 
  clean_countries("country") %>% 
  filter(year == ymax,
         full_cascade == 1) %>% 
  mutate(ou_order = fct_reorder(country, ou_order_val)) 
ou_order_rhs <- levels(ou_order_rhs$ou_order)

ou_order_rhs_labs <- df_viz %>% 
  clean_countries("country") %>% 
  filter(year == ymax,
         full_cascade == 1) %>% 
  mutate(ou_order = fct_reorder(country, ou_order_val)) %>% 
  distinct(ou_order)


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
  geom_point(aes(x = est, color = ifelse(est >= 95, scooter, old_rose))) +
  facet_wrap(~ind_facet) +
  scale_color_identity() +
  scale_x_continuous(label = label_number(suffix = "%"))+
  si_style_xgrid() +
  labs(caption = glue::glue("UNAIDS 2022 Data | {ref_id}"), x = NULL, y = NULL,
       title = "NO PEPFAR SUPPORTED OPERATING UNIT HAS REACHED 95-95-95 AS OF 2021")

  si_save("Graphics/UNAIDS_95s_2022_error_bars.svg")
  

# CLUSTERING --------------------------------------------------------------

  # What if we incorporate an additional metric into the mix that captures how large
  # the error measurement is
  
  # % of error associated with country?
  # Let's start with a simple method, we'll take the point estimate - min value
  
  # How do the OUs cluster?
  df_clust <- df_viz %>% 
    clean_countries("country") %>% 
    filter(year == ymax,
           full_cascade == 1) %>% 
    mutate(error_var = est - lb) %>% 
    select(country, est, ind_facet, error_var) 
  
  # Split table apart then reappend
  tmp1 <- df_clust %>% 
    select(country, ind_facet, est = error_var) %>% 
    mutate(ind_facet = str_c(ind_facet, "_err"))
    
  df_clust_error <- 
    df_clust %>% 
    select(-error_var) %>% 
    bind_rows(tmp1) %>% 
    spread(ind_facet, est)

  set.seed(41)
  mod_clust <-  kmeans(select(df_clust_error, -country), centers = 5)    
  summary(mod_clust)  

  library(broom)
  tidy(mod_clust)

  # Augment back results to data
  
  df_3d <- augment(mod_clust, df_clust_error)
  augment(mod_clust, df_clust_error) %>% 
    ggplot(aes(`KNOWN STATUS`-thresh, `KNOWN STATUS ON ART`-thresh, color = .cluster)) + 
    annotate("rect", fill = grey10k, xmin = -5, xmax = 5, 
             ymin = -5, ymax = 5, alpha = 0.5) +
    geom_vline(xintercept = -5, linetype = "dotted", color = glitr::grey40k) +
    geom_hline(yintercept = -5, linetype = "dotted", color = glitr::grey40k) +
    geom_point(aes(size = abs(`ON ART VLS`-thresh))) +
    ggrepel::geom_text_repel(aes(label = country), size = 8/.pt) +
    scale_size_binned() +
    scale_color_discrete() +
    si_style() +
    labs(x = "Gap to Known Status Goal", y = "Gap to ART Goal", 
         size = "Gap to VLS Goal",
         color = "Cluster",
         title = "SAMPLE CLUSTER ANALYSIS OF 95s CASCADE",
         caption = glue::glue("Source: UNAIDS 2022 | {ref_id}")) +
    guides(color = guide_legend(nrow = 2, byrow = TRUE))
  
  ggsave("Images/UNAIDS_cascade_cluster.png", height = 8, width = 10.5)

  

  
  # PCA approach
  pca_fit <- df_clust_error %>% 
    select(-country) %>% 
    prcomp(scale = T)
  
  pca_fit %>%
    augment(df_clust_error) %>% # add original dataset back in
    ggplot(aes(.fittedPC1, .fittedPC2)) + 
    geom_point(size = 1.5) +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    ggrepel::geom_label_repel(aes(label = country)) 

  
  library(plotly)
  
  tmp <- plot_ly(data = df_3d, x = ~`KNOWN STATUS`, y = ~`KNOWN STATUS ON ART`,
          z = ~`ON ART VLS`, type = "scatter3d", text = ~country, color = ~.cluster )
  tmp
  
  # prep data for cluster dd

  df_dd <- df_3d %>% 
    column_to_rownames("country") %>% 
    select(-.cluster) %>% 
    mutate(across(1:3, ~ .x - thresh))
  
  # cluster dendogram
  dd <- dist(scale(df_dd), method = "euclidean")
  hc <- hclust(dd, method = "ward.D2")
  plot(hc)
  rect.hclust(hc, k = 5, border = 1:5)
  
  #convert into dendogram
  hcd <- as.dendrogram(hc)
  plot(hcd, horiz = TRUE)  
  
  # ggplot2 method
  library(ggdendro)
  cut_hc <- cutree(hc, k = 6)

  hcdata <- dendro_data(hc, type = "rectangle")  
  
  ggplot() +
    geom_segment(data = segment(hcdata), 
                 aes(x = x, y = y, xend = xend, yend = yend)
    ) +
    geom_text(data = label(hcdata), 
              aes(x = x, y = y, label = label, hjust = 0), 
              size = 3
    ) +
    coord_flip() +
    scale_y_reverse(expand = c(0.2, 0))
  
  
  library(dendextend)
  dend <- as.dendrogram(hc)
  plot(dend)
  dend2 <- cut(dend, h = 3)
  plot(dend2$upper)
  
  dend  
  dend %>% head  
  