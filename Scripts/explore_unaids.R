# PROJECT: thathill
# AUTHOR: J. Hoehner | USAID
# PURPOSE: To munge and visualize achievement and gaps in UNAIDS 2021 data 
# REF ID:   15cbf641 
# LICENSE: MIT
# DATE: 2022-08-15
# NOTES:

# setup ========================================================================

# Libraries
library(glamr)
library(glitr)
library(tidyverse)
library(tidymodels)
library(tidytext)
library(janitor)
library(ggplot2)
library(googlesheets4)
library(assertr)
library(extrafont)

tidymodels_prefer()

# Set paths  
data   <- "Data"
dataout <- "Dataout"
images  <- "Images"
graphs  <- "Graphics"
ref_id <- "15cbf641"
goal <- 95
pepfar_countries <- pepfar_country_list
load_secrets("email")
set.seed(22315)
ymax <- 2021
ymin <- 2020

# load data ====================================================================  

# cascade
cascade <- read_sheet("19oSbysX6FvMJ38--HBPE_eME4VlR2edAiYJHtZpK3as") %>%
  clean_names() 

# verify that there are no differences in our team pepfar country 
# list and data we have included as pepfar supported here
peps_unaids <- cascade %>%
  filter(pepfar == "TRUE") %>%
  select(country, iso3)

# i expect that all countries listed as pepfar in the unaids data 
# will be listed in pepfar_countries
diff <- as.character(setdiff(peps_unaids$country, 
                             pepfar_countries$country))

verify(diff, length(diff) == 0)

# munge ========================================================================

cascade_new <- cascade %>%
  clean_countries("country") %>% 
  filter(year == ymax,
         sex == "all", 
         age == "all", 
         pepfar == "TRUE", 
         cascade_mkr %in% c("1st 90", "2nd 90", "3rd 90")) %>%
  group_by(country, year) %>%
  mutate(
    estimate = as.numeric(estimate),
    # binary var. flagging OUs with complete cascade for 2021,
    # 1 = has, 0 = doesn't
    # TODO: add unit test
    complete_cascade_count = sum(!is.na(estimate)),
    complete_cascade = if_else(complete_cascade_count == 3, 1, 0),
    ou_order_val = case_when(
    indicator == "Percent Known Status of PLHIV" & 
    year == ymax ~ estimate)) %>% 
  filter(complete_cascade == 1) %>%
  group_by(country) %>% 
  fill(ou_order_val, .direction = "downup")

# What if we incorporate an additional metric into the mix that captures how large
# the error measurement is

# % of error associated with country?
# Let's start with a simple method, we'll take the point estimate - min value

# How do the OUs cluster?
casc_clust <- cascade_new %>% 
  mutate(
    estimate = as.numeric(estimate),
    error_var = as.numeric(estimate - lower_bound), 
    gap_from_95 = as.numeric(estimate - goal)) %>% 
  select(country, indicator, estimate, error_var, gap_from_95) 

# Split table apart then reappend
tmp1 <- casc_clust %>% 
  select(country, indicator, estimate = error_var) %>% 
  mutate(indicator = str_c(indicator, "_err"))

tmp2 <- casc_clust %>% 
  select(country, indicator, estimate = gap_from_95) %>% 
  mutate(indicator = str_c(indicator, "_gap"))

casc_clust_error <- casc_clust %>% 
  select(-c(error_var, gap_from_95)) %>% 
  bind_rows(tmp1) %>% 
  bind_rows(tmp2) %>% 
  distinct() %>%
  pivot_wider(names_from = indicator, values_from = estimate) %>%
  clean_names() %>%
  ungroup()

# clustering ===================================================================

# k-means
mod_clust <- kmeans(casc_clust_error %>%
                      select(-country), centers = 5)
summary(mod_clust)

tidied_clust <- tidy(mod_clust)

# Augment back results to data

augment(mod_clust, casc_clust_error) %>% 
  ggplot(
    aes(percent_known_status_of_plhiv-goal, 
        percent_on_art_with_known_status-goal, 
        color = .cluster)) + 
  annotate("rect", fill = grey10k, xmin = -5, xmax = 5, 
           ymin = -5, ymax = 5, alpha = 0.5) +
  geom_vline(xintercept = -5, linetype = "dotted", color = glitr::grey40k) +
  geom_hline(yintercept = -5, linetype = "dotted", color = glitr::grey40k) +
  geom_point(aes(size = abs(percent_on_art_with_known_status-goal))) +
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

# PCA

# looking at the errors only suggests that there are 3 PCs:
# PC1: countries which have met all cascade goals
# PC2: countries which have met goal 2 but not 1 and 3
# PC3: countries which have only met goal 1

casc_clust_error_only <- casc_clust_error %>%
  select(country, ends_with("_err"))

pca_rec_err <- recipe(~., data = casc_clust_error_only) %>%
  update_role(country, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

pca_prep_err <- prep(pca_rec_err)
tidied_pca_err <- tidy(pca_prep_err, 2)

tidied_pca_err %>%
  filter(component %in% paste0("PC", 1:3)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL) 

juice(pca_prep_err) %>%
  ggplot(aes(PC1, PC2, label = country)) +
  geom_text(check_overlap = TRUE, hjust = "inward", 
            family = "Source Sans Pro") +
  si_style() +
  scale_fill_manual(labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x =  NULL) +
  labs(x = "High Achievement Across All 95S", 
       y = "High Achievement On 1st and 3rd 95s", 
       title = "SAMPLE PCA OF 95s CASCADE",
       caption = glue::glue("Source: UNAIDS 2022 | {ref_id}"))

juice(pca_prep_err) %>%
  ggplot(aes(PC2, PC3, label = country)) +
  geom_text(check_overlap = TRUE, hjust = "inward", 
            family = "Source Sans Pro") +
  si_style() +
  scale_fill_manual(labels = NULL) +
  theme(legend.position = "none") +
  labs(x = "High Achievement On Goals 1 and 3 only", 
       y = "High Achievement On Goal 1 only", 
       title = "SAMPLE PCA OF 95s CASCADE",
       caption = glue::glue("Source: UNAIDS 2022 | {ref_id}"))

juice(pca_prep_err) %>%
  ggplot(aes(PC1, PC3, label = country)) +
  geom_text(check_overlap = TRUE, hjust = "inward", 
            family = "Source Sans Pro") +
  si_style() +
  scale_fill_manual(labels = NULL) +
  theme(legend.position = "none") +
  labs(x = "High Achievement Across All Goals", 
       y = "High Achievement On Goal 1 only", 
       title = "SAMPLE PCA OF 95s CASCADE",
       caption = glue::glue("Source: UNAIDS 2022 | {ref_id}"))


# looking at the errors only suggests that there are 3 PCs:
# PC1: countries which have high errs on all cascade goals
# PC2: countries which have not met/have high errs on goal 2 but not 1 and 3
# PC3: countries which have not met/ have high errs on goal 1 only
# We can see this when we look at a scatter plot too
# I think it would be easier to interpet the ests directly

# I would expect to see the same relationships in reverse
# when we look at only the estimates directly, let's see if that holds: 

casc_clust_ests <- casc_clust_error %>%
  select(country, !ends_with("_err"))

pca_rec_ests <- recipe(~., data = casc_clust_ests) %>%
  update_role(country, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

pca_prep_ests <- prep(pca_rec_ests)
tidied_pca_ests <- tidy(pca_prep_ests, 2)

tidied_pca_ests %>%
  filter(component %in% paste0("PC", 1:3)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL)

# Indeed we see the same relationships in the PCs here:
# PC1: countries which have have high estimates on all goals
# PC2: countries which have only met/have high ests on goals 1 and 3 and not 2
# PC3: countries which have only met/have high ests goal 1

# Zooming in on these components, which goals are contributing most
# to each relationship?

tidied_pca_ests %>%
  filter(component %in% paste0("PC", 1:3)) %>%
  group_by(component) %>%
  top_n(3, abs(value)) %>%
  ungroup() %>%
  mutate(terms = reorder_within(terms, abs(value), component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  scale_y_reordered() +
  labs(
    x = "Absolute value of contribution",
    y = NULL, fill = "Positive?")

# PC1: we see that all goals are contributing equally. 
# This makes sense because PC1 is about high estimates 
# across all goals and we see that

# PC2: we see that "Percent on Art with Known Status" (goal 2)
# contributes the most and this makes sense because this
# PC is all about having high estimates on 1 and 3 and not 2

# PC3: we see that high estimates on goals 1 and 3 contribute 
# the most to this PC and this makes sense because this describes
# countries which have only met goal 1 and not goals 2 and 3, specifically 
# not 3 which we would expect would be harder to meet if 2 is not met

juice(pca_prep_ests) %>%
  ggplot(aes(PC1, PC2, label = country)) +
  geom_text(check_overlap = TRUE, hjust = "inward", 
            family = "Source Sans Pro") +
  si_style() +
  scale_fill_manual(labels = NULL) +
  theme(legend.position = "none") +
  labs(x = "High Achievement Across All Goals", 
       y = "High Achievement On Goals 1 and 3 only", 
       title = "SAMPLE PCA OF 95s CASCADE",
       caption = glue::glue("Source: UNAIDS 2022 | {ref_id}"))

juice(pca_prep_ests) %>%
  ggplot(aes(PC2, PC3, label = country)) +
  geom_text(check_overlap = TRUE, hjust = "inward", 
            family = "Source Sans Pro") +
  si_style() +
  scale_fill_manual(labels = NULL) +
  theme(legend.position = "none") +
  labs(x = "High Achievement On Goals 1 and 3 only", 
       y = "High Achievement On Goal 1 only", 
       title = "SAMPLE PCA OF 95s CASCADE",
       caption = glue::glue("Source: UNAIDS 2022 | {ref_id}"))

juice(pca_prep_ests) %>%
  ggplot(aes(PC1, PC3, label = country)) +
  geom_text(check_overlap = TRUE, hjust = "inward", 
            family = "Source Sans Pro") +
  si_style() +
  scale_fill_manual(labels = NULL) +
  theme(legend.position = "none") +
  labs(x = "High Achievement Across All Goals", 
       y = "High Achievement On Goal 1 only", 
       title = "SAMPLE PCA OF 95s CASCADE",
       caption = glue::glue("Source: UNAIDS 2022 | {ref_id}"))

# Let's look at the gaps from achievement and see how countries with similar gaps
# cluster

casc_clust_gap <- casc_clust_error %>%
  select(country, ends_with("_gap"))

pca_rec_gap <- recipe(~., data = casc_clust_gap ) %>%
  update_role(country, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

pca_prep_gap <- prep(pca_rec_gap)
tidied_pca_gap <- tidy(pca_prep_gap, 2)

tidied_pca_gap %>%
  filter(component %in% paste0("PC", 1:3)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL) 

# PC1: The source of difference in this PC is a similarly (low) gap in achievement
# PC2: Source of difference in this PC is the difference in gaps in achievement in 
# 1st and 3rd 95s 
# PC3: Source of difference is the difference in gaps in achievement in first 95 relative
# to next two 95s

juice(pca_prep_gap) %>%
  ggplot(aes(PC1, PC2, label = country)) +
  geom_text(check_overlap = TRUE, hjust = "inward", 
            family = "Source Sans Pro") +
  si_style() +
  scale_fill_manual(labels = NULL) +
  theme(legend.position = "none", 
        axis.text.x =  NULL) +
  labs(x = "Gap In All 95s", 
       y = "Gap in 1st and 3rd 95s", 
       title = "SAMPLE PCA OF 95s CASCADE",
       caption = glue::glue("Source: UNAIDS 2022 | {ref_id}"))

juice(pca_prep_gap) %>%
  ggplot(aes(PC2, PC3, label = country)) +
  geom_text(check_overlap = TRUE, hjust = "inward", 
            family = "Source Sans Pro") +
  si_style() +
  scale_fill_manual(labels = NULL) +
  theme(legend.position = "none") +
  labs(x = "Gap in 1st and 3rd 95s", 
       y = "Gap in 1st 95 compared to 2nd and 3rd 95s", 
       title = "SAMPLE PCA OF 95s CASCADE",
       caption = glue::glue("Source: UNAIDS 2022 | {ref_id}"))

juice(pca_prep_gap) %>%
  ggplot(aes(PC1, PC3, label = country)) +
  geom_text(check_overlap = TRUE, hjust = "inward", 
            family = "Source Sans Pro") +
  si_style() +
  scale_fill_manual(labels = NULL) +
  theme(legend.position = "none") +
  labs(x = "Gap In All 95s", 
       y = "Gap in 1st 95 compared to 2nd and 3rd 95s", 
       title = "SAMPLE PCA OF 95s CASCADE",
       caption = glue::glue("Source: UNAIDS 2022 | {ref_id}"))

