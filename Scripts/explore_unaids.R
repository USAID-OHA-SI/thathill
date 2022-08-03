# PURPOSE: 
# AUTHOR: J. Hoehner | SI, 
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

  # Set paths  
  data   <- "Data"
  dataout <- "Dataout"
  images  <- "Images"
  graphs  <- "Graphics"

  # Functions  


# load data ====================================================================  
unaids <- read_sheet("1ZfxOScjuLnoGiXcsmfNnWn4EsqtxyFrvql8ERqdWSjk") %>%
    clean_names() %>%
    filter(year %in% c("2020", "2021"))
    

# munge ========================================================================

#  

# viz ==========================================================================
