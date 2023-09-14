# Load the required libraries
library(readxl)
library(dplyr)
#install.packages("reshape2")
library(reshape2)
library(ggplot2)
library(reshape2)

library(glamr)
library(glitr)
library(scales)
library(tidyverse)
library(scales)

#package for combining graphs
#install.packages("patchwork")
library(patchwork)
##install.packages("C:/Users/YourUsername/Downloads/patchwork.tar.gz", repos = NULL, type = "source")
#install.packages(reshape2)
# Read the Excel file
PLHIV_Estimates <- read_excel("Data/HIV age distribution trends.xlsx")



# Filter the data for Fiscal Year 2013
fy_2013_data <- PLHIV_Estimates %>%
  filter(FY == "2013")


# Convert Total, Male, and Female columns to numeric
(fy_2013_data) <- fy_2013_data %>%
  mutate_at(vars(Total, Male, Female), as.numeric)

# Restructure the dataframe to long format
fy_2013_long <- fy_2013_data %>%
  pivot_longer(cols = c(Male, Female), names_to = "Gender", values_to = "count")


# Collapse age bands
fy_2013_long_collapsed <- fy_2013_long %>%
  mutate(Age_band = ifelse(Age_band %in% c("50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+"), "50+", Age_band)) %>%
  group_by(FY, Age_band, Gender) %>%
  summarize(Total = sum(Total), count = sum(count))

# Define the custom order for Age_band
custom_order <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50+")

fy_2013_long_collapsed <- fy_2013_long %>%
  mutate(Age_band = factor(Age_band, levels = custom_order)) %>%
  group_by(FY, Age_band, Gender) %>%
  summarize(Total = sum(Total), count = sum(count)) %>%
  arrange(FY, Age_band, Gender)






# restructure 2022 data

# Filter the data for Fiscal Year 2022
fy_2022_data <- PLHIV_Estimates %>%
  filter(FY == "2022")


# Convert Total, Male, and Female columns to numeric
(fy_2022_data) <- fy_2022_data %>%
  mutate_at(vars(Total, Male, Female), as.numeric)

# Restructure the dataframe to long format
fy_2022_long <- fy_2022_data %>%
  pivot_longer(cols = c(Male, Female), names_to = "Gender", values_to = "count")


# Collapse age bands
fy_2022_long_collapsed <- fy_2022_long %>%
  mutate(Age_band = ifelse(Age_band %in% c("50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+"), "50+", Age_band)) %>%
  group_by(FY, Age_band, Gender) %>%
  summarize(Total = sum(Total), count = sum(count))

# Define the custom order for Age_band
custom_order <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50+")

fy_2022_long_collapsed <- fy_2022_long %>%
  mutate(Age_band = factor(Age_band, levels = custom_order)) %>%
  group_by(FY, Age_band, Gender) %>%
  summarize(Total = sum(Total), count = sum(count)) %>%
  arrange(FY, Age_band, Gender)



# Create the ggplot object with modified x-axis scale formatting
pyramid_plot_13 <- ggplot(fy_2013_long_collapsed, aes(x = Age_band, y = ifelse(Gender == "Male", -count, count), fill = Gender)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = "", hjust = ifelse(Gender == "Male", 1.2, -0.2)), position = "stack",
            family = "Source Sans Pro", size = 4, color = "#505050") +
  geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 1.5) +  # Thick line at 0
  coord_flip() +
  scale_fill_manual(values = c("Male" = "#2D8073", "Female" = "#7069B2")) +
  labs(x = "", y = "Count",
       title = "Uganda Population Pyramid - FY 2013",
       subtitle = "Uganda PHIV Estimates | USAID"+
       caption = "Source: spectrum") +
  scale_x_discrete(labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50+")) +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 1),
        axis.text.y = element_text(angle = 0, hjust = 0.5),
        plot.title = element_text(hjust = 0),
        panel.background = element_rect(fill = "white"))



pyramid_plot_22 <- ggplot(fy_2022_long_collapsed, aes(x = Age_band, y = ifelse(Gender == "Male", -count, count), fill = Gender)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = "", hjust = ifelse(Gender == "Male", 1.2, -0.2)), position = "stack",
            family = "Source Sans Pro", size = 4, color = "#505050") +
  geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 1.5) +  # Thick line at 0
  coord_flip() +
  scale_fill_manual(values = c("Male" = "#2D8073", "Female" = "#7069B2")) +
  labs(x = "", y = "Count",
       title = "Uganda Population Pyramid - FY 2022",
       subtitle = "Uganda PHIV Estimates | USAID",
       caption = "Source: spectrum") +
  scale_x_discrete(labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50+")) +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 1),
        axis.text.y = element_text(angle = 0, hjust = 0.5),
        plot.title = element_text(hjust = 0),
        panel.background = element_rect(fill = "white"))




# Save the ggplot as an image file (adjust the filename and path as needed)
ggsave(filename = "Pyramid_2022.png", plot = pyramid_plot_2, width = 8, height = 6, dpi = 300)


# Combine the plots using patchwork
pyramid_plot_2013 + pyramid_plot_2





