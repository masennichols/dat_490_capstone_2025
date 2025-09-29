#EDA and Cleaning Facilities

#setwd("C:/Users/masen/OneDrive/Desktop/School/Capstone/EDA/Datasets")

library(tidyverse)
library(janitor)
library(naniar)

#load & chnage names

facilities <- read_csv("Facilities_2000-2025.csv") %>%
  clean_names()

#handle missing data############################################################

#drop rows/cols with >70% missing

facilities_clean <- facilities %>%
  filter(rowMeans(is.na(.)) <= 0.7) %>%
  select(where(~ mean(is.na(.)) <= 0.7))

#before v after cleaning table

summary_table <- tibble(
  Measure = c("Rows", "Columns"),
  Before  = c(nrow(facilities), ncol(facilities)),
  After   = c(nrow(facilities_clean), ncol(facilities_clean))
)

print(summary_table)

#names of removed columns

removed_columns <- setdiff(names(facilities), names(facilities_clean))

removed_columns_table <- tibble(
  Removed_Columns = removed_columns
)

print(removed_columns_table)

#facility characteristics plotting############################################

#RGGI states

rggi_states <- c("CT","DE","ME","MD","MA","NH","NJ","NY","RI","VT","VA")

#count of facilities per state - national

p1 <- ggplot(facilities_clean, aes(x = state)) +
  geom_bar(fill = "blue4") +
  labs(title = "Number of Facilities per State",
       x = "State", y = "Facility Count") +
  theme_minimal()

#count of facilities per state - RGGI

p2 <- facilities_clean %>%
  filter(state %in% rggi_states) %>%
  ggplot(aes(x = state)) +
  geom_bar(fill = "violetred") +
  labs(title = "Number of Facilities per RGGI State",
       x = "State", y = "Facility Count") +
  theme_minimal()

#facilities over time

p3 <- ggplot(facilities_clean, aes(x = commercial_operation_date)) +
  geom_histogram(fill = "forestgreen", bins = 40) +
  labs(title = "Facility Commissioning Over Time",
       x = "Year of Operation", y = "Facility Count") +
  theme_minimal()

#top 10 fuel types

p4 <- facilities_clean %>%
  filter(!is.na(primary_fuel_type) & primary_fuel_type != "") %>%
  count(primary_fuel_type, sort = TRUE) %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(primary_fuel_type, n), y = n)) +
  geom_col(fill = "honeydew3") +
  coord_flip() +
  labs(title = "Top 10 Facility Fuel Types",
       x = "Primary Fuel Type", y = "Facility Count") +
  theme_minimal()

#print
print(p1); print(p2)
print(p3); print(p4); print(p5)

#save##########################################################################

#tables
#write_csv(summary_table, "summary_table.csv")
#write_csv(removed_columns_table, "removed_columns_table.csv")

#plots
#ggsave("facilities_per_state.png", p1, width = 12, height = 6)
#ggsave("facilities_per_rggi_state.png", p2, width = 8, height = 6)
#ggsave("facilities_commissioning_over_time.png", p3, width = 8, height = 6)
#ggsave("top10_fuel_types.png", p4, width = 8, height = 6)
