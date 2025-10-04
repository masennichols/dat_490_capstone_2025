library(tidyverse)
library(scales)

#setwd("~/Desktop/EDA/Datasets")

#upload datasets and join
facilities <- read_csv("Facilities_2000-2025.csv")
annual_emis <- read_csv("Annual_emissions_byFacility.csv")

annual_data <- annual_emis %>%
  left_join(facilities, by = c("Facility ID", "State", "Facility Name", "Year")) %>%

#clean the data
df <- annual_data %>%
  filter(!is.na(`Gross Load (MWh)`),
         !is.na(`CO2 Mass (short tons)`),
         `Gross Load (MWh)` > 0,
         `CO2 Mass (short tons)` > 0)

#Set RGGI region statuses
rggi_states_always <- c("CT","DE","ME","MD","MA","NH","NY","RI","VT")

state_participation <- tribble(
  ~State, ~Start, ~End,
  "NJ",   2009,   2011,
  "NJ",   2020,   2025,
  "VA",   2021,   2023,
  "PA",   2022,   2025
)

df <- df %>%
  mutate(
    RGGI_Status = ifelse(State %in% rggi_states_always, 1, 0)
  ) %>%
  rowwise() %>%
  mutate(
    RGGI_Status = if_else(
      any(State == state_participation$State &
            Year >= state_participation$Start &
            Year <= state_participation$End),
      1,
      RGGI_Status
    )
  ) %>%
  ungroup()

# Create plot
ggplot(df, aes(x = `Gross Load (MWh)`, y = `CO2 Mass (short tons)`, color = factor(RGGI_Status))) +
  geom_jitter(alpha = 0.2, size = 1, width = 0.05, height = 0.05) +  
  scale_x_log10(labels = label_comma()) +  
  scale_y_log10(labels = label_comma()) +
  scale_color_manual(values = c("0" = "red", "1" = "blue"),
                     labels = c("Non-RGGI", "RGGI"),
                     name = "RGGI Status") +
  labs(title = "Gross Load vs CO2 Emissions (Log Scale, Jittered)",
       x = "Gross Load (Megawatt Hours)",
       y = "CO2 Emissions (Short Tons)") +
  theme_minimal() 


