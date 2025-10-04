
library(tidyverse)
library(lubridate)

setwd("~/Desktop/EDA/Datasets")

# Load and join datasets
facilities <- read_csv("Facilities_2000-2025.csv")
annual_emis <- read_csv("Annual_emissions_byFacility.csv")

annual_emis <- annual_emis %>%
  mutate(Year = as.numeric(Year))

full_data <- annual_emis %>%
  left_join(facilities, by = c("Facility ID", "State", "Facility Name", "Year"))

# filter na's
full_data <- full_data %>%
  filter(!is.na(`Gross Load (MWh)`),
         !is.na(`CO2 Mass (short tons)`),
         !is.na(Latitude) & !is.na(Longitude))

# Setup flag variables for regions
rggi_states_always <- c("CT","DE","ME","MD","MA","NH","NY","RI","VT")

state_participation <- tribble(
  ~State, ~Start, ~End,
  "NJ",   2009,   2011,
  "NJ",   2020,   2025,
  "VA",   2021,   2023,
  "PA",   2022,   2025
)

full_data <- full_data %>%
  mutate(
    RGGI_Status = ifelse(State %in% rggi_states_always, 1, 0)
  ) %>%
  mutate(
    RGGI_Status = case_when(
      (State == "NJ" & Year >= 2009 & Year <= 2011) ~ 1,
      (State == "NJ" & Year >= 2020 & Year <= 2025) ~ 1,
      (State == "VA" & Year >= 2021 & Year <= 2023) ~ 1,
      (State == "PA" & Year >= 2022 & Year <= 2025) ~ 1,
      TRUE ~ RGGI_Status
    ),
    RGGI_Label = ifelse(RGGI_Status == 1, "RGGI", "Non-RGGI")
  )

#Neighboring states (excluding variable states during their RGGI periods)
fixed_neighbor_states <- c("WV", "OH")
variable_neighbor_states <- c("NJ", "VA", "PA")

#variable states are neighbors when...
full_data <- full_data %>%
  mutate(
    Neighbor_Status = case_when(
      State %in% fixed_neighbor_states ~ 1,
      (State == "NJ" & !(Year >= 2009 & Year <= 2011) & !(Year >= 2020 & Year <= 2025)) ~ 1,
      (State == "VA" & !(Year >= 2021 & Year <= 2023)) ~ 1,
      (State == "PA" & !(Year >= 2022 & Year <= 2025)) ~ 1,
      TRUE ~ 0
    )
  )

# add status field for mapping
full_data <- full_data %>%
  mutate(
    Group_Status = case_when(
      RGGI_Status == 1 ~ "RGGI",
      Neighbor_Status == 1 ~ "Neighbor Non-RGGI",
      TRUE ~ "Other Non-RGGI"
    )
  )

table(full_data$Neighbor_Status, full_data$State)

write_csv(full_data, "all_FacilitiesandEmissions.csv")
write_csv(full_data %>% filter(RGGI_Status == 1), "RGGI_FacilitiesandEmissions.csv")
write_csv(full_data %>% filter(Neighbor_Status == 1), "Neighboring_FacilitiesandEmissions.csv")
write_csv(full_data %>% filter(RGGI_Status == 0 & Neighbor_Status == 0), "NonRGGI_NonNeighbor_FacilitiesandEmissions.csv")


