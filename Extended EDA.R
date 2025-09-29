#Extended EDA

#setwd("C:/Users/masen/OneDrive/Desktop/School/Capstone/EDA/Datasets")

library(tidyverse)
library(janitor)
library(lubridate)
library(RColorBrewer)
library(viridis)

#load & clean

annual_emis   <- read_csv("Annual_emissions_byFacility.csv")
monthly_emis  <- read_csv("monthlyEmissions_byFacility_2000-2025.csv")
facilities    <- read_csv("Facilities_2000-2025.csv") %>% clean_names()

annual_emis   <- annual_emis %>% mutate(Year = as.numeric(Year))
facilities    <- facilities %>% mutate(year = as.numeric(year))

facilities_unique <- facilities %>%
  distinct(facility_id, year, primary_fuel_type, .keep_all = TRUE)

annual_data <- annual_emis %>%
  left_join(facilities_unique,
            by = c("Facility ID" = "facility_id",
                   "Year" = "year",
                   "State" = "state",
                   "Facility Name" = "facility_name"))

annual_data <- annual_data %>% filter(Year <= 2024)

rggi_states <- c("CT","DE","ME","MD","MA","NH","NY","RI","VT","NJ","VA","PA")
annual_data <- annual_data %>%
  mutate(Group = ifelse(State %in% rggi_states, "RGGI", "Non-RGGI"))

annual_data <- annual_data %>%
  filter(`Gross Load (MWh)` > 0,
         `CO2 Mass (short tons)` > 0,
         !is.na(primary_fuel_type))

#facility distributions###################################

facility_co2 <- annual_data %>%
  group_by(`Facility ID`, `Facility Name`, Group) %>%
  summarise(Total_CO2 = sum(`CO2 Mass (short tons)`, na.rm = TRUE), .groups = "drop")

p_facility_co2 <- ggplot(facility_co2, aes(x = log10(Total_CO2), fill = Group)) +
  geom_histogram(alpha = 0.7, position = "identity", bins = 30) +
  labs(title = expression("Distribution of Facility-Level CO" [2] ~ " Emissions (Log Scale)"),
       x = expression("Log"[10] * " of Total CO"[2] * " (short tons)"),
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")
print(p_facility_co2)

#state-level CO2 emissions - RGGI v neighbor######################

rggi_states <- c("CT","DE","ME","MD","MA","NH","NY","RI","VT","NJ","VA","PA")
neighbors   <- c("OH","WV","KY","NJ","PA","VA")

state_subset <- annual_data %>%
  filter(State %in% c(rggi_states, neighbors))

#boxplot - annual CO2 by state

p_boxplot_state <- ggplot(state_subset, aes(x = State,
                                            y = `CO2 Mass (short tons)` / 1e6,
                                            fill = ifelse(State %in% rggi_states, "RGGI", "Neighbor"))) +
  geom_boxplot(outlier.alpha = 0.3) +
  labs(title = expression("State-Level CO"[2] * " Emissions (2000–2024)"),
       y = expression("Annual CO"[2] * " (Million Short Tons)"),
       x = "State",
       fill = "Group") +
  theme_minimal() +
  coord_flip() +
  scale_fill_brewer(palette = "Set1")

print(p_boxplot_state)

#fuel efficiency ratios (CO2 per MWh)

#calculate CO2 per MWh

annual_data <- annual_data %>%
  mutate(CO2_per_MWh = `CO2 Mass (short tons)` / `Gross Load (MWh)`)

#top 10 fuels

top_fuels <- annual_data %>%
  count(primary_fuel_type, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(primary_fuel_type)

fuel_data <- annual_data %>%
  filter(primary_fuel_type %in% top_fuels,
         is.finite(CO2_per_MWh))

#remove >99th %

upper_cutoff <- quantile(fuel_data$CO2_per_MWh, 0.99, na.rm = TRUE)
fuel_data <- fuel_data %>%
  filter(CO2_per_MWh <= upper_cutoff)

#plot

p_fuel_eff <- ggplot(fuel_data, aes(x = reorder(primary_fuel_type, CO2_per_MWh, median),
                                    y = CO2_per_MWh, fill = primary_fuel_type)) +
  geom_boxplot(outlier.alpha = 0.2) +
  coord_flip() +
  scale_y_log10() +
  labs(title = expression("CO"[2] * " Intensity by Primary Fuel Type"),
       y = expression("CO"[2] * " per MWh (log scale)"),
       x = "Fuel Type") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Set3")

print(p_fuel_eff)

p_boxplot_nonrggi <- annual_data %>%
  filter(Group == "Non-RGGI") %>%
  ggplot(aes(x = reorder(State, CO2_per_MWh, median), 
             y = CO2_per_MWh, 
             fill = State)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_y_log10() +
  scale_fill_viridis_d(option = "turbo", begin = 0.2, end = 0.9) +
  labs(title = expression("Facility-Level CO" [2] * " Intensity by State"),
       x = "State", y = expression("CO" [2] * " per MWh")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

print(p_boxplot_nonrggi)

#save#################################

#ggsave("facility_distribution.pdf", p_facility_co2, width = 10, height = 6)
#ggsave("state_boxplot.pdf", p_boxplot_state, width = 10, height = 6)
#ggsave("fuel_efficiency.pdf", p_fuel_eff, width = 10, height = 6)
#ggsave("boxplot_nonrggi.pdf", p_boxplot_nonrggi, width = 10, height = 6)

