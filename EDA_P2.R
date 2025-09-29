#EDA of CO2 Emissions (2000–2025) - Section 1 from DropBox

#setwd("C:/Users/masen/OneDrive/Desktop/School/Capstone/EDA/Datasets")

library(tidyverse)
library(lubridate) 
library(scales)

#load

annual_emis <- read_csv("Annual_emissions_byFacility.csv")
monthly_emis <- read_csv("monthlyEmissions_byFacility_2000-2025.csv")
facilities   <- read_csv("Facilities_2000-2025.csv")

#merge emissions & facilities##############################

#merge annual

annual_data <- annual_emis %>%
  left_join(facilities, by = c("Facility ID", "State", "Facility Name", "Year")) %>%
  mutate(Year = as.numeric(Year)) #make sure year is a number

head(annual_data)

#merge monthly

monthly_data <- monthly_emis %>%
  left_join(facilities, by = c("Facility ID", "State", "Facility Name", "Year")) %>%
  mutate(Date = make_date(year = Year, month = Month, day = 1)) #make a date column

head(monthly_data)

#remove 2025

annual_data <- annual_data %>% filter(Year != 2025)
monthly_data <- monthly_data %>% filter(Year != 2025)

#set figure directory\

fd <- "C:/Users/masen/OneDrive/Desktop/School/Capstone/EDA"

#save plots function
save_plot <- function(plot, filename) {
  ggsave(
    filename = file.path(fd, filename),
    plot = plot,
    device = "pdf",
    width = 10, height = 5, units = "in"
  )
}

#define RGGI states and participation changes#########################3

#states w/ changing participation
#   NJ: 2009–2011 (joined), left 2012–2019, rejoined 2020
#   VA: joined 2021, left 2023
#   PA: intended to join ~2022, law passed, compliance quesionable(contested)

state_participation <- tribble(
  ~State, ~Start, ~End,
  "NJ",   2009,   2011,
  "NJ",   2020,   2025,
  "VA",   2021,   2023,
  "PA",   2022,   2025
)

#RGGI states all

rggi_states_all <- c("CT","DE","ME","MD","MA","NH","NY","RI","VT","NJ","VA","PA")

#National v RGGI annual CO2####################################

#aggr national annual totals

national_annual <- annual_data %>%
  group_by(Year) %>%
  summarise(CO2 = sum(`CO2 Mass (short tons)`, na.rm = TRUE)) %>%
  mutate(Level = "National") #add level column, assign national

head(national_annual)

#aggr RGGI annual totals all

rggi_annual <- annual_data %>%
  filter(State %in% rggi_states_all) %>%
  group_by(Year) %>%
  summarise(CO2 = sum(`CO2 Mass (short tons)`, na.rm = TRUE)) %>%
  mutate(Level = "RGGI States") #add level column, assign RGGI

head(rggi_annual)

#plot national annual CO2

p_national_annual <- ggplot(national_annual, aes(x = Year, y = CO2/1e6)) + #/ by million for million short tons
  geom_line(color = "blue4", size = 1) +
  geom_vline(xintercept = 2009, linetype = "dashed", color = "darkgreen") +
  labs(
    title = expression("National Annual " * CO[2] * " Emissions (2000–2024)"),
    y = expression(CO[2] ~ "Emissions (Million Short Tons)",
                   x = "Year")) +
  scale_x_continuous(limits = c(2000, 2024)) +
  theme_minimal()
#save_plot(p_national_annual, "national_annual.pdf")

#plot RGGI annual CO2

p_rggi_annual <- ggplot(rggi_annual, aes(x = Year, y = CO2/1e6)) +
  geom_line(color = "blue4", size = 1) +
  geom_vline(xintercept = 2009, linetype = "dashed", color = "darkgreen") +
  labs(title = expression("RGGI States Annual " * CO[2] * " Emissions (2000–2024)"),
       y = expression(CO[2] ~ "Emissions (Million Short Tons)"), x = "Year") +
  scale_x_continuous(limits = c(2000, 2024)) +
  theme_minimal()
#save_plot(p_rggi_annual, "RGGI_annual.pdf")

#state-level plots#################################

#aggr annual emissions/state

state_annual <- annual_data %>%
  group_by(State, Year) %>%
  summarise(CO2 = sum(`CO2 Mass (short tons)`, na.rm = TRUE), .groups = "drop")

#all state emissions (gray line on plot)

baseline_state <- state_annual %>%
  filter(State %in% c("NJ","VA","PA"))

#overlay years when states were members (colored line)

overlay_state <- baseline_state %>%
  inner_join(state_participation, by = "State") %>%
  filter(Year >= Start & Year <= End) %>%
  mutate(Period = paste0(State, "_", Start, "_", End))

#plot overlay

p_RGGI_overlay <- ggplot() +
  geom_line(data = baseline_state,
            aes(x = Year, y = CO2/1e6, group = State),
            color = "gray", size = 1) +
  geom_line(data = overlay_state,
            aes(x = Year, y = CO2/1e6, color = State, group = Period),
            size = 1.2) +
  facet_wrap(~State, scales = "free_y") + #create subplots with auto adjusting y
  labs(
    title = expression("Annual " * CO[2] * " Emissions: Variable State Participation Trends"),
    y = expression(CO[2] ~ " Emissions (Million Short Tons)"),
    x = "Year",
    color = "Active RGGI by State"
  ) +
  scale_color_manual(
    values = c(
      "NJ" = "royalblue",
      "VA" = "violetred",
      "PA" = "palegreen4"
    )
  ) +
  scale_x_continuous(limits = c(2000, 2024)) +
  theme_minimal() +
  theme(strip.text = element_text(size = 10, face = "bold"),
        panel.spacing = unit(1.5, "lines"))
#save_plot(p_RGGI_overlay, "RGGI_overlay.pdf")
print(p_RGGI_overlay)

#monthly plots (national v RGGI)##############################

#aggr national monthly

national_monthly <- monthly_data %>%
  group_by(Date) %>%
  summarise(CO2 = sum(`CO2 Mass (short tons)`, na.rm = TRUE)) %>%
  mutate(Level = "National")

#aggr RGGI monthly

rggi_monthly <- monthly_data %>%
  filter(State %in% rggi_states_all) %>%
  group_by(Date) %>%
  summarise(CO2 = sum(`CO2 Mass (short tons)`, na.rm = TRUE)) %>%
  mutate(Level = "RGGI States")

#plot national monthly

p_national_monthly <- ggplot(national_monthly, aes(x = Date, y = CO2/1e6)) +
  geom_line(color = "blue4", size = 1) +
  geom_vline(xintercept = as.Date("2009-01-01"),
             linetype = "dashed", color = "darkgreen") +
  labs(title = expression("National Monthly " * CO[2] * " Emissions (2000–2024)"),
       y = expression(CO[2] ~ " Emissions (Million Short Tons)"),
       x = "Year") +
  scale_x_date(limits = c(as.Date("2000-01-01"), as.Date("2024-12-31"))) +
  theme_minimal()
#save_plot(p_national_monthly, "national_monthly.pdf")

#plot RGGI monthly

p_rggi_monthly <- ggplot(rggi_monthly, aes(x = Date, y = CO2/1e6)) +
  geom_line(color = "blue4", size = 1) +
  geom_vline(xintercept = as.Date("2009-01-01"),
             linetype = "dashed", color = "darkgreen") +
  labs(title = expression("RGGI States Monthly " * CO[2] * " Emissions (2000–2024)"),
       y = expression(CO[2] ~ " Emissions (Million Short Tons)"),
       x = "Year") +
  scale_x_date(limits = c(as.Date("2000-01-01"), as.Date("2024-12-31"))) +
  theme_minimal()
#save_plot(p_rggi_monthly, "rggi_monthly.pdf")

# added code 9/19 ####################################

#filter RGGI states excluding NJ, VA, PA

rggi_basic_states <- setdiff(rggi_states_all, c("NJ","VA","PA"))

rggi_state_annual <- annual_data %>%
  filter(State %in% rggi_basic_states) %>%
  group_by(State, Year) %>%
  summarise(CO2 = sum(`CO2 Mass (short tons)`, na.rm = TRUE), .groups = "drop")

#plot RGGI annual side-by-side

p_rggi_state_annual <- ggplot(rggi_state_annual, aes(x = Year, y = CO2/1e6)) +
  geom_line(color = "blue4", size = 1) +
  geom_vline(xintercept = 2009, linetype = "dashed", color = "darkgreen") +
  facet_wrap(~State, scales = "free_y") +
  labs(
    title = expression("Annual " * CO[2] * " Emissions: Consistent RGGI States (2000–2024)"),
    y = expression(CO[2] ~ " Emissions (Million Short Tons)"),
    x = "Year"
  ) +
  scale_x_continuous(limits = c(2000, 2024)) +
  theme_minimal()
#save_plot(p_rggi_state_annual, "rggi_state_annual.pdf")

#neighboring states
# I included NJ, VA, and PA because their participation is short and could be
# considered neighboring when not participating

neighbors <- c("OH","WV","KY", "NJ", "PA", "VA")

#aggr neighbors

neighbors_total <- annual_data %>%
  filter(State %in% neighbors) %>%
  group_by(Year) %>%
  summarise(CO2 = sum(`CO2 Mass (short tons)`, na.rm = TRUE), .groups = "drop") %>%
  mutate(Level = "Neighboring States")

#plot annual neighbors

p_neighboring_states <- ggplot(neighbors_total, aes(x = Year, y = CO2/1e6)) +
  geom_line(color = "violetred", size = 1) +
  geom_vline(xintercept = 2009, linetype = "dashed", color = "darkgreen") +
  labs(
    title = expression("Annual " * CO[2] * " Emissions: Neighboring States"),
    y = expression(CO[2] ~ " Emissions (Million Short Tons)"),
    x = "Year"
  ) +
  scale_x_continuous(limits = c(2000, 2024)) +
  theme_minimal()
#save_plot(p_neighboring_states, "neighboring_states.pdf")

#neighbors subplots

neighbors_annual <- annual_data %>%
  filter(State %in% neighbors) %>%
  group_by(State, Year) %>%
  summarise(CO2 = sum(`CO2 Mass (short tons)`, na.rm = TRUE), .groups = "drop")

p_neighboring_subplots <- ggplot(neighbors_annual, aes(x = Year, y = CO2/1e6)) +
  geom_line(color = "violetred", size = 1) +
  geom_vline(xintercept = 2009, linetype = "dashed", color = "darkgreen") +
  facet_wrap(~State, scales = "free_y") +
  labs(
    title = expression("Annual " * CO[2] * " Emissions: Neighboring States (2000–2024)"),
    y = expression(CO[2] ~ " Emissions (Million Short Tons)"),
    x = "Year"
  ) +
  scale_x_continuous(limits = c(2000, 2024)) +
  theme_minimal()
#save_plot(p_neighboring_subplots, "neighboring_subplots.pdf")

#national v RGGI v neighbors subplots

#combine

compare_levels <- bind_rows(
  national_annual %>% select(Year, CO2, Level),
  rggi_annual %>% select(Year, CO2, Level),
  neighbors_total
)

#plot

p_national_v_rggi_v_neighbors <- ggplot(compare_levels, aes(x = Year, y = CO2/1e6, color = Level)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2009, linetype = "dashed", color = "darkgreen") +
  facet_wrap(~Level, scales = "free_y") +
  scale_color_manual(
    values = c(
      "National" = "darkgrey",
      "RGGI States" = "blue4",
      "Neighboring States" = "violetred"
    )
  ) +
  labs(
    title = expression("Annual " * CO[2] * " Emissions: National vs RGGI vs Neighbors"),
    y = expression(CO[2] ~ " Emissions (Million Short Tons)"),
    x = "Year",
    color = "Region"
  ) +
  scale_x_continuous(limits = c(2000, 2024)) +
  theme_minimal() +
  theme(legend.position = "none")   # hides legend since facet labels already say Level
#save_plot(p_national_v_rggi_v_neighbors, "nat_v_rggi_v_neigh.pdf")

#national v RGGI v neighbors 1 plot

p_nat_v_rggi_neigh_oneplot <- ggplot(compare_levels, aes(x = Year, y = CO2/1e6, color = Level)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2009, linetype = "dashed", color = "darkgreen") +
  scale_color_manual(
    values = c(
      "National" = "darkgrey",
      "RGGI States" = "blue4",
      "Neighboring States" = "violetred"
    )
  ) +
  labs(
    title = expression("Annual " * CO[2] * " Emissions: National vs RGGI vs Neighbors"),
    y = expression(CO[2] ~ " Emissions (Million Short Tons)"),
    x = "Year",
    color = "Region"
  ) +
  scale_x_continuous(limits = c(2000, 2024)) +
  theme_minimal()
#save_plot(p_nat_v_rggi_neigh_oneplot, "oneplot_nat_v_rggi_v_neigh.pdf")


#seasonal analysis

one_year <- monthly_data %>%
  filter(Year == 2018) %>% # arbitrary year
  group_by(Month) %>%
  summarise(CO2 = sum(`CO2 Mass (short tons)`, na.rm = TRUE), .groups = "drop")

#plot monthly trend
p_seasonal_analysis <- ggplot(one_year, aes(x = Month, y = CO2/1e6)) +
  geom_line(color = "darkturquoise", size = 1) +
  geom_point(color = "darkturquoise", size = 1) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(
    title = expression("Monthly " * CO[2] * " Emissions 2018"),
    y = expression(CO[2] ~ " Emissions (Million Short Tons)"),
    x = "Month"
  ) +
  theme_minimal()
#save_plot(p_seasonal_analysis, "seasonal_analysis.pdf")

#season heatmap

seasonality_heatmap <- monthly_data %>%
  group_by(Year, Month) %>%
  summarise(CO2 = sum(`CO2 Mass (short tons)`, na.rm = TRUE), .groups = "drop")

p_seasonality_heatmap <- ggplot(seasonality_heatmap, aes(x = Month, y = Year, fill = CO2/1e6)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(
    title = expression("Seasonal Heatmap of " * CO[2] * " Emissions"),
    x = "Month", y = "Year",
    fill = expression("CO"[2]~"(Million Short Tons)")
  ) +
  theme_minimal()
#save_plot(p_seasonality_heatmap, "seasonality_heatmap.pdf")
print(p_seasonality_heatmap)

#top ten CO2 states

top_states <- annual_data %>%
  group_by(State) %>%
  summarise(Total_CO2 = sum(`CO2 Mass (short tons)`, na.rm = TRUE), .groups = "drop") %>%
  slice_max(Total_CO2, n = 10)

p_top_states <- ggplot(top_states, aes(x = reorder(State, Total_CO2), y = Total_CO2/1e6)) +
  geom_col(fill = "blue4") +
  coord_flip() +
  labs(
    title = expression("Top 10 CO"[2]*" Emitting States (2000–2024)"),
    x = "State", y = expression("Total CO"[2]*" (Million Short Tons)")
  ) +
  theme_minimal()
#save_plot(p_top_states, "top_states.pdf")
print(p_top_states)

#stacked

share_data <- bind_rows(
  rggi_annual %>% select(Year, CO2, Level),
  national_annual %>% mutate(CO2 = CO2 - rggi_annual$CO2) %>% 
    select(Year, CO2, Level)
)

p_share_area <- ggplot(share_data, aes(x = Year, y = CO2/1e6, fill = Level)) +
  geom_area(position = "stack") +
  scale_fill_manual(values = c("RGGI States" = "blue4", "National" = "gray70")) +
  labs(
    title = expression("Share of National CO"[2]*" from RGGI States"),
    y = expression("CO"[2]*" (Million Short Tons)"), x = "Year"
  ) +
  theme_minimal()
#save_plot(p_share_area, "share_area.pdf")
print(p_share_area)

