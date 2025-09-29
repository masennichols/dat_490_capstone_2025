#Inferential Statistics

#setwd("C:/Users/masen/OneDrive/Desktop/School/Capstone/EDA/Datasets")

library(tidyverse)
library(janitor)
library(broom)

#load & clean

annual_emis <- read_csv("Annual_emissions_byFacility.csv")
facilities  <- read_csv("Facilities_2000-2025.csv") %>% clean_names()

#year column is numeric

annual_emis <- annual_emis %>% mutate(Year = as.numeric(Year))
facilities  <- facilities %>% mutate(year = as.numeric(year))

#merge datasets

annual_data <- annual_emis %>%
  left_join(facilities,
            by = c("Facility ID" = "facility_id",
                   "State"       = "state",
                   "Facility Name" = "facility_name",
                   "Year"        = "year"))

#drop future year 2025

annual_data <- annual_data %>% filter(Year <= 2024)

#RGGI states

rggi_states <- c("CT","DE","ME","MD","MA","NH","NY","RI","VT","NJ","VA","PA")

#group variable

annual_data <- annual_data %>%
  mutate(Group = ifelse(State %in% rggi_states, "RGGI", "Non-RGGI"))

#chi-square - fuel type by RGGI v non-RGGI################################

fuel_table <- annual_data %>%
  tabyl(Group, primary_fuel_type)

chisq_input <- as.matrix(fuel_table[,-1])
rownames(chisq_input) <- fuel_table$Group

#use simulated p-value

chisq_result <- chisq.test(chisq_input, simulate.p.value = TRUE, B = 10000)

cat("\nChi-Square Test: Fuel Type by RGGI vs Non-RGGI\n")
print(chisq_result)

chisq_out <- broom::tidy(chisq_result)
write_csv(chisq_out, "chisq_test_results.csv")

#save table of proportions

fuel_prop <- fuel_table %>%
  pivot_longer(-Group, names_to = "Fuel_Type", values_to = "Count") %>%
  group_by(Group) %>%
  mutate(Proportion = Count / sum(Count))

write_csv(fuel_prop, "fuel_type_distribution.csv")

#trend regression - annual CO2 v year###############################

trend_data <- annual_data %>%
  group_by(Group, Year) %>%
  summarise(CO2 = sum(`CO2 Mass (short tons)`, na.rm = TRUE), .groups = "drop")

#fit regression models

trend_models <- trend_data %>%
  group_by(Group) %>%
  nest() %>%
  mutate(
    model   = map(data, ~ lm(CO2 ~ Year, data = .)),
    summary = map(model, glance)
  )

cat("\nTrend Regression Results: Annual CO2 vs Year\n")
trend_results <- trend_models %>%
  select(Group, summary) %>%
  unnest(summary) %>%
  select(Group, r.squared, adj.r.squared, p.value, AIC)

print(trend_results)

#save results

#write_csv(trend_results, "trend_regression_results.csv")

#plot trend regression##########################################

p_trend <- ggplot(trend_data, aes(x = Year, y = CO2/1e6, color = Group)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
  labs(title = "Trend Regression of Annual CO2 Emissions",
       x = "Year", y = "CO2 Emissions (Million Short Tons)") +
  theme_minimal()

#ggsave("trend_regression_plot.pdf", p_trend, width = 8, height = 6)
print(p_trend)
