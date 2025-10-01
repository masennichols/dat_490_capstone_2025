library(dplyr)
library(ggplot2)

df_ci <- full_data %>%
  filter(!is.na(`CO2 Mass (short tons)`), `CO2 Mass (short tons)` > 0)

yearly_summary <- df_ci %>%
  group_by(Year, Group_Status) %>%
  summarise(
    mean_CO2 = mean(`CO2 Mass (short tons)`),
    sd_CO2 = sd(`CO2 Mass (short tons)`),
    n = n(),
    se = sd_CO2 / sqrt(n),
    ci_lower = mean_CO2 - qt(0.975, n-1) * se,
    ci_upper = mean_CO2 + qt(0.975, n-1) * se
  ) %>%
  ungroup()

ggplot(yearly_summary, aes(x = Year, y = mean_CO2, color = Group_Status, group = Group_Status)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = Group_Status), alpha = 0.2, color = NA) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("RGGI" = "blue",
                                "Neighbor Non-RGGI" = "darkgreen",
                                "Other Non-RGGI" = "red")) +
  scale_fill_manual(values = c("RGGI" = "blue",
                               "Neighbor Non-RGGI" = "darkgreen",
                               "Other Non-RGGI" = "red")) +
  labs(title = "Mean CO2 Emissions by Group Over Time (with 95% CI)",
       x = "Year",
       y = "Mean CO2 Emissions (Short Tons)",
       color = "Group Status",
       fill = "Group Status") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


