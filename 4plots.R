library(dplyr)
library(ggplot2)
library(cowplot)

#added column for fuel type
full_data <- full_data %>%
  mutate(
    Fuel_Type_Grouped = case_when(
      `Primary Fuel Type` %in% c("Coal", "Coal Refuse", "Coal, Pipeline Natural Gas") ~ "Coal",
      `Primary Fuel Type` %in% c("Pipeline Natural Gas", "Natural Gas", "Natural Gas, Pipeline Natural Gas", "Other Gas", "Process Gas") ~ "Natural Gas",
      `Primary Fuel Type` %in% c("Diesel Oil", "Other Oil", "Diesel Oil, Other Oil", "Diesel Oil, Pipeline Natural Gas", "Residual Oil","Petroleum Coke") ~ "Oil",
      TRUE ~ "Other"
    )
  )

valid_fuel_types <- full_data %>%
  group_by(Fuel_Type_Grouped, `Primary Fuel Type`) %>%
  tally(name = "n") %>%
  filter(n >= 5)

plot_data <- full_data %>%
  semi_join(valid_fuel_types, by = c("Fuel_Type_Grouped", "Primary Fuel Type"))


x_limits <- range(plot_data$`Gross Load (MWh)`, na.rm = TRUE)
y_limits <- range(plot_data$`CO2 Mass (short tons)`, na.rm = TRUE)

groups <- unique(plot_data$Fuel_Type_Grouped)

get_legend <- function(myplot) {
  cowplot::get_legend(myplot)
}

#creating the plots
create_plot_with_legend <- function(g) {
  df <- plot_data %>% filter(Fuel_Type_Grouped == g)
  
  p <- ggplot(df, aes(x = `Gross Load (MWh)`, y = `CO2 Mass (short tons)`,
                      color = `Primary Fuel Type`)) +
    geom_point(alpha = 0.25) +              # scatter alpha
    geom_smooth(aes(group = Fuel_Type_Grouped),
                method = "lm", color = "darkgray", se = FALSE, size = 1) +
    theme_minimal(base_size = 12) +
    theme(
      strip.background = element_rect(fill = "#f0f0f0", color = NA),
      strip.text = element_text(face = "bold"),
      legend.position = "none"
    ) +
    labs(title = g,
         x = "Gross Load (MWh)",
         y = "CO2 Emissions (short tons)") +
    coord_cartesian(xlim = x_limits, ylim = y_limits)
  
  legend <- get_legend(
    ggplot(df, aes(x = `Gross Load (MWh)`, y = `CO2 Mass (short tons)`,
                   color = `Primary Fuel Type`)) +
      geom_point(alpha = 1) +               
      theme_minimal() +
      labs(color = paste0("Fuel Type: ", g)) +
      guides(color = guide_legend(ncol = 1, title.position = "top", title.hjust = 0.5)) +
      theme(
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.6, "cm"),
        legend.spacing.y = unit(0.2, "cm")
      )
  )

  plot_with_legend <- plot_grid(p, legend, ncol = 2, rel_heights = c(1, 0.25))
  return(plot_with_legend)
}

plots_list <- lapply(groups, create_plot_with_legend)

final_plot <- plot_grid(plotlist = plots_list, ncol = 2)
final_plot

