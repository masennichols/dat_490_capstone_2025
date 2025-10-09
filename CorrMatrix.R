library(readr)
library(corrr)


#Reading data
facilities = read_csv("Facilities_2000-2025.csv")
annual_emmisions = read_csv("Annual_emissions_byFacility.csv")
annual_data = annual_emmisions %>% 
  left_join(facilities, by = c("Facility ID", "State","Facility Name", "Year")) %>%
  mutate(Year = as.numeric(Year))

#Data Cleaning
df_num = annual_data[sapply(annual_data, is.numeric)]
df_num$`Facility ID` = NULL
df_num $`Steam Load (1000 lb)` = NULL
# Compute correlation matrix
corr_matrix = cor(df_num, use = "complete.obs")
#Matrix plot
corr_p = cor.mtest(corr_matrix, conf.level = 0.95)
corrplot(corr_matrix, method = "circle", addCoef.col = "black", number.cex = 0.6,tl.col =
           "black",p.mat = corr_p$p, sig.level = 0.05, insig = "blank", type = "upper")

