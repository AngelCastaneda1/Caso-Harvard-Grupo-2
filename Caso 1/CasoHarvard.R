install.packages("dplyr")
library(dplyr)

path_visits <- "C:/Users/user/Desktop/R1/visitas.csv"
path_financials <- "C:/Users/user/Desktop/R1/Financials.csv"

weekly_visits <- read.csv2(path_visits, skip = 4, header = TRUE)
financials <- read.csv2(path_financials, skip = 4, header = TRUE)

merged_data <- inner_join(weekly_visits, financials, by = "Week..2008.2009.")

Base_de_datos_limpia <- merged_data %>%
  select(
    Week = Week..2008.2009.,
    Visits = Visits,
    Unique_Visits = Unique.Visits,
    Revenue = Revenue,
    Profit = Profit,
    Lbs_Sold = Lbs..Sold
  ) %>%
 
  mutate(across(c(Visits, Unique_Visits, Revenue, Profit, Lbs_Sold), 
                ~ as.numeric(gsub("[$,]", "", .)))) %>%
  mutate(Period = case_when(
    row_number() <= 20 ~ "Initial",
    row_number() > 20 & row_number() <= 35 ~ "Pre-Promotion",
    row_number() > 35 & row_number() <= 52 ~ "Promotion",
    TRUE ~ "Post-Promotion"
  )) %>%
  na.omit()


head(Base_de_datos_limpia)


write.csv(Base_de_datos_limpia, "C:/Users/user/Desktop/R1/Base_Datos_Caso.csv", row.names = FALSE)
str(Base_de_datos_limpia)
