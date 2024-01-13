#install.packages("tidyverse")
library(tidyverse)


ElectricityAccessFile <- "data/World Bank/API_EG.ELC.ACCS.ZS_DS2_en_csv_v2_2766506.csv"
ElectricityAccessRaw <- read.csv(ElectricityAccessFile,
                              skip = 4)


ElectricityAccess <- ElectricityAccessRaw %>%
  select(-c("Indicator.Name", "Indicator.Code", "Country.Code")) %>%
  rename("Country" = "Country.Name") %>%
  filter(Country %in% c(North, East, Central, West, South)) %>%
  #add regions
  mutate(
    Region =  case_when(
      Country %in% North ~ "Northern Africa",
      Country %in% West ~ "Western Africa",
      Country %in% Central ~ "Central Africa",
      Country %in% East ~ "Eastern Africa",
      Country %in% South ~ "Southern Africa"
    )
  ) %>%
  pivot_longer(cols = (-c(Country, Region)),
               names_to = "Year",
               values_to = "ElectricityAccess") %>%
  mutate(across(c('Year'), substr, 2, nchar(Year))) %>%
  mutate(Year = as.integer(Year),
         ElectricityAccess = ElectricityAccess / 100) %>%
  filter(Year >= 2000,
         Year < 2020)