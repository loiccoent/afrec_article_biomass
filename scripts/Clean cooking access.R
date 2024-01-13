#install.packages("tidyverse")
library(tidyverse)



CleanCookingAccessFile <- "data/World Bank/API_EG.CFT.ACCS.ZS_DS2_en_csv_v2_2766479.csv"
CleanCookingAccessRaw <- read.csv(CleanCookingAccessFile,
                                 skip = 4)


CleanCookingAccess <- CleanCookingAccessRaw %>%
  select(-c("Indicator.Name", "Indicator.Code", "Country.Code")) %>%
  rename("Country" = "Country.Name") %>%
  mutate(
    Region =  case_when(
      Country %in% North ~ "Northern Africa",
      Country %in% West ~ "Western Africa",
      Country %in% Central ~ "Central Africa",
      Country %in% East ~ "Eastern Africa",
      Country %in% South ~ "Southern Africa"
    )
  ) %>%
  filter(Country %in% c(North, East, Central, West, South)) %>%
  #add regions
  pivot_longer(cols = (-c(Country, Region)),
               names_to = "Year",
               values_to = "CleanCookingAccess") %>%
  mutate(across(c('Year'), substr, 2, nchar(Year))) %>%
  mutate(Year = as.integer(Year),
         CleanCookingAccess = CleanCookingAccess / 100) %>%
  filter(Year >= 2000,
         Year < 2020)