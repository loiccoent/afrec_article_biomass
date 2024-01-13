#install.packages("tidyverse")
library(tidyverse)

PopulationFile <- "data/World Bank/API_SP.POP.TOTL_DS2_en_csv_v2_2918012.csv"
PopulationRaw <- read.csv(PopulationFile, skip = 4)


Population <- PopulationRaw %>%
  dplyr::select(-c("Indicator.Name", "Indicator.Code", "Country.Code")) %>%
  rename("Country" = "Country.Name") %>%
  #Correct the country names
  mutate(Country = case_when(
    Country == "Congo, Rep." ~ "Republic of the Congo",
    Country == "Cote d'Ivoire" ~ "C?te d'Ivoire",
    Country == "Congo, Dem. Rep." ~ "Democratic Republic of the Congo",
    Country == "Egypt, Arab Rep." ~ "Egypt",
    Country == "Gambia, The" ~ "The Gambia",
    TRUE ~ Country)) %>%
  filter(Country %in% c(North, East, Central, West, South)) %>%
  replace(is.na(.), 0) %>%
  pivot_longer(cols = (-Country),
               names_to = "Year",
               values_to = "Total") %>%
  mutate(across(c('Year'), substr, 2, nchar(Year))) %>%
  mutate(Year = as.integer(Year),
         Total = Total / 1000000) %>%
  filter(Year >= 2000,
         Year < 2020)