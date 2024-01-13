#install.packages("tidyverse")
library(tidyverse)



GDPperCapitaFile <- "data/World Bank/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_2869882.csv"
GDPperCapitaRaw <- read.csv(GDPperCapitaFile,
                                  skip = 4)


GDPperCapita <- GDPperCapitaRaw %>%
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
               values_to = "GDPperCapita") %>%
  mutate(across(c('Year'), substr, 2, nchar(Year))) %>%
  mutate(Year = as.integer(Year)) %>%
  filter(Year >= 2000,
         Year < 2020)


