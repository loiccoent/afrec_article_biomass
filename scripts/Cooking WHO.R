#install.packages("tidyverse")
library(tidyverse)


PopulationDirtyFuelFile <- "data/WHO/Population with primary reliance on polluting fuels and technologies for cooking (in millions).csv"
PopulationDirtyFuelRaw <- read.csv(PopulationDirtyFuelFile)


PopulationDirtyFuel <- PopulationDirtyFuelRaw %>%
  filter(Dim1 == "Total") %>%
  dplyr::select(c("Location", "Period", "FactValueNumeric")) %>%
  rename("Country" = "Location",
         "Year" = "Period",
         "Without access to clean cooking" = "FactValueNumeric") %>%
  #Correct the country names
  mutate(Country = case_when(
    Country == "Congo" ~ "Republic of the Congo",
    Country == "Tanzania" ~ "United Republic of Tanzania",
    Country == "Gambia, The" ~ "The Gambia",
    TRUE ~ Country)) %>%
  filter(Country %in% c(North, East, Central, West, South))
  

PopulationCooking <- PopulationDirtyFuel %>%
  inner_join(Population, by = c("Country", "Year")) %>%
  pivot_longer(cols = -c("Country", "Year"), 
               names_to = "Indicator", 
               values_to = "Value") %>%
  mutate(Unit = "Millions")
  
##################### Add Regional aggregates #################################

# Add regions
PopulationCookingExpanded <- PopulationCooking %>%
  mutate(
    Region =  case_when(
      Country %in% North ~ "Northern Africa",
      Country %in% West ~ "Western Africa",
      Country %in% Central ~ "Central Africa",
      Country %in% East ~ "Eastern Africa",
      Country %in% South ~ "Southern Africa"
    )
  )

# Add a total Africa
AfricaPopulationCooking <- PopulationCookingExpanded %>%
  group_by(Year, Indicator, Unit) %>%
  summarize(Value = sum(Value)) %>%
  ungroup() %>%
  mutate(Country = "Africa",
         Region = "Africa") %>%
  dplyr::select("Region", "Country", "Year", "Indicator", "Unit", "Value")

RegionsPopulationCooking <- PopulationCookingExpanded %>%
  group_by(Region, Year, Indicator, Unit) %>%
  summarize(Value = sum(Value)) %>%
  ungroup() %>%
  mutate(Country = Region) %>%
  dplyr::select("Region", "Country", "Year", "Indicator", "Unit", "Value")

PopulationCookingExpanded <- PopulationCookingExpanded %>%
  bind_rows(RegionsPopulationCooking) %>%
  bind_rows(AfricaPopulationCooking) %>%
  rename("Population" = "Value")