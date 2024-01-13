#install.packages("tidyverse")
library(tidyverse)

################################ Read the data #################################

balancefile <- "data/AFREC/all_balances.csv"
BalanceRaw <- read.csv(balancefile)

############################### Prepare the data ###############################
Balance <- BalanceRaw %>%
  #Convert everything to Mtoe
  mutate(Value = Value / 1000,
         Unit = "Mtoe") %>%
  #Merge crude and oil products into one
  pivot_wider(names_from = "Product", values_from = "Value") %>%
  replace(is.na(.), 0) %>%
  mutate(Oil =
           .data[["Crude Oil"]] +
           .data[["Oil Products"]]) %>%
  rename("Oil and Oil Products" = "Oil") %>%
  dplyr::select(-c("Crude Oil", "Oil Products")) %>%
  pivot_longer(
    cols = -c("Country", "Year", "Flow", "Unit"),
    names_to = "Product",
    values_to = "Value"
  ) %>%
  filter(!is.na(Value)) %>%
  #Use factors for the charts
  mutate(Product = factor(Product,
                          levels = BalanceProductlevels,
                          labels = BalanceProductlabels),
         Flow = factor(Flow,
                       levels = BalanceFlowlevels,
                       labels = BalanceFlowlabels)) %>%
  #Correct the country names
  mutate(
    Country = case_when(
      Country == "Bostwana" ~ "Botswana",
      Country == "Cape Verde" ~ "Cabo Verde",
      Country == "Central Africa Republic" ~ "Central African Republic",
      Country == "Congo" ~ "Republic of the Congo",
      Country == "Cote d Ivoire" ~ "C?te d'Ivoire",
      Country == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
      Country == "Gambia" ~ "The Gambia",
      Country == "Guinea Bissau" ~ "Guinea-Bissau",
      Country == "South sudan" ~ "South Sudan",
      Country == "Sahrawi Republic" ~ "Western Sahara",
      TRUE ~ Country
    )
  )

############################### Correct the data ##############################

#### Madagascar 2013, coal seems to be reported in toe instead of ktoe #######

# Correct the coal
MadagascarCoal2013 <- Balance %>%
  filter(Country == "Madagascar",
         Year == 2013,
         Product == "Coal and coal products") %>%
  mutate(Value = Value / 1000)

# add the Coal in the 2013 data
Madagascar2013 <- Balance %>%
  filter(Country == "Madagascar",
         Year == 2013,
         Product != "Coal and coal products") %>%
  bind_rows(MadagascarCoal2013)

#Recalculate the Total as the sum of all the other products
Madagascar2013 <- Madagascar2013 %>%
  pivot_wider(names_from = "Product", 
              values_from = "Value") %>%
  replace(is.na(.), 0) %>%
  mutate(Total =
           .data[["Hydro"]] +
           .data[["Biofuels & waste"]] +
           #No Nuclear
           #.data[["Nuclear"]] +
           #No Natural Gas
           #.data[["Natural gas"]] +
           .data[["Coal and coal products"]] +
           .data[["Oil and oil products"]] +
           .data[["Electricity"]]) %>%
  pivot_longer(cols = -c("Country", "Year", "Flow", "Unit"), 
               names_to = "Product", 
               values_to = "Value")

# Replace in the initial data frame
BalanceCorrected <- Balance %>%
  filter(!((Country == "Madagascar") &
             (Year == 2013))) %>%
  bind_rows(Madagascar2013)

### Morocco 2014, electricity is either 10x or 1000 x too high for some flows ###

# Correct the electricity
MoroccoEle2014 <- BalanceCorrected %>%
  filter(Country == "Morocco",
         Year == 2014,
         Product == "Electricity") %>%
  # correct as needed
  mutate(Value = case_when(
    Flow %in% c("Imports", "TPES", "Transformation", "Electricity Producers") ~ Value / 10,
    Flow %in% c("Industry", "Agriculture/Forestry", "Losses") ~ Value / 1000,
    TRUE ~ Value
  )) %>%
  # recalculate the aggregate flows in order
  pivot_wider(names_from = Flow,
              values_from = Value) %>%
  replace(is.na(.), 0)

# Cannot be done easily in dplyr because of the space in names
MoroccoEle2014["Total Final Consumption"] =
  MoroccoEle2014["Industry"] +
  MoroccoEle2014["Transport"] +
  MoroccoEle2014["Households"] +
  MoroccoEle2014["Com. & Public"] +
  MoroccoEle2014["Agriculture/Forestry"] 
  #No Others
  #MoroccoEle2014["Others (Non Specified)"]

MoroccoEle2014["TPES"] =
  MoroccoEle2014["Imports"] +
  MoroccoEle2014["Exports"]

MoroccoEle2014["Statistical Difference"] =
  MoroccoEle2014["Total Final Consumption"] - 
  MoroccoEle2014["Transformation"] -
  MoroccoEle2014["Energy Industry Own Use"] -
  MoroccoEle2014["Losses"] +
  MoroccoEle2014["TPES"]

MoroccoEle2014 <- MoroccoEle2014 %>%
  pivot_longer(cols = -c("Country", "Year", "Product", "Unit"), 
               names_to = "Flow", 
               values_to = "Value") %>%
  dplyr::select(c("Country", "Year", "Flow", "Unit", "Product", "Value"))

# add the Electricity in the 2014 data
Morocco2014 <- BalanceCorrected %>%
  filter(Country == "Morocco",
         Year == 2014,
         Product != "Electricity") %>%
  bind_rows(MoroccoEle2014)

#Recalculate the Total as the sum of all the other products
Morocco2014 <- Morocco2014 %>%
  pivot_wider(names_from = "Product", 
              values_from = "Value") %>%
  replace(is.na(.), 0) %>%
  mutate(Total =
           .data[["Hydro"]] +
           .data[["Biofuels & waste"]] +
           .data[["Nuclear"]] +
           .data[["Natural gas"]] +
           .data[["Coal and coal products"]] +
           .data[["Oil and oil products"]] +
           .data[["Electricity"]]) %>%
  pivot_longer(cols = -c("Country", "Year", "Flow", "Unit"), 
               names_to = "Product", 
               values_to = "Value")

# Replace in the data frame corrected for Madagascar
BalanceCorrected <- BalanceCorrected %>%
  filter(!((Country == "Morocco") &
             (Year == 2014))) %>%
  bind_rows(Morocco2014)

##################### Add Regional aggregates #################################

BalanceExpanded <- BalanceCorrected %>%
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
  dplyr::select("Region", "Country", "Year", "Flow", "Product", "Unit", "Value")

# Add a total Africa
AfricaBalance <- BalanceExpanded %>%
  group_by(Year, Flow, Product, Unit) %>%
  summarize(Value = sum(Value)) %>%
  ungroup() %>%
  mutate(Country = "Africa",
         Region = "Africa") %>%
  dplyr::select("Region", "Country", "Year", "Flow", "Product", "Unit", "Value")

RegionsBalance <- BalanceExpanded %>%
  group_by(Region, Year, Flow, Product, Unit) %>%
  summarize(Value = sum(Value)) %>%
  ungroup() %>%
  mutate(Country = Region) %>%
  dplyr::select("Region", "Country", "Year", "Flow", "Product", "Unit", "Value")

BalanceExpanded <- BalanceExpanded %>%
  bind_rows(RegionsBalance) %>%
  bind_rows(AfricaBalance)

################# Share of biomass in each end use sector ######################

ShareBiomassInSectors <- BalanceExpanded %>%
  pivot_wider(names_from = Product,
              values_from = Value) %>%
  mutate(ShareBiomass = .data[["Biofuels & waste"]] / .data[["Total"]]) %>%
  dplyr::select(c("Country", "Region", "Flow", "Year", "ShareBiomass"))

################ Share of biomass vs Access to clean cooking ###################

SharevsCook <- ShareBiomassInSectors %>% 
  inner_join(CleanCookingAccess, by = c("Region", "Country", "Year"))

################ Share of biomass vs Access to Electricity ###################

SharevsElec <- ShareBiomassInSectors %>% 
  inner_join(ElectricityAccess, by = c("Region", "Country", "Year"))

######### Share of Biomass in End use energy consumption vs GDP ################

SharevsGDP <- ShareBiomassInSectors %>% 
  inner_join(GDPperCapita, by = c("Region", "Country", "Year"))

# Biofuel and waste consumption vs population without acccess to clean cooking #

ConsovsPop <- BalanceExpanded %>% 
  filter(Product == "Biofuels & waste",
         Flow == "Households") %>%
  rename("Consumption" = "Value") %>%
  inner_join(PopulationCookingExpanded, by = c("Region", "Country", "Year"))

