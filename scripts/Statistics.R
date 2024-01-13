#install.packages("tidyverse")
library(tidyverse)

# Read the data
statsfile <- "Data/AFREC/all_stats.csv"
StatsRaw <- read.csv(statsfile)

############################### Prepare the data ###############################
SolidStats <- StatsRaw %>%
  #Convert everything to Mt
  mutate(Value = Value / 1000,
         Unit = "Mt") %>%
  filter(Flow %in% StatsCommodityFlowlevels,
         Product %in% StatsSolidBioProductlevels) %>%
  #Use factors for the charts
  mutate(
    Product = factor(Product,
                     levels = StatsSolidBioProductlevels,
                     labels = StatsSolidBioProductlabels),
    Flow = factor(Flow,
                  levels = StatsCommodityFlowlevels,
                  labels = StatsCommodityFlowlabels)
  )

BiogasStats <- StatsRaw %>%
  filter(Product == "Biogas",
         Flow %in% StatsCommodityFlowlevels) %>%
  mutate(Flow = factor(Flow,
                       levels = StatsCommodityFlowlevels,
                       labels = StatsCommodityFlowlabels))

ElecStats <- StatsRaw %>%
  filter(Flow %in% StatsElectricityFlowlevels,
         Product == "Electricity") %>%
  mutate(Flow = factor(Flow,
                       levels = StatsElectricityFlowlevels,
                       labels = StatsElectricityFlowlabels))

############################### Correct the data ##############################

SolidCorrected <- SolidStats %>%
  #Algeria 2015 keep only the second instance
  filter(
    !(
      Country == "Algeria" & Year == "2015" & (
        Value == 5.23100 |
          Value == 6.95000 |
          Value == 0.06686 |
          Value == 14.04100 |
          Value == 0.49000
      )
    )) %>%
    #remove all the weird values for Agro waste in 2017
  filter(
    !(
    Year == "2017"  & Product == "Agro-residues and waste" &
      (
        Country == "Angola" |
          Country == "Burundi" |
          Country == "Cameroon" |
          Country == "Chad" |
          Country == "Democratic Republic of Congo" |
          Country == "Gabon" |
          Country == "Rwanda" |
          Country == "Somalia" |
          Country == "Sudan" |
          Country == "Tanzania" |
          Country == "Zambia"
      ))) %>%
  mutate(
    # Ethiopia biogas figure for 2012 is actually Agro-waste misreported
    Value = case_when((Year == 2012 &
                         Country == "Ethiopia" &
                         Product == "Agro-residues and waste" &
                         (Flow == "Total Final Consumption" |
                            Flow == "Households")
                       ) ~ 17.125,
                      TRUE ~ Value)
  ) %>%
  # some years are split over 2 rows, add them together
  group_by(Country, Year, Flow, Product, Unit) %>%
  summarize(Value = sum(Value)) %>%
  # Remove the breakdown commercial/Non-commercial,
  # due to a weird reclassification in 2016 for all countries
  pivot_wider(names_from = "Product", values_from = "Value") %>%
  replace(is.na(.), 0) %>%
  mutate(Firewood = .data[["Non-commercial firewood"]] + .data[["Commercial firewood"]]) %>%
  select(-c("Non-commercial firewood", "Commercial firewood")) %>%
  pivot_longer(
    cols = -c("Country", "Year", "Flow", "Unit"),
    names_to = "Product",
    values_to = "Value"
  ) %>%
  filter(Value != 0)
  
BiogasCorrected <- BiogasStats %>%
  filter(!(Year == 2012 &
             Country == "Ethiopia" &
             (Flow == "Total Final Consumption" |
                Flow == "Households")))

ElecStatsCorrected <- ElecStats %>%
  #Djibouti 2008-2009 remove weird double inputs
  filter(
    !(
      Country == "Djibouti" & Year %in% c("2008", "2009") & (
        Value == 17000.00 |
          Value == 314910.00 |
          Value == 7228000.00 |
          Value == 343219.00
      )),
    #Guinea 2016 remove wird double inputs
    !(
      Country == "Guinea" & Year == "2016" & (
        Value == 153055.12 |
          Value == 3017.28 |
          Value == 156072.40
    ))) %>%
  mutate(
    Value = case_when(
      #correct again the Morroco all value 10 times too big
      (Country == "Morocco" &
        Year == "2014" &
        Product == "Electricity") ~ Value / 10,
      #correct Mali Hydro (probably 1000 times too big)
      (Country == "Mali" &
         Year == "2015" &
         Product == "Electricity" &
         Flow == "Hydro") ~ Value / 1000,
      TRUE ~ Value)
    ) %>%
  pivot_wider(names_from = Flow, values_from = Value) %>%
  replace(is.na(.), 0) %>%
  mutate(
    Oil = .data[["Fuel oil"]] + .data[["Gas / diesel oil"]],
    WindgeothermalsolarPV = Wind + Solar + Geothermal + Others,
    Total =  .data[["Oil"]] + 
      .data[["Coal"]] +
      .data[["Natural gas"]] +
      .data[["Nuclear"]] +
      .data[["Hydro"]] +
      .data[["Biomass and waste"]] +
      .data[["WindgeothermalsolarPV"]]) %>%
  select(-c("Fuel oil", "Gas / diesel oil", "Wind", "Solar", "Geothermal", "Others")) %>%
  rename("Coal and coal products" = "Coal",
         "Biofuels & waste" = "Biomass and waste",
         "Oil and oil products" = "Oil",
         "Wind & geothermal & solar PV" = "WindgeothermalsolarPV") %>%
  pivot_longer(cols = -c("Country", "Year", "Product", "Unit"), names_to = "Flow", values_to = "Value") %>%
  filter(Value != 0)


##################### Join the biogas data #####################################

StatsCorrected <- SolidCorrected %>%
  bind_rows(BiogasCorrected) %>%
  bind_rows(ElecStatsCorrected)

##################### Add Regional aggregates #################################

StatsExpanded <- StatsCorrected %>%
  # Correct the country names
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
  )  %>%
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
AfricaStats <- StatsExpanded %>%
  group_by(Year, Flow, Product, Unit) %>%
  summarize(Value = sum(Value)) %>%
  ungroup() %>%
  mutate(Country = "Africa",
         Region = "Africa") %>%
  dplyr::select("Region", "Country", "Year", "Flow", "Product", "Unit", "Value")

RegionsStats <- StatsExpanded %>%
  group_by(Region, Year, Flow, Product, Unit) %>%
  summarize(Value = sum(Value)) %>%
  ungroup() %>%
  mutate(Country = Region) %>%
  dplyr::select("Region", "Country", "Year", "Flow", "Product", "Unit", "Value")

StatsExpanded <- StatsExpanded %>%
  bind_rows(RegionsStats) %>%
  bind_rows(AfricaStats)

####################### Charcoal Statistics ####################################

CharcoalStats <- StatsExpanded %>% 
  filter(Product %in% c("Charcoal", "Firewood"),
         Flow %in% c("Production", "Input to charcoal production")) %>%
  pivot_wider(names_from = "Product", values_from = "Value") %>%
  pivot_wider(names_from = "Flow", values_from = c("Charcoal", "Firewood"), names_sep = " ") %>%
  select(-c("Charcoal Input to charcoal production")) %>%
  mutate(ShareFirewoodtoCharcoal = .data[["Firewood Input to charcoal production"]] / 
           .data[["Firewood Production"]],
         MassRatio = .data[["Charcoal Production"]] / .data[["Firewood Input to charcoal production"]]) %>%
  filter(
    ShareFirewoodtoCharcoal < 1,
    MassRatio < 1) %>%
  pivot_longer(cols = -c("Region", "Country", "Year", "Unit"), 
               names_to = "Flow", 
               values_to = "Value")