library(RColorBrewer)

# List of countries per regions

Regions = c(
  "Central Africa",
  "Eastern Africa",
  "Northern Africa",
  "Southern Africa",
  "Western Africa"
)

RegionsColors = c(
  "Africa" = "black",
  "Central Africa" = brewer.pal(5, "Dark2")[1],
  "Eastern Africa" = brewer.pal(5, "Dark2")[2],
  "Northern Africa" = brewer.pal(5, "Dark2")[3],
  "Western Africa" = brewer.pal(5, "Dark2")[4],
  "Southern Africa" = brewer.pal(5, "Dark2")[5]
)

North = c("Algeria",
          "Egypt",
          "Libya",
          "Morocco",
          "Sudan",
          "Tunisia",
          "Western Sahara")
East = c(
  "Burundi",
  "Comoros",
  "Djibouti",
  "Eritrea",
  "Ethiopia",
  "Kenya",
  "Madagascar",
  "Malawi",
  "Mauritius",
  "Rwanda",
  "Seychelles",
  "Somalia",
  "South Sudan",
  "Tanzania",
  "Uganda"
)
Central = c(
  "Cameroon",
  "Central African Republic",
  "Chad",
  "Republic of the Congo",
  "Democratic Republic of the Congo",
  "Equatorial Guinea",
  "Gabon",
  "Sao Tome and Principe"
)
South = c(
  "Angola",
  "Botswana",
  "Eswatini",
  "Lesotho",
  "Mozambique",
  "Namibia",
  "South Africa",
  "Zambia",
  "Zimbabwe"
)
West = c(
  "Benin",
  "Burkina Faso",
  "Cabo Verde",
  "Côte d'Ivoire",
  "The Gambia",
  "Ghana",
  "Guinea",
  "Guinea-Bissau",
  "Liberia",
  "Mali",
  "Mauritania",
  "Niger",
  "Nigeria",
  "Senegal",
  "Sierra Leone",
  "Togo"
)

# Lists of Products

# Used to order items in charts
BalanceProductlevels = c(
  "Total",
  "Oil and Oil Products",
  "Coal and Coal Products",
  "Natural Gas",
  "Nuclear",
  "Electricity",
  "Hydro",
  "Wind & Geothermal & Solar PV",
  "Biofuels & Waste"
)

#Consider relabeling for charts
BalanceProductlabels = c(
  "Total",
  "Oil and oil products",
  "Coal and coal products",
  "Natural gas",
  "Nuclear",
  "Electricity",
  "Hydro",
  "Wind & geothermal & solar PV",
  "Biofuels & waste"
)

#used to color items in charts
BalanceProductColors <- c(
  "Total" = "black",
  "Oil and oil products" = brewer.pal(8, "Dark2")[2],
  "Coal and coal products" = brewer.pal(8, "Dark2")[7],
  "Natural gas" = brewer.pal(8, "Dark2")[8],
  "Nuclear" = brewer.pal(8, "Dark2")[4],
  "Electricity" = brewer.pal(8, "Dark2")[1],
  "Hydro" = brewer.pal(8, "Dark2")[3],
  "Wind & geothermal & solar PV" = brewer.pal(8, "Dark2")[6],
  "Biofuels & waste" = brewer.pal(8, "Dark2")[5]
)

# Used to order items in charts
StatsSolidBioProductlevels <- c(
  "Commercial Firewood",
  "Non Commercial Firewood",
  "Agro Residues and Waste",
  "Charcoal"
)

#Consider relabeling for charts
StatsSolidBioProductlabels <- c(
  "Commercial firewood",
  "Non-commercial firewood",
  "Agro-residues and waste",
  "Charcoal"
)

#Consider relabeling for charts
StatsSolidBioProductColors <- c(
  "Firewood" = brewer.pal(3, "Dark2")[1],
  "Agro-residues and waste" = brewer.pal(3, "Dark2")[2],
  "Charcoal"= brewer.pal(3, "Dark2")[3]
)



# Lists of Flows

BalanceFlowlevels = c(
  "Production",
  "Imports",
  "Exports (-)",
  "International Aviation Bunkers (-)",
  "International Marine Bunkers (-)",
  "Stock Changes  (+ draw, - build)",
  "TPES",
  "Transferts",
  "Statistical Difference",
  "Transformation",
  "Electricity Producers",
  "Petroleum Refineries",
  "Charcoal Plants",
  "Coal-To-Liquids",
  "Gas-To-Liquids",
  "Blast Furnaces",
  "Other Transformation",
  "Energy Industry Own Use",
  "Losses",
  "Total Final Consumption",
  "Industry",
  "Transport",
  "Households",
  "Com. & Public",
  "Agriculture/Forestry",
  "Others (Non Specified)",
  "Non-Energy Use"
)

BalanceFlowlabels = c(
  "Production",
  "Imports",
  "Exports",
  "International Aviation Bunkers",
  "International Marine Bunkers",
  "Stock Changes",
  "TPES",
  "Transferts",
  "Statistical Difference",
  "Transformation",
  "Electricity Producers",
  "Petroleum Refineries",
  "Charcoal Plants",
  "Coal-To-Liquids",
  "Gas-To-Liquids",
  "Blast Furnaces",
  "Other Transformation",
  "Energy Industry Own Use",
  "Losses",
  "Total Final Consumption",
  "Industry",
  "Transport",
  "Households",
  "Com. & Public",
  "Agriculture/Forestry",
  "Others (Non Specified)",
  "Non-Energy Use"
)

EndUseSectors = c(
  "Total Final Consumption",
  "Households", 
  "Com. & Public", 
  "Agriculture/Forestry",
  "Industry", "Transport", 
  "Others (Non Specified)")

#used to color items in charts
EndUseSectorColors <- c(
  "Total Final Consumption" = "black",
  "Industry" = brewer.pal(6, "Dark2")[1],
  "Transport" = brewer.pal(6, "Dark2")[2],
  "Households" = brewer.pal(6, "Dark2")[3],
  "Com. & Public" = brewer.pal(6, "Dark2")[4],
  "Agriculture/Forestry" = brewer.pal(6, "Dark2")[5],
  "Others (Non Specified)" = brewer.pal(6, "Dark2")[6]
)

StatsCommodityFlowlevels <- c(
  "production",
  "import",
  "export",
  "refinery_input",
  "input_to_electricity_production",
  "input_to_charcoal_production",
  "distribution_losses",
  "consumption",
  "consumption_in_industry",
  "consumption_in_transport",
  "consumption_in_households",
  "consumption_in_commerce_and_public_servive",
  "consumption_in_agriculture_forestry",
  "consumption in other non specified",
  "consumption_in_non-energy_use") 

StatsCommodityFlowlabels <- c(
  "Production",
  "Import",
  "Export",
  "Refinery input",
  "Input to electricity production",
  "Input to charcoal production",
  "Distribution losses",
  "Total Final Consumption",
  "Industry",
  "Transport",
  "Households",
  "Com. & Public",
  "Agriculture/Forestry",
  "Others (Non Specified)",
  "Non-energy use") 

StatsElectricityFlowlevels <- c(
  "total generation",
  "fuel oil electricity generation",
  "gas / diesel oil electricity generation",
  "natural gas electricity generation",
  "coal electricity generation",
  "biomass and waste electricity generation",
  "nuclear production",
  "hydroelectric",
  "wind production",
  "solar production",
  "geothermal production",
  "others productions"
)

StatsElectricityFlowlabels <- c(
  "Total",
  "Fuel oil",
  "Gas / diesel oil",
  "Natural gas",
  "Coal",
  "Biomass and waste",
  "Nuclear",
  "Hydro",
  "Wind",
  "Solar",
  "Geothermal",
  "Others"
)