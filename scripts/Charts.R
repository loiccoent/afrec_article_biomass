#install.packages(c("spData", "tmap", "cartogram"))
library(sf)
#library(raster)
library(spData)
#library(tmap)
#library(leaflet)
#library(cartogram)

########################## Total Energy Supply in Africa #######################

BalanceExpanded %>%
  filter(Country == "Africa",
         Flow == "TPES",
         Product != "Total") %>%
  mutate(Product = factor(Product,
                          levels = BalanceProductlabels[9:1])) %>%
  ggplot(aes(x = Year, y = Value, fill = Product)) +
  geom_col() +
  theme_classic() +
  ggtitle("Total energy supply in Africa") +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.title = element_text(hjust = 0.5)
  ) +
  ylab("Total Energy Supply (Mtoe)") +
  scale_fill_manual(values = BalanceProductColors[-1])

############### Share of biomass in TPES, by region #############################

ShareBiomassInSectors %>%
  filter(Country %in% c(Regions, "Africa"),
         Flow == "TPES") %>%
  ggplot(aes(x = Year, y = ShareBiomass, colour = Region)) +
  geom_line(size=1) +
  theme_classic() +
  facet_wrap(~Region) +
  ggtitle("Share of biofuel and waste in Total Energy Supply") +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.title = element_text(hjust = 0.5)
  ) +
  xlim(2000, 2017) +
  scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
  ylab("Share") +
  scale_colour_manual(values = RegionsColors)

############# Population vs Population without access to clean cooking ########

PopulationCookingExpanded %>%
  filter(Country %in% c(Regions, "Africa")) %>%
  ggplot(aes(x = Year, y = Population, color = Indicator)) +
  geom_line(size = 1) +
  facet_wrap(~Region) +
  theme_classic() +
  ggtitle("Population growth and access to clean cooking fuels and technologies") +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.title = element_text(hjust = 0.5)
  )  +
  ylab("Population (Millions)")


###### Consumption of biofuel and waste vs Population relying on dirty fuel ####

#used to match the 2 scales
coeff = 0.5

ConsovsPop %>% 
  filter(Country == "Africa") %>%
  ggplot(aes(x = Year)) +
  geom_col(aes(y = Consumption), fill = "darkgreen") +
  geom_point(aes(y = Population / coeff, color = Indicator), size = 3) +
  scale_y_continuous(
    # Features of the first axis
    name = "Consumption in residential (Mtoe)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Population (Millions)")
  ) + 
  theme_classic() +
  ggtitle("Biofuel and waste consumption in households and total population without access to clean cooking in Africa") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.title.y = element_text(color = "darkgreen"),
    axis.title.x = element_blank(),
    axis.title.y.right = element_text(color = "black"))

########## Share of Sectors in Biomass end use consumption #####################

BalanceExpanded %>%
  #exclude the total final consumption
  filter(Flow %in% EndUseSectors[-1],
         Product == "Biofuels & waste",
         Country == "Africa") %>%
  ggplot(aes(x = Year, y = Value, fill = Flow)) +
  geom_col() +
  theme_classic() +
  ggtitle("Biofuel and waste consumption in end use sectors") +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.title = element_text(hjust = 0.5)
  )  +
  ylab("Consumption (Mtoe)") +
  scale_fill_manual(values = EndUseSectorColors[-1])

############### Share of biomass in TFC, by region #############################

ShareBiomassInSectors %>%
  filter(Country %in% c(Regions, "Africa"),
         Flow == "Total Final Consumption") %>%
  ggplot(aes(x = Year, y = ShareBiomass, colour = Region)) +
  geom_line(size=1) +
  theme_classic() +
  ggtitle("Share of biofuel and waste in Total Final Energy Consumption") +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.title = element_text(hjust = 0.5)
  ) +
  xlim(2000, 2017) +
  scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
  ylab("Share") +
  scale_colour_manual(values = RegionsColors)

###### Heat map of share of biomass in residential Sector in Africa ############

# Geographical element
AfricaShareBiomassInSectors = world %>%
  filter(continent == "Africa") %>%
  rename("Country" = "name_long") %>%
  #Correct the typo in Eswatini name
  mutate(Country = case_when(Country == "eSwatini" ~ "Eswatini",
                             TRUE ~ Country)) %>%
  st_transform("+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25") %>%
  left_join(ShareBiomassInSectors, by = c("Country"))

# Not used but not bad
#plot(africa["ShareBiomass"])
#tm_shape(africa) +
#  tm_polygons("ShareBiomass")

AfricaShareBiomassInSectors %>%
  filter(Flow == "Total Final Consumption",
         Year == 2017) %>%
  ggplot() +
  geom_sf(aes(geometry = geom, fill = ShareBiomass)) +
  ggtitle("Share of biofuel and waste in Total Final Energy consumption, 2017") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    line = element_blank(),
    legend.position = c(0.25, 0.30)
  ) +
  labs(fill = "Share") +
  scale_fill_gradient(
    # it was a pain to show the 100% correctly
    limits = c(1, 0),
    breaks = scales::pretty_breaks(n = 4)(0:1),
    labels = scales::percent,
    trans = 'reverse'
  )

############### Country ranked by share of Biomass in TFC  #####################

ShareBiomassInSectors %>%
  filter(Year %in% c("2000","2017"),
         !Country %in% Regions,
         Flow == "Total Final Consumption") %>%
  pivot_wider(names_from = "Year", values_from = "ShareBiomass") %>%
  arrange(desc(.data[["2017"]])) %>%
  top_n(20) %>%
  pivot_longer(col=-c("Country", "Region", "Flow"), names_to = "Year", values_to = "ShareBiomass") %>%
  ggplot(aes(x = reorder(Country, ShareBiomass), y = ShareBiomass)) +
  geom_point(aes(color = factor(Year)), size = 3) +
  geom_line(aes(group = Country), colour = "grey") +
  coord_flip() +
  theme_classic() +
  ggtitle("Countries with the highest shares of biofuels and waste in TFC, 2000 to 2017") +
  theme(
    axis.title.y = element_blank(),
    legend.position = c(0.5, 0.8),
    legend.box = "horizontal",
    plot.title = element_text(hjust = 0.5)
  )  +
  scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
  ylab("Share of biofuels in TFC") +
  labs(color = "Year")

################## Share of Biomass in TFC vs Access to Clean cooking ##########

SharevsCook %>%
  filter(Year==2017,
         !Country %in% Regions,
         Flow == "Total Final Consumption") %>%
  ggplot(aes(x = CleanCookingAccess, y = ShareBiomass)) +
  geom_point(aes(colour = Region)) +
  ggtitle("Electricity access vs share of biomass in Residential consumption") +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        plot.title = element_text(hjust = 0.5)
  ) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  ylab("Share of biomass in residential") +
  xlab("Access to clean cooking") +
  geom_text(aes(label= Country, colour = Region), 
            hjust=0, 
            vjust=0)

################## Share of Biomass in TFC vs Access to Electricity ##########

SharevsElec %>%
  filter(Year==2017,
         !Country %in% Regions,
         Flow == "Total Final Consumption") %>%
  ggplot(aes(x = ElectricityAccess, y = ShareBiomass, colour = Region)) +
  geom_point() +
  ggtitle("Electricity access vs share of biomass in Residential consumption") +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        plot.title = element_text(hjust = 0.5)
  ) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  ylab("Share of biomass in residential") +
  xlab("Electricity access") +
  geom_text(aes(label= Country), 
            hjust=0, 
            vjust=0)

######################### Share of Biomass in TFC vs GDP by year ###############

SharevsGDP %>%
  filter(Year %in% c("2000", "2017"),
         !Country %in% Regions,
         Flow == "Households") %>%
  ggplot(aes(x = GDPperCapita, y = ShareBiomass)) +
  geom_point(aes(colour = factor(Year)), size = 2) +
  geom_line(aes(group = Country), colour = "grey") +
  ggtitle("Share of biofuel in residential consumption vs GDP per capita, 2000 to 2017") +
  theme_classic() +
  theme(legend.position = c(0.8, 0.4),
        legend.box = "horizontal",
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent) +
  ylab("Share of biofuel in residential energy consumption") +
  xlab("GDP per capita (in current USD)") +
  labs(color = "Year") +
  geom_text(aes(label=ifelse((Year == 2000|
                                (ShareBiomass > 0.87) & 
                                (GDPperCapita < 1600)), 
                             '', Country), 
                hjust=-0.1, 
                vjust=-0.1), size = 4)

###################### Biomass production by type in Africa ####################

StatsExpanded %>%
  filter(Country == "Africa",
         Flow == "Total Final Consumption",
         Product %in% c("Firewood", "Agro-residues and waste", "Charcoal")) %>%
  ggplot(aes(x = Year, y = Value, fill = Product)) +
  geom_col() +
  theme_classic() +
  ggtitle("Solid biofuels consumption in Africa") +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.title = element_text(hjust = 0.5)
  ) +
  ylab("Total Final Consumption (Mt)") +
  scale_fill_manual(values = StatsSolidBioProductColors)

###################### Biogas consumption by sectors in Africa #################

StatsExpanded %>%
  filter(Country == "Africa",
  #exclude the total final consumption
         Flow %in% EndUseSectors[-1],
         Product == "Biogas") %>%
  ggplot(aes(x = Year, y = Value, fill = Flow)) +
  geom_col() +
  theme_classic() +
  ggtitle("Biogas consumption in Africa") +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.title = element_text(hjust = 0.5)
  ) +
  ylab("Consumption (TJ)") +
  scale_fill_manual(values = EndUseSectorColors[-1])

################### Charcoal production by region in Africa ####################

CharcoalStats %>%
  filter(Flow == "ShareFirewoodtoCharcoal",
         Country %in% Regions) %>%
  ggplot(aes(x = Year, y = Value, colour = Country)) +
  geom_line(size=1) +
  theme_classic() +
  ggtitle("Share of wood used for charcoal production") +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
  ylab("Share") +
  scale_color_manual(values = RegionsColors[-1])

CharcoalStats %>%
  filter(Flow == "MassRatio",
         Country %in% Regions) %>%
  ggplot(aes(x = Year, y = Value, colour = Country)) +
  geom_line(size=1) +
  theme_classic() +
  ggtitle("Mass ratio Charcoal output / Wood Input") +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
  ylab("Ratio") +
  scale_color_manual(values = RegionsColors[-1])

################### Charcoal production by region in Africa ####################

CharcoalStats %>%
  filter(Flow %in% c("ShareFirewoodtoCharcoal", "MassRatio"),
         Country %in% c("Africa", Regions)) %>%
  mutate(Flow = factor(Flow, 
                       levels = c("ShareFirewoodtoCharcoal", "MassRatio"),
                       labels = c("Share of firewood used for charcoal production", 
                                  "Mass ratio charcoal / firewood"))) %>%
  ggplot(aes(x = Year, y = Value, colour = Flow)) +
  geom_line(size=1) +
  theme_classic() +
  facet_wrap(~Region) +
  ggtitle("Share of wood used for charcoal production and mass ratio Charcoal output / Wood Input") +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
  ylab("Share / Ratio")


##### Biofuel and waste input to electricity production and consumption in Industry

StatsExpanded %>%
  filter(Flow == "Input to electricity production",
         Country %in% c("Africa", Regions)) %>%
  ggplot(aes(x = Year, y = Value, fill = Product)) +
  geom_col() +
  theme_classic() +
  facet_wrap(~Region) +
  ggtitle("Biofuel inputs to electricity generation") +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.title = element_text(hjust = 0.5)) +
  ylab("Inputs to electricity generation (Mt)") +
  scale_fill_manual(values = StatsSolidBioProductColors)


StatsExpanded %>% 
  filter(Flow %in% BalanceProductlabels[-c(1, 6)],
         Product == "Electricity") %>% 
  ggplot(aes(x = Year, y = Value, fill = Flow)) +
  geom_col(position = "fill") +
  theme_classic() +
  facet_wrap(~Region) +
  ggtitle("Electricity generation by source") +
  theme(
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.title = element_text(hjust = 0.5)) +
  ylab("Share of electricity generation") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = BalanceProductColors[-c(1,6)])

test <- StatsExpanded %>% 
  filter(Flow == "Input to electricity production",
         Region == "Central Africa")

test2 <- StatsExpanded %>% 
  filter(Flow == "Biofuels & waste",
         Region == "Central Africa")