#######-------PKO Map-------#######

pacman::p_load(
  "tidyverse", # Data Manipulation and Visualization
  "sf", # Maps
  "rnaturalearth", # Maps
  "rnaturalearthdata", # Maps
  install = FALSE
)

# Load the Earth Spatial Polygons

world_sf <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)

# Rename Country IDs for Merging That Don't Match

world_sf <- world_sf %>%
  mutate(sovereignt = str_replace(
    sovereignt, "Republic of Serbia", "Serbia"))

# Merge Earth Data With Synth Data Set

map_data <- ucdp %>%
  full_join(world_sf, by = c("country_name" = "sovereignt")) %>%
  # Drop Antarctica
  filter(name_long != "Antarctica")

# Create Text PKO Data

map_data <- map_data %>%
  mutate(ever_pko_txt = case_when(
    ever_pko == 1 ~ "PKO",
    ever_pko == 0 ~ "No PKO"
  ))

# Start Creating the Map

pko_map <- map_data %>% 
  ggplot() + 
  geom_sf(
    aes(geometry = geometry, fill = ever_pko_txt),
    color = "black",
    size = .2,
    na.rm = T
  ) +
  
  # Adjust Color Scales
  
  scale_fill_viridis_d(
    na.translate = FALSE,
    begin = 0.90,
    end = 0.50,
    option = "mako"
  ) +
  
  # Legend and Margins Customization
  
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(), 
    axis.text.y=element_blank(),  
    axis.ticks.y=element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 8, family = "serif", vjust = 1),
    legend.key.height = unit(0.25, 'cm'),
    legend.text = element_text(size = 6, family = "serif"),
    legend.key.width = unit(1, 'cm'),
    plot.margin = unit(c(-1, -0.7, -1, -0.7), "cm")
  ) +
  
  # Add Labels
  
  labs(
    fill = "")

# Save the Map

ggsave(
  "pko_map.png",
  width = 6,
  height = 4,
  path = "C:/Users/brian/Desktop/Peacebuilding Dissertation/PKO/Graphics"
)