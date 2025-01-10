library(dplyr)
library(tidyverse)
library(ggplot2)
library(gganimate)
library(ggrepel)
library(rworldmap)
library(sf)
library(terra)
library(rworldxtra)
library(RColorBrewer)
library(knitr)
library(plotly)
library(htmlwidgets)
library(GGally)
library(stats)
library(rstudioapi)
#install.packages("sp")
library(sp)
#install.packages("geos")
library(geos)
#install.packages("stars")
library(stars)
#install.packages("eurostat")
#library(eurostat)
library(rnaturalearth)
library(rnaturalearthdata)

path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)


#Data Source: https://ec.europa.eu/eurostat/databrowser/view/ilc_pw01__custom_14386442/bookmark/table?lang=en&bookmarkId=20181ca1-da2c-45cc-a8be-7e8a6888403f
life_satisfaction_europe <- read.csv("Life_Satisfaction/life_satisfaction_europe.csv", stringsAsFactors = FALSE)

head(life_satisfaction_europe)
str(life_satisfaction_europe)
colnames(life_satisfaction_europe)

life_satisfaction_europe <- life_satisfaction_europe %>%
  mutate(geo = ifelse(geo == "Türkiye", "Turkey", geo))


#map
#map for Europe
world <- ne_countries(scale = "medium", returnclass = "sf")
europe <- world %>%
  filter(continent == "Europe" | name %in% c("Turkey"))

turcja_geom <- europe %>% filter(name == "Turkey")
print(turcja_geom)

#joining data with map
europe_data <- europe %>%
  left_join(life_satisfaction_europe, by = c("name" = "geo"))

europe_data <- europe_data %>%
  mutate(
    label_point = st_point_on_surface(geometry),
    label_x = st_coordinates(label_point)[, 1],
    label_y = st_coordinates(label_point)[, 2]
  ) %>%
  mutate(
    label_x = ifelse(name == "Norway", 10, label_x),  
    label_y = ifelse(name == "Norway", 62, label_y),
    label_x = ifelse(name == "Cyprus", 33.2, label_x),  
    label_y = ifelse(name == "Cyprus", 34.8, label_y),
    label_x = ifelse(name == "Turkey", 35, label_x),  
    label_y = ifelse(name == "Turkey", 39, label_y)
  )

#debugging
missing_countries <- europe$name[!europe$name %in% life_satisfaction_europe$geo]
print(missing_countries)

# vizualization
map <- ggplot(europe_data) +
  geom_sf(aes(fill = OBS_VALUE), color = "white") +
  geom_label(aes(x = label_x, y = label_y, label = round(OBS_VALUE, 1)), 
             size = 3, color = "black", fill = "white", label.size = 0.2) +
  scale_fill_viridis_c(option = "magma", na.value = "grey90", name = "Life Satisfaction") +
  theme_minimal() +
  labs(
    title = "Life Satisfaction in Europe in 2022",
    caption = "Source: Eurostat"
  ) +
  theme(
      plot.title = element_text(color = "darkred", size = 14, face = "bold", hjust = 0.5),
      legend.title = element_text(color = "darkred", size = 10, face = "bold"),
      legend.position = "right",
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      
      plot.caption = element_text(color = "purple4", size = 6, face = "bold", hjust = 0.5)
    ) +
  coord_sf(xlim = c(-30, 40), ylim = c(30, 75), expand = FALSE)


unique(europe$name)
unique(life_satisfaction_europe$geo)