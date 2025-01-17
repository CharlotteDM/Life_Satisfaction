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
library(corrplot)

path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)


#Data Source: https://ec.europa.eu/eurostat/databrowser/view/ilc_pw01__custom_14386442/bookmark/table?lang=en&bookmarkId=20181ca1-da2c-45cc-a8be-7e8a6888403f
life_satisfaction_europe <- read.csv("Life_Satisfaction/life_satisfaction_europe.csv", stringsAsFactors = FALSE)

head(life_satisfaction_europe)
str(life_satisfaction_europe)
colnames(life_satisfaction_europe)

life_satisfaction_europe <- life_satisfaction_europe %>%
  mutate(geo = ifelse(geo == "Türkiye", "Turkey", geo))

life_satisfaction_europe <- life_satisfaction_europe %>%
  mutate(geo = ifelse(geo == "Cyprus", "Cyprus", geo))


#map
#map for Europe
world <- ne_countries(scale = "medium", returnclass = "sf")
europe <- world %>%
  filter(continent == "Europe" | name %in% c("Cyprus", "Turkey"))


turcja_geom <- europe %>% filter(name == "Turkey")
print(turcja_geom)


# Adding Cyprus manually to the map data
cyprus <- world %>%
  filter(name == "Cyprus")

europe <- bind_rows(europe, cyprus)


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
    label_x = ifelse(name == "Cyprus", 33.5, label_x),  
    label_y = ifelse(name == "Cyprus", 34.5, label_y),
    label_x = ifelse(name == "Turkey", 35, label_x),  
    label_y = ifelse(name == "Turkey", 39, label_y)
  )

#debugging
missing_countries <- europe$name[!europe$name %in% life_satisfaction_europe$geo]
print(missing_countries)

print(life_satisfaction_europe %>% filter(geo == "Cyprus"))

print(europe_data %>% filter(name == "Norway") %>% select(name, label_x, label_y))



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


#Adding data for corrplot and fiuture analysis
gdp <- read.csv("gross_domestic_product_miilioneuro.csv", stringsAsFactors = FALSE)
healthcare_expenditure <- read.csv("total_heathcare_expenditure_millioneuro.csv", stringsAsFactors = F)
unemployment_rate <- read.csv("total_unemployment_rate_thosandperson .csv", stringsAsFactors = F)
expenditure_on_social_protection <- read.csv("expenditure_on_spcial_protection_percentageofGDP.csv", stringsAsFactors = F)
victims_homicide_sexexploitation <- read.csv("victims_of_homicide_and_sex_exploitation.csv", stringsAsFactors = F)
life_expectancy <- read.csv("life_expectancy.csv", stringsAsFactors = F)
fertility_rate <- read.csv("fertility_rate.csv", stringsAsFactors = F)
total_incomes_median <- read.csv("total_incomes_median_euro.csv", stringsAsFactors = F)
pollution <- read.csv("polution_percentage_2023.csv", stringsAsFactors = F )
noise <- read.csv("noise_pollution_percentage_2023.csv", stringsAsFactors = F)


#data standarization
datasets <- list(
  gdp = gdp,
  healthcare_expenditure = healthcare_expenditure,
  unemployment_rate = unemployment_rate,
  expenditure_on_social_protection = expenditure_on_social_protection,
  victims_homicide_sexexploitation = victims_homicide_sexexploitation,
  life_expectancy = life_expectancy,
  fertility_rate = fertility_rate,
  total_incomes_median = total_incomes_median,
  pollution = pollution,
  noise = noise,
  life_satisfaction = life_satisfaction_europe
)

str(datasets)

# Standardize and rename columns
standardized_datasets <- lapply(names(datasets), function(name) {
  df <- datasets[[name]]
  if ('OBS_VALUE' %in% names(df)) {
    df <- df %>% mutate(!!paste0(name, "_std") := scale(OBS_VALUE))  
    df <- df %>% select(geo, !!paste0(name, "_std"))  
  }
  return(df)
})

# Function for making one data frame by country
common_columns <- c("geo")

combined_data <- Reduce(function(x, y) merge(x, y, by = common_columns, all = TRUE), standardized_datasets)

# Removing rows with NA
combined_data <- na.omit(combined_data)

# Excluding rows starting with "Euro area" and "European Union"
combined_data <- combined_data %>%
  filter(!grepl("^Euro area|^European Union", geo))

# Drop geo column for correlation analysis
correlation_data <- combined_data %>% select(-geo)

# Ensure all columns are numeric
correlation_data <- data.frame(lapply(correlation_data, as.numeric))

# Shapiro-Wilk test for normality
shapiro_results <- sapply(correlation_data, shapiro.test)
print(shapiro_results)

# Calculate Spearman correlation matrix
correlation_matrix_spearman <- cor(correlation_data, method = "spearman")
str(correlation_matrix_spearman)

#new variable names
new_variable_names <- c("GDP", "Healthcare Expenditure", "Unemployment Rate", 
                        "Social Protection Expenditure", "Homicide & Sex Exploitation Victims", 
                        "Life Expectancy", "Fertility Rate", "Median Income", 
                        "Pollution", "Noise", "Life Satisfaction")

colnames(correlation_matrix_spearman) <- new_variable_names
rownames(correlation_matrix_spearman) <- new_variable_names



# Drawing corrplot with improved colors and larger size
corrplot(
  correlation_matrix_spearman,
  method = "color",
  type = "upper",
  col = colorRampPalette(c("blue", "white", "red"))(200),  
  tl.col = "black",  
  tl.srt = 45,       
  addCoef.col = "black",  
  number.cex = 0.7,  
  tl.cex = 0.7,     
  mar = c(1, 1, 3, 1)
)

# Add the main title, closer to the plot
title(
  main = "Correlation Matrix of Life Satisfaction and Socioeconomic Variables \nin European Countries",
  line = -14,
  cex.main = 1.5  
)

# Add a subtitle with data source and years
title(
  sub = "Data Source: Eurostat.",
  line = 3, 
  cex.sub = 0.8  
)

# Add additional information below the plot
mtext(
  "Most data from 2022, except Pollution and Noise (2023). \nData standardized; Spearman correlation used due to non-normal distribution",
  side = 1, line = 4.2, cex = 0.6
)

#