## Header ####
## Title: "Exploring Global Crop Yield Trends: An Analysis of the Factors Impacting Crop Yields Over Time." ####
## Author- Student ID: 22358100 ####
## C7083 Assignment #####
## Last edited 06-03-2023, 18:45 ####


## Contents ####

## 01. Setup a working directory.
## 02. Packages,Libraries and Datasets.
## 03. Bar chart of top 5 countries by average crop yield in 2018.
## 04. Chloropleth map of Wheat around the globe in the year 2018.
## 05. Stacked bar chart for yields of different crops from 1961 to 2018.
## 06. Arable land use line chart.
## 07. Heatmap showing the correlation between crops.
## 08. Comparing two countries for Fertilizer use and yield.


## 01. Setup a working directory ####

setwd("D:/HAU/Modules/C7083")

## 02. Installing packages, Loading the libraries and the Datasets ####

install.packages("tidytuesdayR")

key_crop_yields <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')
fertilizer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/cereal_crop_yield_vs_fertilizer_application.csv')
tractors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/cereal_yields_vs_tractor_inputs_in_agriculture.csv')
land_use <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/land_use_vs_yield_change_in_cereal_production.csv')
arable_land <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/arable_land_pin.csv')


library(dplyr)

library(tidyverse)

library(tidyr)

library(ggplot2)

install.packages("tinytex")

## 03. Bar chart of top 5 countries by average crop yield in 2018 ####

# Filter for the most recent year available (2018)
key_crop_yields_2018 <- key_crop_yields %>% 
  filter(Year == 2018)

colnames(key_crop_yields)

# Create a bar chart of top 5 countries by average crop yield in 2018
library(ggplot2)


# Load libraries
library(tidyverse)

# top 5 countries wheat yield in 2018

# Filter for 2018 and select top 5 countries by average yield

key_crop_yields_2018 <- key_crop_yields %>%
  filter(Year == 2018) %>%
  group_by(Entity) %>%
  summarise(yield = mean(`Wheat (tonnes per hectare)`, na.rm = TRUE)) %>%
  top_n(5, yield)

# Create bar chart
ggplot(key_crop_yields_2018, aes(x = Entity, y = yield, fill = Entity)) +
  geom_col() +
  scale_fill_manual(values = c("#F8766D", "#00BFC4", "#E76BF3", "#7CAE00", "#619CFF")) +
  labs(title = "Top 5 Countries by Average Wheat Yield in 2018",
       x = "Country",
       y = "Average Yield (tonnes per hectare)",
       fill = "") +
  theme(plot.title = element_text(hjust = 0.5))


## 04. Chloropleth map of Wheat around the globe in the year 2018 ####

library(ggplot2)
library(maps)
library(dplyr)

key_crop_yields <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')

# Filter data for the most recent year (2018)
crop_filtered <- key_crop_yields %>%
  filter(Year == 2018)

# Load the world map data
world_map <- map_data("world")

# Merge crop yields data with world map data by country name
crop_map <- merge(world_map, crop_filtered, by.x = "region", by.y = "Entity")

# Create a choropleth map of crop yields by region
ggplot(crop_map, aes(x = long, y = lat, group = group, fill = `Wheat (tonnes per hectare)`)) +
  geom_polygon() +
  scale_fill_gradient(low = "white", high = "darkgreen", name = "Wheat Yield\n(tonnes per hectare)") +
  coord_equal() +
  labs(title = "Wheat Yields by Region (2018)", 
       x = "", y = "", 
       fill = "") +
  theme_void()
#####

## 05. Stacked bar chart for yields of different crops from 1961 to 2018 ####

library(tidyverse)

# Filter data for years 1961-2018
crop_filtered <- key_crop_yields %>%
  filter(Year >= 1961, Year <= 2018)

# Reshape data into long format
crop_long <- crop_filtered %>%
  pivot_longer(cols = c("Wheat (tonnes per hectare)", "Rice (tonnes per hectare)", "Maize (tonnes per hectare)", "Soybeans (tonnes per hectare)", "Potatoes (tonnes per hectare)", "Beans (tonnes per hectare)", "Peas (tonnes per hectare)", "Cassava (tonnes per hectare)", "Barley (tonnes per hectare)", "Cocoa beans (tonnes per hectare)", "Bananas (tonnes per hectare)"), 
               names_to = "Crop Type", 
               values_to = "Yield")

# Create a stacked bar chart with different colors for each crop
ggplot(crop_long, aes(x = Year, y = Yield, fill = `Crop Type`)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Crop Yield (tonnes per hectare)", 
       fill = "Crop Type",
       title = "Crop Yields by Year") +
  theme_minimal() +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999", "#EFEFEF", "#000000", "#8B0000"))
######

## 06. Arable land use line chart ####

library(ggplot2)


# Filter for world data only
arable_land_world <- arable_land[arable_land$Entity == "World",]

# Plot the line chart
ggplot(arable_land_world, aes(x = Year, y = `Arable land needed to produce a fixed quantity of crops ((1.0 = 1961))`)) +
  geom_line(color = "blue") +
  scale_x_continuous(breaks = seq(min(arable_land_world$Year), max(arable_land_world$Year), by = 5)) +
  theme_minimal() +
  labs(title = "Trend in Arable Land Use for the World over Time",
       x = "Year",
       y = "Arable Land Use")
####


## 07. Heat map showing the correlation between crops ####

# Select the columns containing crop yields
crop_cols <- c(4:13)

# Calculate the correlation matrix between crop yields
corr_mat <- cor(key_crop_yields[, crop_cols])

install.packages("reshape2")
library(reshape2)

library(ggplot2)

# create correlation matrix for crop yields
corr_mat <- cor(key_crop_yields[,4:13])

# melt correlation matrix into dataframe
corr_df <- melt(corr_mat)

# rename columns
colnames(corr_df) <- c("Crop 1", "Crop 2", "Correlation")

# create heatmap
ggplot(corr_df, aes(x = `Crop 1`, y = `Crop 2`, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "red", high = "green", midpoint = 0, 
                       name = "Correlation", guide = guide_colorbar(reverse = TRUE)) +
  theme_minimal() +
  labs(title = "Correlation between Crop Yields",
       x = "Crop Type",
       y = "Crop Type")


## 08. Comparing two countries for Fertilizer use and yield ####

## 08.1 Fertilizer used in 2002-2017

library(dplyr)

# Filter data for India and United Kingdom for years 2002-2017
fert_filtered <- fertilizer %>%
  filter(Entity %in% c("India", "United Kingdom"), Year >= 2002, Year <= 2017)

# View the filtered data
fert_filtered

library(ggplot2)

colnames(fert_filtered)

ggplot(fert_filtered, aes(x = Year, y = `Nitrogen fertilizer use (kilograms per hectare)`, color = Entity)) +
  geom_line() +
  labs(title = "Fertilizer Use Over Time",
       x = "Year",
       y = "Nitrogen fertilizer use (kg/ha)",
       color = "Country") +
  theme_minimal()
# cereal yield

# Create grouped bar chart
ggplot(fert_filtered, aes(x = Year, y = `Cereal yield (tonnes per hectare)`, fill = Entity)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  labs(x = "Year", y = "Cereal Yield (tonnes per hectare)", title = "Cereal Yield for India and UK (2002-2017)")

#

## 8.02 Relation between fertilizer use and cereal yield ####
colnames(fertilizer)


# Filter data for years 1961-2018
fertilizer_filtered <- fertilizer %>%
  filter(Year >= 1961, Year <= 2018)

# Create a scatter plot of fertilizer use and cereal yield
ggplot(fertilizer_filtered, aes(x = `Nitrogen fertilizer use (kilograms per hectare)`, y = `Cereal yield (tonnes per hectare)`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Nitrogen fertilizer use (kg/ha)", y = "Cereal yield (tonnes/ha)", 
       title = "Relationship between Fertilizer Use and Cereal Yield") +
  theme_minimal() +
  scale_x_continuous(limits = c(0, NA), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, NA), expand = c(0, 0))


## only fertilizer ####

library(tidyverse)

# Filter data for years 1961-2018
fert_filtered <- fertilizer %>%
  filter(Year >= 1961, Year <= 2018)

# Create a bar chart of fertilizer use over time
ggplot(fert_filtered, aes(x = Year, y = `Nitrogen fertilizer use (kilograms per hectare)`)) +
  geom_bar(stat = "identity", fill = "#0072B2") +
  labs(x = "Year", y = "Nitrogen Fertilizer Use (kg/ha)", title = "Nitrogen Fertilizer Use Over Time") +
  theme_minimal()


install.packages("xfun")

tinytex::install_tinytex()
library(tinytex)
