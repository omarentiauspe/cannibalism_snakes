
## Note: in some cases, "Country" refers to the column name in our data. 
## Modify it as needed to match the actual column names in your dataset.

## required packages 

install.packages(c("ggplot2", "dplyr", "rnaturalearth", "rnaturalearthdata", "sf", "cowplot", "viridis"))
library(viridis) ## (colorblind friendly)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(cowplot)

## don't forget to load your own data

data <- read.csv("example.csv")

## to load a world map with country geographic data

world <- ne_countries(scale = "medium", returnclass = "sf")

## to check if the country names in the data match the names in the package:

setdiff(data$Country, world$name)

## to correct the country name (e.g., "United States" to "United States of America") if necessary.

data <- data %>%
  mutate(Country = recode(Country,
                          "United States" = "United States of America"
  ))

## merging the world and data datasets by matching the name column in world with the Country column in data.

world_data <- left_join(world, data, by = c("name" = "Country"))

## to set the color scales. 

world_data <- world_data %>%
  mutate(Event_Class = factor(
    case_when(
      Count == 1 ~ "1",
      Count <= 3 ~ "2–3",
      Count <= 6 ~ "4–6",
      Count <= 10 ~ "7–10",
      Count <= 20 ~ "11–20",
      Count > 20 ~ ">20",
      TRUE ~ NA_character_
    ),
    levels = c(">20", "11–20", "7–10", "4–6", "2–3", "1")
  ))

## map showing the presence of cannibalism worldwide

world_data$Has_Event <- ifelse(!is.na(world_data$Count), "Present", "Absent")

map_single_color <- ggplot(world_data) +
  geom_sf(aes(fill = Has_Event), color = "black", size = 0.1) +
  scale_fill_manual(
    values = c("Present" = "#08306B", "Absent" = "gray90"),
    name = "Cannibalism",
    labels = c("Absent", "Present")
  ) +
  theme_minimal() +
  labs(title = "Countries with documented snake cannibalism events") +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
    legend.title = element_text(face = "bold")
  )

## worldwide map displaying the scale/levels of cannibalism events

map_heat <- ggplot(world_data) +
  geom_sf(aes(fill = Event_Class), color = "black", size = 0.1) +
  scale_fill_viridis_d(
    option = "viridis",
    direction = -1,
    na.value = "gray90",
    name = "Cannibalism events"
  ) +
  theme_minimal() +
  labs(
    title = "Global distribution of snake cannibalism events",
    caption = "Countries with no data are shown in gray.\nNote: Events from Wales were grouped under United Kingdom."
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.title = element_text(face = "bold")
  )

## plotting them togethem

plot_grid(
  map_single_color,
  map_heat,
  labels = c("A", "B"),
  nrow = 2
)

## plotting it individually

plot(map_single_color)
plot(map_heat)
