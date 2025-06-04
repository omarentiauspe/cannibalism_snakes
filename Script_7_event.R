## required packages

library(dplyr)
library(ggplot2)
library(viridis)
library(tidyr)

## you can import your data using read.csv

data <- tribble(
  ~Family, ~`Maternal-offspring cannibalism`, ~`Sexual cannibalism`, ~`Cannibalism between offspring`, ~`Combat-dance cannibalism`, ~Undetermined,
  "Aniliidae", 0, 0, 0, 0, 1,
  "Atractaspididae", 0, 0, 0, 0, 3,
  "Boidae", 14, 2, 0, 0, 14,
  "Colubridae", 4, 0, 5, 0, 131,
  "Dipsadidae", 3, 0, 5, 0, 26,
  "Elapidae", 0, 0, 1, 2, 90,
  "Erycidae", 0, 0, 0, 0, 1,
  "Lamprophiidae", 0, 0, 0, 0, 5,
  "Natricidae", 5, 0, 2, 0, 18,
  "Psammophiidae", 0, 0, 0, 0, 9,
  "Pseudoxyrhophiidae", 0, 0, 0, 0, 7,
  "Pythonidae", 1, 0, 2, 0, 6,
  "Sanziniidae", 1, 0, 0, 0, 0,
  "Tropidophiidae", 0, 0, 0, 0, 1,
  "Viperidae", 3, 0, 12, 0, 89
)

## defining six colors using the plasma palette (colorblind friendly)

plasma_colors <- plasma(6)[3:8]

## to transform the data into a long format with non-zero counts and orders the Family factor alphabetically.

data_long <- data %>%
  pivot_longer(
    cols = -Family,
    names_to = "Cannibalism_type",
    values_to = "Count"
  ) %>%
  filter(Count > 0) %>%
  mutate(Family = factor(Family, levels = sort(unique(data$Family))))

## reverse Family order for plotting

data_long$Family <- factor(data_long$Family, levels = rev(sort(unique(data_long$Family))))

## calculate total counts per Family

totals <- data_long %>%
  group_by(Family) %>%
  summarise(Total = sum(Count))

## match factor levels with data_long for plotting.

totals$Family <- factor(totals$Family, levels = levels(data_long$Family))

## define spacing for text labels

spacing <- max(totals$Total) * 0.02

## plot the image

ggplot(data_long, aes(x = Count, y = Family, fill = Cannibalism_type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = plasma_colors) +
  geom_text(data = totals, aes(x = Total + spacing, y = Family, label = Total), 
            inherit.aes = FALSE, size = 3) +
  labs(
    x = "Number of cannibalism events",
    y = "Family",
    fill = "Cannibalism type",
    title = "Cannibalism types by snake family"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.y = element_text(size = 10)
  )
