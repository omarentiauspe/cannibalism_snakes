## required packages

library(dplyr)
library(ggplot2)
library(viridis)
library(tidyr)

## you can import your data using read.csv

data <- tribble(
  ~Family,             ~Captivity, ~Nature, ~`Dissection/Undetermined`,
  "Aniliidae",               0,         1,              0,
  "Atractaspididae",         1,         0,              2,
  "Boidae",                 23,         6,              1,
  "Colubridae",             48,        42,             50,
  "Dipsadidae",             19,         9,              6,
  "Elapidae",               28,        22,             43,
  "Erycidae",                1,         0,              0,
  "Lamprophiidae",           1,         0,              4,
  "Natricidae",             15,         5,              5,
  "Psammophidae",            0,         6,              3,
  "Pseudoxyrhophiidae",      6,         1,              0,
  "Pythonidae",              7,         0,              2,
  "Sanziniidae",             1,         0,              0,
  "Tropidophiidae",          1,         0,              0,
  "Viperidae",              61,        26,             17
)

## defining three colors using the plasma palette (colorblind friendly)

plasma_colors <- plasma(3, begin = 0.1, end = 0.6)

## to transform the data into a long format with non-zero counts and orders the Family factor alphabetically.

data_long <- data %>%
  pivot_longer(
    cols = -Family,
    names_to = "Occurrence_type",
    values_to = "Count"
  ) %>%
  filter(Count > 0) %>%
  mutate(Family = factor(Family, levels = rev(sort(unique(Family)))))

## calculate total counts per Family

totals <- data_long %>%
  group_by(Family) %>%
  summarise(Total = sum(Count))

## match factor levels with data_long for plotting.

totals$Family <- factor(totals$Family, levels = levels(data_long$Family))

## define spacing for text labels

spacing <- max(totals$Total) * 0.02

## plot the image

ggplot(data_long, aes(x = Count, y = Family, fill = Occurrence_type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = plasma_colors) +
  geom_text(data = totals, aes(x = Total + spacing, y = Family, label = Total), 
            inherit.aes = FALSE, size = 3) +
  labs(
    x = "Number of events",
    y = "Family",
    fill = "Occurrence type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.y = element_text(size = 10)
  )
