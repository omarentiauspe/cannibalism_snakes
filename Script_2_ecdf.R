library(ggplot2)
library(ggpubr)
library(dplyr)
library(RColorBrewer)

# Read and prepare data
my_data <- read.csv('DAT.csv')
df <- data.frame(my_data)
df$Year <- as.numeric(df$Year)

# ECDF Plot
ecdf_plot <- ggplot(df, aes(x = Year, color = Publication_type, shape = Publication_type)) +
  stat_ecdf(geom = "point", size = 3) +
  stat_ecdf(geom = "line", aes(group = Publication_type), color = "black", linetype = "dashed") +
  geom_vline(xintercept = c(1967, 1980, 1999), linetype = "dashed", color = "gray40") +
  annotate("text", x = 1967, y = 0.05, label = "HR", angle = 90, vjust = -0.5, hjust = 0, size = 4) +
  annotate("text", x = 1980, y = 0.05, label = "HB", angle = 90, vjust = -0.5, hjust = 0, size = 4) +
  annotate("text", x = 1999, y = 0.05, label = "HN", angle = 90, vjust = -0.5, hjust = 0, size = 4) +
  scale_x_continuous(breaks = seq(floor(min(df$Year, na.rm = TRUE) / 10) * 10,
                                  ceiling(max(df$Year, na.rm = TRUE) / 10) * 10, by = 20)) +
  xlab("Year") +
  ylab("Cumulative Probability") +
  ggtitle("ECDF of Publication Year") +
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(16, 17, 15)) +
  theme_minimal(base_size = 14)


# Create decade summary for histogram
df_hist <- df %>%
  mutate(Decade = 10 * (Year %/% 10)) %>%
  group_by(Decade, Publication_type) %>%
  summarise(Occurrences = n(), .groups = "drop") %>%
  na.omit()

# Histogram
hist_plot <- ggplot(df_hist, aes(x = factor(Decade), y = Occurrences, fill = Publication_type)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "Dark2") +
  xlab("Decade") +
  ylab("Occurrences") +
  ggtitle("Published Occurrences per Decade") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Combine plots vertically
ggarrange(hist_plot, ecdf_plot,
          ncol = 1, nrow = 2,
          heights = c(1, 1.2),  # ECDF a bit larger
          common.legend = TRUE, legend = "right")
