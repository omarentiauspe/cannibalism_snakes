## required packages

library(ggpubr)
library(dplyr)

# Seu dado já carregado em my_data

my_data <- read.csv('example.csv')


# linear regression

linear_model <- lm(prey ~ predator, data = my_data)
slope <- round(coef(linear_model)[2], 2)
r2 <- round(summary(linear_model)$r.squared, 2)

# pearson cor scatterplot 

p1 <- ggscatter(my_data, x = "predator", y = "prey", 
                conf.int = TRUE, 
                cor.coef = TRUE, cor.method = "pearson",
                xlab = "Predator TTL size", ylab = "Prey TTL size")

# linear regression scatterplot

p2 <- ggscatter(my_data, x = "predator", y = "prey", 
                add = "reg.line", conf.int = TRUE,
                add.params = list(color = "blue", fill = "lightgray"),
                xlab = "Predator TTL size", ylab = "Prey TTL size") +
  annotate("text", x = min(my_data$predator), y = max(my_data$prey), 
           hjust = 0, vjust = 1.2,
           label = paste0("Slope = ", slope, "; R² = ", r2),
           size = 4)


## now the boxplots 
## filtering by Family with at least 5 records

my_data_filtrado <- my_data %>%
  group_by(Family) %>%
  filter(n() >= 5) %>%
  ungroup()

## ensuring that Family is a factor

my_data_filtrado$Family <- factor(my_data_filtrado$Family)

## boxplot for predator

p3 <- ggboxplot(my_data_filtrado, x = "Family", y = "predator",
                palette = c("#f51720", "#E7B800", "#FC4E07",
                            "#00AFBB", "#fa26a0", "#145da0", "#81b622"), 
                fill = "Family", add = "jitter") + 
  rotate_x_text(angle = 45) + 
  ylab("Predator TTL size")

## boxplot for prey

p4 <- ggboxplot(my_data_filtrado, x = "Family", y = "prey",
                palette = c("#f51720", "#E7B800", "#FC4E07",
                            "#00AFBB", "#fa26a0", "#145da0", "#81b622"), 
                fill = "Family", add = "jitter") + 
  rotate_x_text(angle = 45) +
  ylab("Prey TTL size")

## plotting all them together

ggarrange(p1, p2, p3, p4,
          ncol = 2, nrow = 2,
          labels = c("A", "B", "C", "D"))

## plotting them individually
plot(p1)
plot(p2)
plot(p3)
plot(p4)
