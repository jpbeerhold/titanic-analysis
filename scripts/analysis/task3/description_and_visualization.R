data <- read.csv("titanic-analysis/data/processed/titanic_cleaned.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

source("../../../task2/statistics_categorial_variables.R")
source("../../../task2/statistics_metric_variables.R")
source("../../../task2/visualization_categorical_variables.R")
source("../../../task2/statistics_bivariate_categorical_variables.R")
source("../../../task2/bivariate_statistics_metric_dichotomous.R")

##Aufgabe 2-a-i
metric_variables(data$Age)
metric_variables(data$Fare)
metric_variables(data$SibSp)
metric_variables(data$Parch)

library(ggplot2)

# Altersverteilung
ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Altersverteilung der Passagiere", x = "Alter", y = "Häufigkeit")

# Fare-Verteilung
ggplot(data, aes(x = Fare)) +
  geom_histogram(binwidth = 10, fill = "green", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Verteilung der Ticketpreise (Fare)", x = "Fare", y = "Häufigkeit")

