library(ggplot2)

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

##Altersverteilung
ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Altersverteilung der Passagiere", x = "Alter", y = "Häufigkeit")

##Fare-Verteilung
ggplot(data, aes(x = Fare)) +
  geom_histogram(binwidth = 10, fill = "green", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Verteilung der Ticketpreise (Fare)", x = "Fare", y = "Häufigkeit")

##Aufgabe 2-a-ii
calculate_categorical_stats(data, c("Sex", "Pclass", "Embarked", "Survived"))

##Anzahl der Passagiere pro Klasse
ggplot(data, aes(x = Pclass, fill = factor(Pclass))) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Anzahl der Passagiere pro Klasse", x = "Passagierklasse", y = "Anzahl")

##Geschlechterverteilung
ggplot(data, aes(x = Sex, fill = Sex)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Geschlechterverteilung", x = "Geschlecht", y = "Anzahl")

##Aufgabe 2-a-iv
##Überlebensrate nach Geschlecht
ggplot(data, aes(x = Sex, fill = factor(Survived))) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Überlebensrate nach Geschlecht", x = "Geschlecht", y = "Anteil überlebt", fill = "Überlebt")

##Überlebensrate nach Passagierklasse
ggplot(data, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Überlebensrate nach Passagierklasse", x = "Passagierklasse", y = "Anteil überlebt", fill = "Überlebt")

