library(ggplot2)
library(dplyr)
library(vcd)
library(reshape2)
library(GGally)

data <- read.csv("titanic-analysis/data/processed/titanic_cleaned.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

source("../../../task2/statistics_categorial_variables.R")
source("../../../task2/statistics_metric_variables.R")
source("../../../task2/visualization_categorical_variables.R")
source("../../../task2/statistics_bivariate_categorical_variables.R")
source("../../../task2/bivariate_statistics_metric_dichotomous.R")

##Referenz: Aufgabe 2-a-i
metric_variables(data$Age)
metric_variables(data$Fare)
metric_variables(data$SibSp)
metric_variables(data$Parch)

########################################
###Altersverteilung
metric_age <- metric_variables(data$Age)

##Histogramm mit Kennzahlen (Mittelwert, Median, Modus)
ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = metric_age$ArithmetischesMittel, color = "Mittelwert"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = metric_age$Median, color = "Median"), linetype = "dotted", size = 1) +
  geom_vline(aes(xintercept = metric_age$Modus, color = "Modus"), linetype = "solid", size = 1) +
  scale_color_manual(name = "Statistiken", values = c("Mittelwert" = "red", "Median" = "blue", "Modus" = "green")) +
  theme_minimal() +
  labs(title = "Altersverteilung mit statistischen Kennwerten", x = "Alter", y = "Häufigkeit")

##...

##Boxplot mit Interquartilsabstand und Spannweite
ggplot(data, aes(y = Age)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.color = "red", outlier.shape = 16) +
  geom_hline(yintercept = metric_age$Quartile[1], linetype = "dashed", color = "blue") +  # 25% Quartil
  geom_hline(yintercept = metric_age$Quartile[2], linetype = "dashed", color = "blue") +  # 50% Quartil (Median)
  geom_hline(yintercept = metric_age$Quartile[3], linetype = "dashed", color = "blue") +  # 75% Quartil
  geom_hline(yintercept = metric_age$Minimum, linetype = "solid", color = "black") +
  geom_hline(yintercept = metric_age$Maximum, linetype = "solid", color = "black") +
  theme_minimal() +
  labs(title = "Boxplot der Altersverteilung", y = "Alter")

##...

##Dichtekurve für Schiefe & Kurtosis
ggplot(data, aes(x = Age)) +
  geom_density(fill = "blue", alpha = 0.3) +
  geom_vline(aes(xintercept = metric_age$ArithmetischesMittel, color = "Mittelwert"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = metric_age$Median, color = "Median"), linetype = "dotted", size = 1) +
  scale_color_manual(name = "Statistiken", values = c("Mittelwert" = "red", "Median" = "blue")) +
  theme_minimal() +
  labs(title = "Dichtekurve der Altersverteilung", x = "Alter", y = "Dichte")

##...

####################

###Fare-Verteilung
metric_fare <- metric_variables(data$Fare)

##Histogramm mit Kennzahlen (Mittelwert, Median, Modus)
ggplot(data, aes(x = Fare)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = metric_fare$ArithmetischesMittel, color = "Mittelwert"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = metric_fare$Median, color = "Median"), linetype = "dotted", size = 1) +
  geom_vline(aes(xintercept = metric_fare$Modus, color = "Modus"), linetype = "solid", size = 1) +
  scale_color_manual(name = "Statistiken", values = c("Mittelwert" = "red", "Median" = "blue", "Modus" = "green")) +
  theme_minimal() +
  labs(title = "Ticketpreisverteilung mit statistischen Kennwerten", x = "Ticketpreis", y = "Häufigkeit")

##...

##Boxplot mit Interquartilsabstand und Spannweite
ggplot(data, aes(y = Fare)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.color = "red", outlier.shape = 16) +
  geom_hline(yintercept = metric_fare$Quartile[1], linetype = "dashed", color = "blue") +  # 25% Quartil
  geom_hline(yintercept = metric_fare$Quartile[2], linetype = "dashed", color = "blue") +  # 50% Quartil (Median)
  geom_hline(yintercept = metric_fare$Quartile[3], linetype = "dashed", color = "blue") +  # 75% Quartil
  geom_hline(yintercept = metric_fare$Minimum, linetype = "solid", color = "black") +
  geom_hline(yintercept = metric_fare$Maximum, linetype = "solid", color = "black") +
  theme_minimal() +
  labs(title = "Boxplot der Ticketpreisverteilung", y = "Ticketpreis")

##...

##Dichtekurve für Schiefe & Kurtosis
ggplot(data, aes(x = Fare)) +
  geom_density(fill = "blue", alpha = 0.3) +
  geom_vline(aes(xintercept = metric_fare$ArithmetischesMittel, color = "Mittelwert"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = metric_fare$Median, color = "Median"), linetype = "dotted", size = 1) +
  scale_color_manual(name = "Statistiken", values = c("Mittelwert" = "red", "Median" = "blue")) +
  theme_minimal() +
  labs(title = "Dichtekurve der Ticketpreisverteilung", x = "Ticketpreis", y = "Dichte")

##...

####################

##Paarweise Korrelationen der metrischen Variablen
numeric_vars <- data %>% select(Age, Fare, SibSp, Parch)
ggpairs(numeric_vars, title = "Paarweise Korrelationen der metrischen Variablen")

##...

########################################

##Referenz: Aufgabe 2-a-ii
##Visualisation von Geschlecht, Passagierklasse, Einsteigehafen & Überlebensrate
categorical_stats <- calculate_categorical_stats(data, c("Sex", "Pclass", "Embarked", "Survived"))

# Balkendiagramm für Häufigkeiten
ggplot(categorical_stats, aes(x = Category, y = Frequency, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Häufigkeit der Kategorien", x = "Kategorie", y = "Anzahl") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")

##...

#########################################################################################################################
##Analysen von Überlebensrate und Ticketpreis gegen andere Variablen
## (i) Wie verhält sich die Überlebensrate gegen andere Variablen?
##Referenz: Aufgabe 2-a-iii, Aufgabe 2-a-v
###Überlebensrate nach Geschlecht
visualize_categorical(data, "Sex", "Survived")

##...

##Mosaikplot Überlebensrate nach Geschlecht
bivariate_survived_sex <- bivariate_two_categorial(data, "Survived", "Sex")

mosaicplot(bivariate_survived_sex$Kontingenztafel, col = c("lightblue", "pink"), 
           main = "Mosaikplot: Überlebensrate nach Geschlecht", 
           xlab = "Geschlecht", ylab = "Überlebt")

##...

##Heatmap für Überlebensrate nach Geschlecht
bivariate_survived_sex_df <- as.data.frame(bivariate_survived_sex$Kontingenztafel)
colnames(bivariate_survived_sex_df) <- c("Survived", "Sex", "Frequency")

ggplot(bivariate_survived_sex_df, aes(x = Survived, y = Sex, fill = Frequency)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(title = "Heatmap der Kontingenztafel: Überlebensrate nach Geschlecht",
       x = "Überlebensrate", y = "Geschlecht", fill = "Häufigkeit")

##...

####################

###Überlebensrate nach Passagierklasse
visualize_categorical(data, "Pclass", "Survived")

##...

##Mosaikplot Überlebensrate nach Passagierklasse
bivariate_survived_Pclass <- bivariate_two_categorial(data, "Survived", "Pclass")

mosaicplot(bivariate_survived_Pclass$Kontingenztafel, col = c("yellow", "orange", "red"), 
           main = "Mosaikplot: Überlebensrate nach Klasse", 
           xlab = "Überlebt", ylab = "Klasse")

##...

##Heatmap für Überlebensrate nach Passagierklasse
bivariate_survived_Pclass_df <- as.data.frame(bivariate_survived_Pclass$Kontingenztafel)
colnames(bivariate_survived_Pclass_df) <- c("Survived", "Pclass", "Frequency")

ggplot(bivariate_survived_Pclass_df, aes(x = Survived, y = Pclass, fill = Frequency)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(title = "Heatmap der Kontingenztafel: Überlebensrate nach Geschlecht",
       x = "Überlebensrate", y = "Geschlecht", fill = "Häufigkeit")

##...

####################

###Überlebensrate nach Alter
ggplot(data, aes(x = factor(Survived), y = Age, fill = factor(Survived))) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Überlebensrate nach Alter", x = "Überlebt", y = "Alter", fill = "Überlebt")

##...

####################

###Überlebensrate nach Anzahl der Geschwister/Ehepartner
visualize_categorical(data, "SibSp", "Survived")

##...

####################

###Überlebensrate nach Anzahl der Eltern/Kinder
visualize_categorical(data, "Parch", "Survived")

##...

####################

###Überlebensrate nach Einsteigehafen
visualize_categorical(data, "Embarked", "Survived")

##...

##Mosaikplot Überlebensrate nach Einsteigehafen
bivariate_survived_embarked <- bivariate_two_categorial(data, "Survived", "Embarked")

mosaicplot(bivariate_survived_embarked$Kontingenztafel, col = c("yellow", "orange", "red"), 
           main = "Mosaikplot: Überlebensrate nach Einsteigehafen", 
           xlab = "Überlebt", ylab = "Einsteigehafen")

##...

##Heatmap für Überlebensrate nach Einsteigehafen
bivariate_survived_embarked_df <- as.data.frame(bivariate_survived_Pclass$Kontingenztafel)
colnames(bivariate_survived_embarked_df) <- c("Survived", "Embarked", "Frequency")

ggplot(bivariate_survived_embarked_df, aes(x = Survived, y = Embarked, fill = Frequency)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(title = "Heatmap der Kontingenztafel: Überlebensrate nach Einsteigehafen",
       x = "Überlebensrate", y = "Einsteigehafen", fill = "Häufigkeit")

##...

####################

###Überlebensrate nach Passagierklasse und Geschlecht
visualize_categorical(data, "Survived", "Pclass", "Sex")

##...

####################

###Überlebensrate nach Passagierklasse und Einsteigehafen
visualize_categorical(data, "Survived", "Pclass", "Embarked")

##...

########################################

## (ii) Wie verhält sich der Ticketpreis gegen andere Variablen?
###Ticketpreis nach Überlebensrate
ggplot(data, aes(x = factor(Survived), y = Fare, fill = factor(Survived))) +
  geom_boxplot() +
  theme_minimal() +
  scale_y_log10() +
  labs(title = "Ticketpreis nach Überlebensrate", x = "Überlebt", y = "Ticketpreis (log)", fill = "Überlebt")

##...

####################

###Ticketpreis nach Passagierklasse
ggplot(data, aes(x = factor(Pclass), y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  theme_minimal() +
  scale_y_log10() +
  labs(title = "Ticketpreis nach Passagierklasse", x = "Passagierklasse", y = "Ticketpreis (log)", fill = "Klasse")

##...

####################

###Ticketpreis nach Einsteigehafen
ggplot(data, aes(x = factor(Embarked), y = Fare, fill = factor(Embarked))) +
  geom_boxplot() +
  theme_minimal() +
  scale_y_log10() +
  labs(title = "Ticketpreis nach Einsteigehafen", x = "Hafen", y = "Ticketpreis (log)", fill = "Hafen")

##...

###Ticketpreis nach Familiengröße
data$FamilySize <- data$SibSp + data$Parch
ggplot(data, aes(x = factor(FamilySize), fill = factor(Survived))) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Überlebensrate nach Familiengröße", x = "Familiengröße", y = "Relative Häufigkeit")

##...

########################################
##Weitere nützliche Analysen:
## Referenz: Aufgabe 2-a-iii, Aufgabe 2-a-iv & Aufgabe 2-a-v
###Scatterplot Alter nach Ticketpreis und Überlebensrate
ggplot(data, aes(x = Age, y = Fare, color = factor(Survived))) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  scale_y_log10() +
  labs(title = "Scatterplot: Alter nach Ticketpreis und Überlebensrate", x = "Alter", y = "Ticketpreis (log)", color = "Überlebt")

##...

####################

###Altersverteilung nach Geschlecht und Überlebensrate
ggplot(data, aes(x = Age, fill = factor(Survived))) +
  geom_histogram(binwidth = 5, alpha = 0.5, position = "identity") +
  facet_wrap(~ Sex) +
  theme_minimal() +
  labs(title = "Altersverteilung nach Geschlecht und Überlebensrate",
       x = "Alter", y = "Häufigkeit", fill = "Überlebt")

##...

####################

###Geschlecht nach Einsteigehafen
##Mosaikplot für die Abhängigkeit zwischen Geschlecht und Einsteigehafen
bivariate_sex_embarked <- bivariate_two_categorial(data, "Sex", "Embarked")

mosaicplot(bivariate_sex_embarked$Kontingenztafel, col = c("yellow", "orange", "red"), 
           main = "Mosaikplot: Geschlecht vs. Einsteigehafen", 
           xlab = "Geschlecht", ylab = "Einsteigehafen")

##...

##Proportionen von Männern und Frauen im Bezug auf Einsteigehafen
ggplot(data, aes(x = Embarked, fill = Sex)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Anteile von Männern & Frauen pro Einsteigehafen",
       x = "Einsteigehafen", y = "Proportion") +
  scale_fill_manual(values = c("male" = "blue", "female" = "pink"))

##...

##Heatmap für Geschlecht nach Einsteigehafen
bivariate_sex_embarked_df <- as.data.frame(bivariate_sex_embarked$Kontingenztafel)
colnames(bivariate_sex_embarked_df) <- c("Sex", "Embarked", "Frequency")

ggplot(bivariate_sex_embarked_df, aes(x = Embarked, y = Sex, fill = Frequency)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(title = "Heatmap der Kontingenztafel: Geschlecht vs. Einsteigehafen",
       x = "Einsteigehafen", y = "Geschlecht", fill = "Häufigkeit")

##...

####################

##Zusammenhang zwischen Titel, Einsteigehafen und Überlebensrate
visualize_categorical(data, "Title", "Embarked", "Survived")
##...

####################

##Zusammenhang zwischen Einsteigehafen, Titel, Geschlecht und Überlebensrate
visualize_categorical(data, "Embarked", "Title", "Sex", "Survived")
##...

####################

##Korrelationen zwischen metrischen und dichotomen Variablen
##Korrelation zwischen Alter und Geschlecht
punktbiseriale_korrelation(data$Age, data$Sex)
##...

##Korrelation zwischen Alter und Überlebensrate
punktbiseriale_korrelation(data$Age, data$Survived)
##...

##Korrelation zwischen Ticketpreis und Geschlecht
punktbiseriale_korrelation(data$Fare, data$Sex)
##...

##Korrelation zwischen Ticketpreis und Überlebensrate
punktbiseriale_korrelation(data$Fare, data$Survived)
##...









