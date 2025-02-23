library(ggplot2)
library(dplyr)
library(vcd)
library(reshape2)
library(GGally)
library(here)

data <- read.csv(here("data", "processed", "titanic_cleaned.csv"), 
                 header = TRUE, sep = ",", stringsAsFactors = FALSE)
source(here("scripts", "task2.R"))

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

# Dieses Histogramm visualisiert die Altersverteilung der Passagiere mit statistischen Kennwerten, 
# (Mittelwert: rot gestrichelt, Median: blau gepunktet, Modus, grüner Strich.)


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

# Dieser Boxplot zeigt die Verteilung des Alters, einschließlich der Quartile und der gesamten Spannweite.
# (Blau gestrichelt, 25% Quartil, Median und 75% Quartil, Schwarz durchgezogen: Minimum und Maximum)

##


##Dichtekurve für Schiefe & Kurtosis
ggplot(data, aes(x = Age)) +
  geom_density(fill = "blue", alpha = 0.3) +
  geom_vline(aes(xintercept = metric_age$ArithmetischesMittel, color = "Mittelwert"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = metric_age$Median, color = "Median"), linetype = "dotted", size = 1) +
  scale_color_manual(name = "Statistiken", values = c("Mittelwert" = "red", "Median" = "blue")) +
  theme_minimal() +
  labs(title = "Dichtekurve der Altersverteilung", x = "Alter", y = "Dichte")

# Die Dichtekurve zeigt die geschätzte Verteilung des Alters als glatte Funktion.
# Blaue Fläche: gibt Dichte an, wobei höhere Bereiche häufiger vorkommende Alterswerte darstellen.
# Die rote gestrichelte Linie repräsentiert den Mittelwert, die blau gepunktete Linie den Median .

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

# Das Histogramm zeigt die Verteilung der Ticketpreise in verschiedenen Preisklassen.
# Die Balkenhöhen geben an, wie viele Tickets in den jeweiligen Preisbereichen liegen.
# Die vertikalen Linien zeigt den Mittelwert (rot), den Median (blau) und den Modus (grün) für bessere Einordnung der Verteilung.

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

# Der Boxplot zeigt die Verteilung der Ticketpreise und identifiziert Ausreißer.
# Die blaue Box stellt den Interquartilsabstand (25%- bis 75%-Quartil) dar, die schwarze Linie ist die Spannweite.
# Der Median ist als blaue gestrichelte Linie eingezeichnet, rote Punkte kennzeichnen potenzielle Ausreißer.

##Dichtekurve für Schiefe & Kurtosis
ggplot(data, aes(x = Fare)) +
  geom_density(fill = "blue", alpha = 0.3) +
  geom_vline(aes(xintercept = metric_fare$ArithmetischesMittel, color = "Mittelwert"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = metric_fare$Median, color = "Median"), linetype = "dotted", size = 1) +
  scale_color_manual(name = "Statistiken", values = c("Mittelwert" = "red", "Median" = "blue")) +
  theme_minimal() +
  labs(title = "Dichtekurve der Ticketpreisverteilung", x = "Ticketpreis", y = "Dichte")

# Die Dichtekurve zeigt die Wahrscheinlichkeitsverteilung der Ticketpreise.
# Die Lage von Mittelwert und Median deutet auf eine mögliche Schiefe der Verteilung hin.
# Eine starke Schiefe oder hohe Kurtosis deutet auf Ausreißer oder eine ungewöhnliche Preisverteilung hin.

####################

##Paarweise Korrelationen der metrischen Variablen
numeric_vars <- data %>% select(Age, Fare, SibSp, Parch)
ggpairs(numeric_vars, title = "Paarweise Korrelationen der metrischen Variablen")

# Die paarweise Korrelationsmatrix zeigt Beziehungen zwischen den numerischen Variablen.
# Streudiagramme visualisieren die Zusammenhänge, während Korrelationskoeffizienten deren Stärke und Richtung angeben.
# Hohe Werte die nahe ±1 sind, weisen auf starke Korrelationen hin, während Werte nah an 0 auf keinen Zusammenhang hindeuten.

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

# Das Balkendiagramm visualisiert die Häufigkeit der Kategorien (Geschlecht, Passagierklasse, Einsteigehafen, Überlebensrate).
# Es zeigt, wie viele Passagiere in jeder Kategorie sind und unterteil dabei nach den verschiedenen Variablen.
# Die Facetten machen eine klare Darstellung der Häufigkeiten für jede Variable möglich.

#########################################################################################################################
##Analysen von Überlebensrate und Ticketpreis gegen andere Variablen
## (i) Wie verhält sich die Überlebensrate gegen andere Variablen?
##Referenz: Aufgabe 2-a-iii, Aufgabe 2-a-v
###Überlebensrate nach Geschlecht
visualize_categorical(data, "Sex", "Survived")

# Die Funktion erstellt eine Visualisierung, die die Überlebensrate nach Geschlecht zeigt.
# Diese Analyse ist dazu da, den Einfluss des Geschlechts auf die Überlebenswahrscheinlichkeit zu untersuchen.

##Mosaikplot Überlebensrate nach Geschlecht
bivariate_survived_sex <- bivariate_two_categorial(data, "Survived", "Sex")

mosaicplot(bivariate_survived_sex$Kontingenztafel, col = c("lightblue", "pink"), 
           main = "Mosaikplot: Überlebensrate nach Geschlecht", 
           xlab = "Geschlecht", ylab = "Überlebt")

# Ein Mosaikplot stellt die Häufigkeiten der Kombinationen der beiden Variablen "Überlebensrate" und "Geschlecht" dar.
# Der Plot zeigt visuell, wie sich die Überlebensrate je nach Geschlecht unterscheidet.

##Heatmap für Überlebensrate nach Geschlecht
bivariate_survived_sex_df <- as.data.frame(bivariate_survived_sex$Kontingenztafel)
colnames(bivariate_survived_sex_df) <- c("Survived", "Sex", "Frequency")

ggplot(bivariate_survived_sex_df, aes(x = Survived, y = Sex, fill = Frequency)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(title = "Heatmap der Kontingenztafel: Überlebensrate nach Geschlecht",
       x = "Überlebensrate", y = "Geschlecht", fill = "Häufigkeit")

# Eine Heatmap zeigt die Häufigkeiten der Überlebensraten für jedes Geschlecht.
# Die Farben der Zellen verdeutlichen, wie stark die jeweiligen Kombinationen vertreten sind.
# -> Je dunkler das Blau, desto höher ist die Häufigkeit.

####################

###Überlebensrate nach Passagierklasse
visualize_categorical(data, "Pclass", "Survived")

##Bei dieser Visualisierung wird die Überlebensrate nach Passagierklasse dargestellt, um signifikante Unterschiede darzustellen.

##Mosaikplot Überlebensrate nach Passagierklasse
bivariate_survived_Pclass <- bivariate_two_categorial(data, "Survived", "Pclass")

mosaicplot(bivariate_survived_Pclass$Kontingenztafel, col = c("yellow", "orange", "red"), 
           main = "Mosaikplot: Überlebensrate nach Klasse", 
           xlab = "Überlebt", ylab = "Klasse")

# Mosaikplot zur Visualisierung der Überlebensrate nach Passagierklasse
# Der Plot zeigt, wie sich die Überlebensrate (Überlebt/Nicht-Überlebt) in den verschiedenen Passagierklassen unterscheidet.
# Die Farben repräsentieren die Häufigkeit der Überlebenden und Nicht-Überlebenden in jeder Klasse.


##Heatmap für Überlebensrate nach Passagierklasse
bivariate_survived_Pclass_df <- as.data.frame(bivariate_survived_Pclass$Kontingenztafel)
colnames(bivariate_survived_Pclass_df) <- c("Survived", "Pclass", "Frequency")

ggplot(bivariate_survived_Pclass_df, aes(x = Survived, y = Pclass, fill = Frequency)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(title = "Heatmap der Kontingenztafel: Überlebensrate nach Passagierklasse",
       x = "Überlebensrate", y = "Geschlecht", fill = "Häufigkeit")

# Heatmap zur Visualisierung der Überlebensrate nach Passagierklasse
# Die Heatmap stellt die Häufigkeiten der Kombinationen von Überlebensstatus und Passagierklasse  dar.
# Farbverläufe von weiß bis blau zeigen die Frequenz der jeweiligen Kombination, wobei höhere Frequenzen intensiver in Blau dargestellt werden.


####################

###Überlebensrate nach Alter
ggplot(data, aes(x = factor(Survived), y = Age, fill = factor(Survived))) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Überlebensrate nach Alter", x = "Überlebt", y = "Alter", fill = "Überlebt")

# Boxplot zur Visualisierung der Überlebensrate im Vergleich zum Alter
# Der Boxplot zeigt, wie sich das Alter (y-Achse) für Überlebende (1) und Nicht-Überlebende (0) (x-Achse) unterscheidet.
# Überlebende und Nicht-Überlebende werden durch unterschiedliche Farben (fill) hervorgehoben.
# Der Boxplot macht es möglich, die Verteilung und Streuung des Alters in beiden Gruppen darzustellen.


####################

###Überlebensrate nach Anzahl der Geschwister/Ehepartner
visualize_categorical(data, "SibSp", "Survived")

# Visualisierung der Überlebensrate in Bezug auf die Anzahl der Geschwister/Ehepartner an Bord
# Der Funktionsaufruf visualisiert die Häufigkeit der Überlebensraten für verschiedene Werte von "Anzahl der Geschwister/Ehepartner".
# Es wird überprüft, wie sich die Überlebensrate im Verhältnis zur Anzahl der Geschwister/Ehe


####################

###Überlebensrate nach Anzahl der Eltern/Kinder
visualize_categorical(data, "Parch", "Survived")

# Visualisierung der Überlebensrate in Bezug auf die Anzahl der Eltern/Kinder an Bord
# Der Funktionsaufruf visualisiert die Häufigkeit der Überlebensraten für verschiedene Werte von "Anzahl der Eltern/Kinder".
# Ist dazu da um zu untersuchen,wie sich die Überlebensrate je nach Anzahl der Eltern/Kinder unterscheidet.


####################

###Überlebensrate nach Einsteigehafen
visualize_categorical(data, "Embarked", "Survived")

# Visualisierung der Überlebensrate in Bezug auf den Einsteigehafen
# Der Funktionsaufruf visualisiert die Häufigkeit der Überlebensraten für verschiedene Werte von "Einsteigehafen".
# Es wird untersucht, ob der Einsteigehafen einen Einfluss auf die Überlebensrate hatte.


##Mosaikplot Überlebensrate nach Einsteigehafen
bivariate_survived_embarked <- bivariate_two_categorial(data, "Survived", "Embarked")

mosaicplot(bivariate_survived_embarked$Kontingenztafel, col = c("yellow", "orange", "red"), 
           main = "Mosaikplot: Überlebensrate nach Einsteigehafen", 
           xlab = "Überlebt", ylab = "Einsteigehafen")

# Mosaikplot zur Visualisierung der Überlebensrate nach Einsteigehafen
# Der Mosaikplot zeigt die Häufigkeit der Überlebensraten für verschiedene Einsteigehäfen.
# Es wird untersucht, ob die Überlebensrate je nach Einsteigehafen unterschiedlich ist.


##Heatmap für Überlebensrate nach Einsteigehafen
bivariate_survived_embarked_df <- as.data.frame(bivariate_survived_Pclass$Kontingenztafel)
colnames(bivariate_survived_embarked_df) <- c("Survived", "Embarked", "Frequency")

ggplot(bivariate_survived_embarked_df, aes(x = Survived, y = Embarked, fill = Frequency)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(title = "Heatmap der Kontingenztafel: Überlebensrate nach Einsteigehafen",
       x = "Überlebensrate", y = "Einsteigehafen", fill = "Häufigkeit")

### Heatmap zur Visualisierung der Überlebensrate nach Einsteigehafen
# Die Heatmap zeigt die Häufigkeit der Überlebensraten in Bezug auf den Einsteigehafen.
# Diese gibt Auskunft darüber, ob der Einsteigehafen einen Einfluss auf die Überlebenswahrscheinlichkeit hatte.


####################

###Überlebensrate nach Passagierklasse und Geschlecht
visualize_categorical(data, "Survived", "Pclass", "Sex")

# Visualisierung der Überlebensrate nach Passagierklasse und Geschlecht
# Diese Funktion zeigt die Verteilung der Überlebensraten je nach Passagierklasse und Geschlecht.
# Es wird überprüft, ob das Überleben mit der Passagierklasse und dem Geschlecht in Verbindung steht.


####################

###Überlebensrate nach Passagierklasse und Einsteigehafen
visualize_categorical(data, "Survived", "Pclass", "Embarked")

# Visualisierung der Überlebensrate nach Passagierklasse und Einsteigehafen
# Diese Funktion zeigt, wie sich die Überlebensraten für verschiedene Kombinationen von Passagierklasse und Einsteigehafen verteilen.
# Es wird überprüft, ob es Unterschiede in der Überlebensrate zwischen den verschiedenen Kombinationen gibt.


########################################

## (ii) Wie verhält sich der Ticketpreis gegen andere Variablen?
###Ticketpreis nach Überlebensrate
ggplot(data, aes(x = factor(Survived), y = Fare, fill = factor(Survived))) +
  geom_boxplot() +
  theme_minimal() +
  scale_y_log10() +
  labs(title = "Ticketpreis nach Überlebensrate", x = "Überlebt", y = "Ticketpreis (log)", fill = "Überlebt")

# Boxplot der Ticketpreisverteilung nach Überlebensrate
# Der Ticketpreis wird auf der y-Achse dargestellt, wobei die Überlebensrate als Gruppierung (x-Achse) verwendet wird.
# Es wird eine logarithmische Skala für den Ticketpreis angewendet, um die Preisverteilung besser darzustellen.


####################

###Ticketpreis nach Passagierklasse
ggplot(data, aes(x = factor(Pclass), y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  theme_minimal() +
  scale_y_log10() +
  labs(title = "Ticketpreis nach Passagierklasse", x = "Passagierklasse", y = "Ticketpreis (log)", fill = "Klasse")

# Boxplot der Ticketpreisverteilung nach Passagierklasse
# Der Ticketpreis wird auf der y-Achse dargestellt, während die Passagierklasse als Gruppierung (x-Achse) verwendet wird.
# Eine logarithmische Skala wird angewendet, um die Preisverteilung besser zu visualisieren.


####################

###Ticketpreis nach Einsteigehafen
ggplot(data, aes(x = factor(Embarked), y = Fare, fill = factor(Embarked))) +
  geom_boxplot() +
  theme_minimal() +
  scale_y_log10() +
  labs(title = "Ticketpreis nach Einsteigehafen", x = "Hafen", y = "Ticketpreis (log)", fill = "Hafen")

# Boxplot der Ticketpreisverteilung nach Einsteigehafen
# Der Ticketpreis wird auf der y-Achse dargestellt, während die Einsteigehafen als Gruppierung (x-Achse) verwendet wird.
# Eine logarithmische Skala wird angewendet, um die Preisverteilung besser zu visualisieren.


###Ticketpreis nach Familiengröße
data$FamilySize <- data$SibSp + data$Parch
ggplot(data, aes(x = factor(FamilySize), fill = factor(Survived))) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Überlebensrate nach Familiengröße", x = "Familiengröße", y = "Relative Häufigkeit")

# Balkendiagramm der Überlebensrate nach Familiengröße
# Die Familiengröße wird auf der x-Achse angezeig.
# Die relative Häufigkeit der Überlebensraten wird für jede Familiengröße als gestapeltes Balkendiagramm dargestellt wird.


########################################
##Weitere nützliche Analysen:
## Referenz: Aufgabe 2-a-iii, Aufgabe 2-a-iv & Aufgabe 2-a-v
###Scatterplot Alter nach Ticketpreis und Überlebensrate
ggplot(data, aes(x = Age, y = Fare, color = factor(Survived))) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  scale_y_log10() +
  labs(title = "Scatterplot: Alter nach Ticketpreis und Überlebensrate", x = "Alter", y = "Ticketpreis (log)", color = "Überlebt")

# Scatterplot, der das Alter und den Ticketpreis mit der Überlebensrate kombiniert
# Die Punkte im Diagramm repräsentieren die Passagiere. Der Ticketpreis wird auf der y-Achse und das Alter auf der x-Achse angezeigt.
# Die Farbe der Punkte zeigt die Überlebensrate, wobei ein transparenter Punkt (alpha = 0.5) verwendet wird, um Überlappungen zu minimieren.


####################

###Altersverteilung nach Geschlecht und Überlebensrate
ggplot(data, aes(x = Age, fill = factor(Survived))) +
  geom_histogram(binwidth = 5, alpha = 0.5, position = "identity") +
  facet_wrap(~ Sex) +
  theme_minimal() +
  labs(title = "Altersverteilung nach Geschlecht und Überlebensrate",
       x = "Alter", y = "Häufigkeit", fill = "Überlebt")

# Das Histogramm zeigt die Verteilung des Alters der Passagiere, wobei die Überlebensrate durch unterschiedliche Farben dargestellt wird.
# Die Facetten unterteilen die Darstellung nach Geschlecht, sodass die Verteilung für Männer und Frauen separat angezeigt wird.
# Die Transparenz der Balken (alpha = 0.5) wird verwendet, um Überlappungen der Balken zwischen den Überlebensgruppen zu reduzieren.


####################

###Geschlecht nach Einsteigehafen
##Mosaikplot für die Abhängigkeit zwischen Geschlecht und Einsteigehafen
bivariate_sex_embarked <- bivariate_two_categorial(data, "Sex", "Embarked")

mosaicplot(bivariate_sex_embarked$Kontingenztafel, col = c("yellow", "orange", "red"), 
           main = "Mosaikplot: Geschlecht vs. Einsteigehafen", 
           xlab = "Geschlecht", ylab = "Einsteigehafen")

# Der Mosaikplot visualisiert die Häufigkeiten der Geschlechter in Bezug auf den Einsteigehafen.
# Jede Zelle im Plot stellt eine Kombination von Geschlecht und Einsteigehafen dar.
# Farben werden verwendet, um die Häufigkeit der jeweiligen Kombinationen zu unterscheiden, wobei rot für eine höhere Häufigkeit steht.


##Proportionen von Männern und Frauen im Bezug auf Einsteigehafen
ggplot(data, aes(x = Embarked, fill = Sex)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Anteile von Männern & Frauen pro Einsteigehafen",
       x = "Einsteigehafen", y = "Proportion") +
  scale_fill_manual(values = c("male" = "blue", "female" = "pink"))

# Das Balkendiagramm zeigt die prozentualen Anteile von Männern und Frauen in den verschiedenen Einsteigehäfen.
# Mit der "position = 'fill'" Option wird jeder Balken auf 100% normalisiert, um die Verhältnisse innerhalb der Einsteigehäfen zu verdeutlichen.
# Die Farben blau und pink sind für Männer bzw. Frauen gewählt, um die Kategorien zu unterscheiden.


##Heatmap für Geschlecht nach Einsteigehafen
bivariate_sex_embarked_df <- as.data.frame(bivariate_sex_embarked$Kontingenztafel)
colnames(bivariate_sex_embarked_df) <- c("Sex", "Embarked", "Frequency")

ggplot(bivariate_sex_embarked_df, aes(x = Embarked, y = Sex, fill = Frequency)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(title = "Heatmap der Kontingenztafel: Geschlecht vs. Einsteigehafen",
       x = "Einsteigehafen", y = "Geschlecht", fill = "Häufigkeit")

# Die Heatmap zeigt die Häufigkeit der Passagiere in Bezug auf ihr Geschlecht und den Einsteigehafen. 
# Die Farbskala geht von Weiß (niedrige Häufigkeit) bis Blau (hohe Häufigkeit).
#Anhand der Visualisierung können wir sehen, wie sich die Verteilung von Männern und Frauen je nach Einsteigehafen unterscheidet.

####################

##Zusammenhang zwischen Titel, Einsteigehafen und Überlebensrate
visualize_categorical(data, "Title", "Embarked", "Survived")
# Zeigt, wie Titel, Einsteigehafen und Überlebensrate miteinander in Beziehung stehen.

####################

##Zusammenhang zwischen Einsteigehafen, Titel, Geschlecht und Überlebensrate
visualize_categorical(data, "Embarked", "Title", "Sex", "Survived")
# Zeigt, wie Titel, Einsteigehafen Geschlecht und Überlebensrate in Beziehung zueinander stehen.

####################

##Korrelationen zwischen metrischen und dichotomen Variablen
##Korrelation zwischen Alter und Geschlecht
punktbiseriale_korrelation(data$Age, data$Sex)
# Zeigt, wie Alter und Geschlecht in Beziehung zu einander stehen.

##Korrelation zwischen Alter und Überlebensrate
punktbiseriale_korrelation(data$Age, data$Survived)
# Zeigt, wie Alter und Überlebensrate in Beziehung zu einander stehen.

##Korrelation zwischen Ticketpreis und Geschlecht
punktbiseriale_korrelation(data$Fare, data$Sex)
# Zeigt, wie Ticketpreis und Geschlecht in Beziehung zu einander stehen.

##Korrelation zwischen Ticketpreis und Überlebensrate
punktbiseriale_korrelation(data$Fare, data$Survived)
# Zeigt, wie Ticketpreis und Überlebensrate in Beziehung zu einander stehen.









