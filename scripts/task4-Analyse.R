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
## Das Alter der Besatzung kann folgendermaßen beschrieben werden:
## Im Mittel beträgt das Alter der Besatzung 29.65 Jahre.
## Das Alter im Median liegt bei 30 Jahren.
## Das Alter im Modus liegt bei 32 Jahren mit 137 Personen.
## Die Altersspanne reicht von 0.42 bis 80 Jahren.


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
## Der IQR konzentriert sich auf 14 Jahre (21-35 Jahre). 
## Die Altersspanne reicht von 0.42 bis 80 Jahren.
## Das Alter im Median liegt bei 30 Jahren.
## Ab 57 Jahren ist man als Ausreißer im Boxplot sichtbar.


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
# Die rote gestrichelte Linie repräsentiert den Mittelwert, die blau gepunktete Linie den Median.
## Die Schiefe liegt bei ca. 0.37 (leichte Rechtssteilung).
## Die Kurtosis liegt bei ca. 0.7 (leichte Abflachung).
## Im Mittel beträgt das Alter der Besatzung 29.65 Jahre.
## Das Alter im Median liegt bei 30 Jahren.

## Die Analyse der Altersdaten zeigt, dass der Großteil der beobachteten Werte 
## um die 30 Jahre liegt, wobei 50 % der Daten zwischen 21 und 35 Jahren konzentriert 
## sind. Die Verteilung ist nahezu symmetrisch mit einer leichten Rechtssteilung, 
## was darauf hindeutet, dass einige höhere Alterswerte vorhanden sind, die den 
## Durchschnitt ein wenig nach oben ziehen. Insgesamt spiegelt der Datensatz eine 
## relativ junge Population wider, jedoch mit einer breiten Streuung, die von sehr 
## jungen (nahe 0,42 Jahre) bis zu hohen Werten (80 Jahre) reicht.

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
## Die Ticketpreise können folgendermaßen beschrieben werden:
## Im Mittel beträgt der Ticketpreis 32.20$.
## Der Ticketpreis im Median liegt bei 14.45$.
## Der Ticketpreis im Modus liegt bei 8.05$ mit 43 Mal.
## Die Ticketpreisspanne reicht von 0$ bis 512.33$.

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
## Der IQR konzentriert sich auf 23.09$ (7.91$-31$). 
## Die Ticketpreisspanne reicht von 0$ bis 512.33$.
## Der Ticketpreis im Median liegt bei 14.45$.
## Ab ca. 60$ ist man als Ausreißer im Boxplot sichtbar.

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
## Die Schiefe liegt bei ca. 4.77 (starke Rechtssteilung).
## Die Kurtosis liegt bei ca. 33.12 (sehr leptokurtisch).
## Im Mittel beträgt das Alter der Besatzung 29.65 Jahre.
## Das Alter im Median liegt bei 30 Jahren.

## Die Analyse der Ticketpreise zeigt, dass der Großteil der Daten im niedrigen 
## Preissegment liegt (Median ca. 14,45 und Modus 8,05), jedoch einige wenige 
## sehr hohe Preise den Durchschnitt (ca. 32,20) in die Höhe treiben. Die sehr 
## hohe Schiefe und Kurtosis unterstreichen, dass es sich um eine stark rechtssteil 
## verteilte Datenmenge mit ausgeprägten Ausreißern handelt. Insgesamt spiegelt 
## der Datensatz eine breite Preisspanne wider, bei der die meisten Tickets günstig 
## sind, während einige extreme Werte die statistischen Kennzahlen maßgeblich beeinflussen.

####################

##Paarweise Korrelationen der metrischen Variablen
numeric_vars <- data %>% select(Age, Fare, SibSp, Parch)
ggpairs(numeric_vars, title = "Paarweise Korrelationen der metrischen Variablen")

# Die paarweise Korrelationsmatrix zeigt Beziehungen zwischen den numerischen Variablen.
# Streudiagramme visualisieren die Zusammenhänge, während Korrelationskoeffizienten deren Stärke und Richtung angeben.
# Hohe Werte die nahe ±1 sind, weisen auf starke Korrelationen hin, während Werte nah an 0 auf keinen Zusammenhang hindeuten.
## Alter (Age) und Ticketpreis (Fare):
## Korrelation: 0.092 (p < 0.01)
## Der Zusammenhang ist sehr schwach positiv. Das bedeutet, dass ältere Passagiere 
## tendenziell etwas teurere Tickets kauften, aber der Effekt ist nicht stark ausgeprägt.
## Alter (Age) und Anzahl Geschwister/Ehepartner (SibSp):
##   Korrelation: -0.269 (p < 0.001)
## Ein moderat negativer Zusammenhang. Jüngere Passagiere hatten tendenziell mehr 
## Geschwister oder Ehepartner an Bord, während ältere Passagiere meist alleine reisten.
## Alter (Age) und Anzahl Eltern/Kinder (Parch):
##   Korrelation: -0.195 (p < 0.001)
## Ebenfalls eine negative Korrelation. Jüngere Passagiere reisten häufiger mit 
## ihren Eltern oder Kindern als ältere Passagiere.
## Ticketpreis (Fare) und Anzahl Geschwister/Ehepartner (SibSp):
##   Korrelation: 0.160 (p < 0.001)
## Ein schwach positiver Zusammenhang. Passagiere mit mehr Geschwistern oder 
## Ehepartnern an Bord hatten im Durchschnitt teurere Tickets.
## Ticketpreis (Fare) und Anzahl Eltern/Kinder (Parch):
##   Korrelation: 0.216 (p < 0.001)
## Ebenfalls ein schwacher positiver Zusammenhang. Familien mit mehr Kindern oder 
## Eltern an Bord haben tendenziell mehr für Tickets bezahlt.
## Anzahl Geschwister/Ehepartner (SibSp) und Anzahl Eltern/Kinder (Parch):
##   Korrelation: 0.415 (p < 0.001)
## Dieser Zusammenhang ist der stärkste in der Matrix. Wer mit vielen Geschwistern 
## oder einem Ehepartner an Bord war, reiste auch häufig mit Eltern oder Kindern.
## Analyse:
## Die stärkste Korrelation besteht zwischen SibSp und Parch: Familien reisten oft zusammen.
## Jüngere Passagiere reisten häufiger mit Verwandten, während ältere allein waren.
## Höhere Ticketpreise wurden leicht mit mehr Begleitpersonen in Verbindung gebracht.

########################################

##Referenz: Aufgabe 2-a-ii
##Visualisation von Geschlecht, Passagierklasse, Einsteigehafen & Überlebensrate
categorical_stats <- calculate_categorical_stats(data, c("Sex", "Pclass", "Embarked", "Survived"))

##Balkendiagramm für Häufigkeiten
ggplot(categorical_stats, aes(x = Category, y = Frequency, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Häufigkeit der Kategorien", x = "Kategorie", y = "Anzahl") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")

# Das Balkendiagramm visualisiert die Häufigkeit der Kategorien (Geschlecht, Passagierklasse, Einsteigehafen, Überlebensrate).
# Es zeigt, wie viele Passagiere in jeder Kategorie sind und unterteilt dabei nach den verschiedenen Variablen.
# Die Facetten machen eine klare Darstellung der Häufigkeiten für jede Variable möglich.
## Embarked:
## Die meisten Personen sind mit 644 (ca. 72.4%) in Southampton eingestiegen
## Es folgt Cherbourg mit 168 (ca. 18.9%) und Queenstown mit 77 Personen (ca. 8.7%)
## Pclass:
## Die meisten Personen belegten die dritte Klasse mit 491 Personen (ca. 55.1%).
## Es folgt die erste Klasse mit 216 Personen (ca. 24.2%) und die zweite Klasse
## mit 184 Personen (ca. 20.7%)
## Sex:
## Männer waren mit 577 Mann (ca. 64.8%) präsenter an Bord. Frauen machten einen 
## Anteil von ca. 35.2% aus, was 314 Frauen sind.
## Survived: 
## Der größere Teil der Besatzung hat nicht überlebt. Mit 549 Todesfällen (ca. 61.6%)
## blieben 342 Personen (ca. 38.4%), welche überlebt haben.

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

## Analyse:
## Männer machen mit über 450 Personen den größten Anteil der Todesfälle aus. 
## Es überlebten bei Männern nur knapp über 100 Personen.
## Frauen überlebten mit ca. 230 Personen deutlich besser, da auch nur knapp 80 
## Frauen starben. Das liegt wahrscheinlich daran, dass bei Rettungsarbeiten
## Frauen bevorzugt wurden.

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

## Analyse:
## Man sieht, dass die dritte Klasse die meisten Todesfälle mit ca. 370 Personen 
## hatte und somit ein Großteil der dritten Klasse starb, da nur ca. 120 Personen
## überlebten. Bei der zweiten Klasse ist die Überlebensrate ausgewogen. Es starben
## an die 100 Personen und es überlebten ca. 90 Personen. Bei der ersten Klasse
## überlebten (mit ca. 140) mehr Personen als Personen (mit ca. 80) starben. 
## Das spiegelt die Sicherheit der Klassen gut wieder, da die Überlebensquote 
## mit der steigenden Klasse auch steigt.

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

## Analyse:
##Die Altersverteilung zwischen Überlebenden und Nicht-Überlebenden ist sehr ähnlich.
## Median:
## Der Median ist für Überlebende etwas höher als für Nicht-Überlebende.
## Dies bedeutet, dass ältere Personen leicht höhere Überlebenschancen hatten.
## Ausreißer:
## In beiden Gruppen gibt es ältere Personen (60–80 Jahre).
## Mehr junge Kinder in der Überlebensgruppe, was auf die bekannte Regel "Kinder zuerst" hindeutet.
## Kinder und Überleben:
## In der „Yes“-Gruppe gibt es mehr kleine Kinder, was bestätigt, dass Kinder bevorzugt gerettet wurden.

## Hypothese:
## Das Alter allein war kein dominanter Faktor für das Überleben.
## Kinder hatten eine höhere Überlebenswahrscheinlichkeit.

####################

###Überlebensrate nach Anzahl der Geschwister/Ehepartner
visualize_categorical(data, "SibSp", "Survived")

# Visualisierung der Überlebensrate in Bezug auf die Anzahl der Geschwister/Ehepartner an Bord
# Der Funktionsaufruf visualisiert die Häufigkeit der Überlebensraten für verschiedene Werte von "Anzahl der Geschwister/Ehepartner".
# Es wird überprüft, wie sich die Überlebensrate im Verhältnis zur Anzahl der Geschwister/Ehe

## Analyse:
## Man kann beobachten, wie sich durch die Anzahl der Geschwister/Ehepartner an 
## Bord die Überlebensrate ändert. An die 400 Personen starben, welche keine 
## Geschwister/Ehepartner an Bord hatten, gegenüber den ca. 200 Personen, welche 
## überlebten. Bei einer weiteren Person (Geschwister/Ehepartner) an Bord haben
## mehr Personen überlebt (mit ca. 115) als gestorben (mit ca. 95) sind.
## Bei zwei weiteren Personen (Geschwister/Ehepartner) haben ca. gleich viele Personen
## überlebt und nicht überlebt (mit jeeweils ca. 30). Ab drei weiteren Personen
## (Geschwistern/Ehepartnern) ist die Überlebensrate gering. 

## Hypothese:
## Die Beobachtungen lassen sich vielleicht dadurch erklären, dass 
## man in Gruppen von 2 bis 3 Personen die besten kooperativen Mittel hatte, um 
## sich am Leben zu halten.


####################

###Überlebensrate nach Anzahl der Eltern/Kinder
visualize_categorical(data, "Parch", "Survived")

# Visualisierung der Überlebensrate in Bezug auf die Anzahl der Eltern/Kinder an Bord
# Der Funktionsaufruf visualisiert die Häufigkeit der Überlebensraten für verschiedene Werte von "Anzahl der Eltern/Kinder".
# Ist dazu da um zu untersuchen,wie sich die Überlebensrate je nach Anzahl der Eltern/Kinder unterscheidet.

## Analyse:
## Ähnlich wie bei der Anzahl der Geschwister/Ehepartner an Bord ist auch die
## Anzahl der Eltern/Kinder aufgebaut. 

## Hypothese:
## Auch hier lassen sich die Beobachtungen vielleicht dadurch erklären, 
## dass man in Gruppen von 2 bis 3 Personen die besten kooperativen Mittel hatte, 
## um sich am Leben zu halten.

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

## Analyse:
## Die meisten Mesnchen sind in Southampton eingestiegen und auch gestorben mit 
## über 400 Personen. ca. 220 Personen haben aus Southampton aber überlebt. 
## Aus Queenstown sind ca. 50 Menschen gestorben und ca. 25 haben überlebt.
## Aus Cherbourg haben mit ca. 95 Personen mehr überlebt, als mit ca. 75 Personen 
## gestorben sind. 

## Hypothese: 
## In Southampton sind wahrscheinlich viele Personen eingestiegen, 
## welche die dritte Klasse belegten. Wahrscheinlich also eher ärmere Leute und/oder
## Arbeiter des Bordes. In Cherbourg/Queenstown hingegen sind wahrscheinlich eher 
## wohlhabende und reiche Leute eingestiegen, welche sich die erste/zweite Klasse 
## leisten konnten. 

####################

###Überlebensrate nach Passagierklasse und Geschlecht
visualize_categorical(data, "Survived", "Pclass", "Sex")

# Visualisierung der Überlebensrate nach Passagierklasse und Geschlecht
# Diese Funktion zeigt die Verteilung der Überlebensraten je nach Passagierklasse und Geschlecht.
# Es wird überprüft, ob das Überleben mit der Passagierklasse und dem Geschlecht in Verbindung steht.

## Analyse:
## Hier sieht man, dass die meisten Männer, welche die dritte Klasse belegten, 
## gestorben sind. nur an die 50 Männer der dritten Klasse haben überlebt.
## Auch in der zweiten und ersten Klasse sieht es ähnlich aus, jedoch klafft die 
## Schere zur ersten Klasse hin zusammen, sodass der größte Anteil der überlebenden
## Männer aus der ersten Klasse stammt. 
## Bei den Frauen sieht man eine höhere Überlebensquote. In jeder Klasse überlebten 
## ca. gleich viele Frauen, jedoch staben bei der dritten Klasse auch ca. so viele
## Frauen, wie Frauen gestorben sind und in der zweiten sowie ersten Klasse
## fast keine Frau gestorben ist. 

## Hypothese: 
## Frauen wurden vor allem in der ersten und zweiten Klasse eher beschützt
## und bei Rettungsarbeiten bevorzugt. Männer hingegen waren vor allem in der 
## dritten Klasse sehr dem Unglück ausgesetzt, da diese wahrscheinlich am Arbeiten 
## und/oder in der dritten Klasse von dem geringsten Schutz umgeben waren.

####################

###Überlebensrate nach Passagierklasse und Einsteigehafen
visualize_categorical(data, "Survived", "Pclass", "Embarked")

# Visualisierung der Überlebensrate nach Passagierklasse und Einsteigehafen
# Diese Funktion zeigt, wie sich die Überlebensraten für verschiedene Kombinationen von Passagierklasse und Einsteigehafen verteilen.
# Es wird überprüft, ob es Unterschiede in der Überlebensrate zwischen den verschiedenen Kombinationen gibt.

## Analyse:
## Cherbourg
## Ein relativ hoher Anteil der Passagiere kam aus der Ersten Klasse.
## Viele dieser Ersten-Klasse-Passagiere überlebten.
## Dritte-Klasse-Passagiere sind vorhanden, aber nicht in großer Zahl.

## Queenstown
## Die meisten Passagiere, die hier einstiegen, waren in der Dritten Klasse.
## Die Mehrheit von ihnen überlebte nicht.
## Es gab nur sehr wenige Passagiere aus der Ersten und Zweiten Klasse.

## Southampton
## Der größte Teil der Passagiere kam aus Southampton, insbesondere aus der Dritten Klasse.
## Viele Dritte-Klasse-Passagiere überlebten nicht.
## Es gibt auch eine nennenswerte Anzahl an Erster-Klasse-Passagieren, die häufiger überlebten.

## Hypothese: 
## Die Überlebenschancen waren stark von der Klasse abhängig:
## Erste-Klasse-Passagiere hatten eine viel höhere Überlebensrate als Zweite- 
## und Dritte-Klasse-Passagiere.
## Besonders gut sichtbar in Cherbourg und Southampton, wo Erste-Klasse-Passagiere 
## häufiger überlebten.
## Dritte-Klasse-Passagiere (blau) hatten die schlechtesten Überlebenschancen, 
## insbesondere wenn sie aus Queenstown und Southampton kamen.
## Southampton hatte die größte Anzahl an Dritte-Klasse-Passagieren, was zeigt, 
## dass viele ärmere Passagiere dort einstiegen.

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

## Analyse:
## Man kann beobachten, dass der Ticketpreis die Überlebensrate beeinflusst hat.
## Die Box ist bei den gestorbenen Personen weiter unten anzusiedeln als die
## Box der Überlebten. Die Quartile sind bei den nicht Überlebten auch weiter unten 
## anzusiedeln, wobei das 0.25-Quartil bei ca. 7.50$, der Median bei ca. 10$ und 
## das 0.75-Quartil bei ca. 30$ liegt. Bei den Überlebten liegt das 0.25-Quartil bei
## ca. 12$, der Median bei ca. 30$ und das 0.75-Quartil bei ca. 60$. Auch die 
## Spannweite der Boxplots ist bei den Überlebten höher anzusiedeln als die 
## Spannweite der nicht Überlebten. Bei den nicht Überlebten gibt es außerdem 
## Ausreißer, welche trotz eines hohen Ticketpreises nicht überlebt haben.

## Hypothese:
## Der Ticketpreis beeinflusst die Überlebensrate, da man durch den
## Ticketpreis zB die Passagierklasse beeinflusst, welche mit Erhöhung auch 
## eher an Stellen im Schiff platziert sind, welche besser geschützt sind. Außerdem
## wird man wahrscheinlich auch von Menschen anders behandelt, wenn man mehr bezahlt
## hat, da man so Prestige zeigt.

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

## Analyse: 
## Hier sieht man eine offensichtliche Statistik, welche den steigenden Ticketpreis
## mit der steigenden Passagierklasse visualisiert. Hier sind vor allem die Ausreißer
## spannend, welche zB für die dritte Klasse überdurchschnlttlich viel bezahlt haben
## oder für die erste Klasse unterdurchschnittlich wenig gezahlt haben. 

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

## Analyse:
## Cherbourg:
## Die Passagiere, die in Cherbourg eingestiegen sind, zahlten im Durchschnitt die höchsten Ticketpreise.
## Der Medianpreis ist deutlich höher als in Queenstown und Southampton.
## Es gibt einige extreme Ausreißer mit sehr hohen Ticketpreisen.
## Queenstown:
## Die Ticketpreise sind am niedrigsten.
## Der Median ist relativ klein und es gibt eine geringere Streuung der Preise.
## Wenige Ausreißer deuten darauf hin, dass es hier weniger extrem teure Tickets gab.
## Southampton:
## Die Preise variieren stark.
## Der Medianpreis liegt über dem von Queenstown, aber unter dem von Cherbourg.
## Es gibt einige sehr teure Tickets (Ausreißer), aber die meisten Preise konzentrieren sich auf einen niedrigeren Bereich.

## Hypothesen: 
## Cherbourg hatte viele Passagiere der Ersten Klasse, da die Ticketpreise hier 
## im Durchschnitt am höchsten sind.
## Queenstown war vermutlich der Hauptabfahrtsort für Dritte-Klasse-Passagiere, 
## da die Preise dort am niedrigsten sind.
## Southampton war ein Mischhafen mit einer breiten Preisstreuung, was darauf 
## hindeutet, dass dort Passagiere aus verschiedenen Klassen einstiegen. Jedoch 
## Sind auch hier vor allem Dritte-Klasse-Passagiere eingestiegen.

###Ticketpreis nach Familiengröße
data$FamilySize <- data$SibSp + data$Parch
ggplot(data, aes(x = factor(FamilySize), fill = factor(Survived))) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Überlebensrate nach Familiengröße", x = "Familiengröße", y = "Relative Häufigkeit")

# Balkendiagramm der Überlebensrate nach Familiengröße
# Die Familiengröße wird auf der x-Achse angezeig.
# Die relative Häufigkeit der Überlebensraten wird für jede Familiengröße als gestapeltes Balkendiagramm dargestellt wird.

## Analyse:
## Alleinreisende (Familiengröße = 0)
## Sehr niedrige Überlebensrate → Die meisten Alleinreisenden starben.
## Hypothese: 
## Dies könnte daran liegen, dass sie weniger Unterstützung oder schlechtere Chancen auf Rettung hatten.

## Kleine Familien (Familiengröße = 2-4)
## Die höchste Überlebensrate zeigt sich bei Familien mit 2 bis 4 Mitgliedern.
## Hypothese:
## Dies könnte daran liegen, dass Familien gemeinsam nach Rettung suchten und sich gegenseitig halfen.

## Größere Familien (Familiengröße > 4)
## Die Überlebensrate nimmt deutlich ab, je größer die Familie ist.
## Familien mit 5 oder mehr Mitgliedern hatten eine sehr geringe Überlebenschance.
## Hypothese:
## Dies könnte daran liegen, dass es schwieriger war, für alle Familienmitglieder 
## Plätze auf Rettungsbooten zu bekommen.

## Sehr große Familien (7-10 Mitglieder)
## Nahezu 100 % der Passagiere mit sehr großen Familien starben.
## Hypothese:
## Vermutlich hatten sie sehr geringe Chancen, alle zusammen gerettet zu werden.

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

## Analyse: 
## Zusammenhang zwischen Ticketpreis und Überlebenschance
## Passagiere mit teuren Tickets hatten eine höhere Überlebensrate.
## Viele blaue Punkte (Überlebende) sind bei höheren Ticketpreisen (über 50) zu sehen.
## Dies deutet darauf hin, dass Passagiere der Ersten Klasse eine bessere Überlebenschance hatten.
## Passagiere mit günstigen Tickets hatten eine niedrigere Überlebensrate.
## In den unteren Preisbereichen (10 oder niedriger) gibt es viele rote Punkte (Nicht-Überlebende).
## Dies zeigt, dass Passagiere der Dritten Klasse schlechtere Überlebenschancen hatten.

## Zusammenhang zwischen Alter und Überlebenschance
## Kinder (unter 10 Jahre) haben eine höhere Überlebensrate.
## Mehr blaue Punkte in der Gruppe der sehr jungen Passagiere.
## Dies könnte darauf hindeuten, dass Kinder bevorzugt gerettet wurden (Hypothese).
## Bei Erwachsenen gibt es keinen starken Altersunterschied.
## Die Überlebenschancen scheinen relativ gleichmäßig verteilt zu sein.
## Sowohl junge als auch ältere Erwachsene finden sich in beiden Gruppen (Überlebt / Nicht überlebt).


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

## Analyse:
## Frauen
## Höhere Überlebensrate:
## Es gibt mehr blaue Balken als rote → mehr Frauen überlebten als starben.
## Verteilung über Altersgruppen:
## Viele jüngere Frauen (20–40 Jahre) überlebten.
## Auch ältere Frauen überlebten häufiger, wenn auch in geringeren Zahlen.
## Kinder (unter 10 Jahren) hatten eine hohe Überlebensrate.
## Schlussfolgerung:
## Die Regel "Frauen und Kinder zuerst" hatte einen starken Einfluss auf die 
## Überlebenschancen weiblicher Passagiere.

## Männer
## Niedrigere Überlebensrate:
## Der rote Bereich dominiert → mehr Männer starben als überlebten.
## Häufigkeitsspitze bei jungen Erwachsenen (20–40 Jahre):
## In dieser Gruppe gibt es besonders viele rote Balken, was darauf hindeutet, 
## dass viele junge Männer nicht überlebten.
## Kinder und ältere Männer:
## Wenige Kinder überlebten, aber auch hier zeigt sich eine leichte Tendenz, 
## dass jüngere Passagiere bessere Chancen hatten.
## Ältere Männer hatten fast keine Überlebenschance.


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

## Analyse:
## Cherbourg: Gleich viele Männer und Frauen -> wohlhabende Passagiere
## Queenstown: Mehr Männer als Frauen -> Dritte-Klasse-Passagiere und Arbeiter
## Southampton: Mehr Männer als Frauen -> Dritte-Klasse-Passagiere und Arbeiter

####################

##Korrelationen zwischen metrischen und dichotomen Variablen
##Korrelation zwischen Alter und Geschlecht
punktbiseriale_korrelation(data$Age, data$Sex)
# Zeigt, wie Alter und Geschlecht in Beziehung zu einander stehen.
## Der negative Wert bedeutet, dass männliche Passagiere tendenziell 
## älter waren als weibliche Passagiere.
## Der Zusammenhang ist jedoch sehr schwach (ca. -0.12).

##Korrelation zwischen Alter und Überlebensrate
punktbiseriale_korrelation(data$Age, data$Survived)
# Zeigt, wie Alter und Überlebensrate in Beziehung zu einander stehen.
## Ältere Passagiere hatten eine geringfügig niedrigere Überlebensrate.
## Der Effekt ist jedoch sehr schwach (ca. -0.09).
## Hypothese:
## Kinder hatten eine höhere Überlebenschance ("Frauen und Kinder zuerst").
## Ältere Menschen hatten möglicherweise weniger Mobilität und schlechtere Chancen auf Rettung.
## Der geringe Wert zeigt, dass das Alter kein dominanter Faktor für das Überleben war.

##Korrelation zwischen Ticketpreis und Geschlecht
punktbiseriale_korrelation(data$Fare, data$Sex)
# Zeigt, wie Ticketpreis und Geschlecht in Beziehung zu einander stehen.
## Der positive Wert zeigt, dass Frauen tendenziell teurere Tickets hatten als Männer.
## Die Korrelation ist schwach bis moderat (ca. 0.18).
## Hypothese:
## Frauen reisten häufiger in der Ersten Klasse, da sie eher aus wohlhabenden Familien kamen.
## Männer waren stärker in der Dritten Klasse vertreten, wo die Ticketpreise niedriger waren.

##Korrelation zwischen Ticketpreis und Überlebensrate
punktbiseriale_korrelation(data$Fare, data$Survived)
# Zeigt, wie Ticketpreis und Überlebensrate in Beziehung zu einander stehen.
## Passagiere mit teureren Tickets hatten eine höhere Überlebenswahrscheinlichkeit.
## Der Wert (ca. 0.26) zeigt eine moderate positive Korrelation.
## Hypothese:
## Passagiere der Ersten Klasse hatten besseren Zugang zur Rettung.
## Dritte-Klasse-Passagiere hatten die schlechtesten Überlebenschancen, da sie 
## oft in vulnerablen Bereichen des Schiffs untergebracht waren.

## => Der Ticketpreis ist der entscheidendste Faktor des Überlebens, während
## das Alter sowie Geschlecht eher eine geringere, aber sichtbare Rolle spielt.
