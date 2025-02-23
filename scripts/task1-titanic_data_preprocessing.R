# Lade das "here" Paket
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
library(here)

# 0. Lese den Titanic Datensatz
file_path <- here("data", "raw", "titanic.csv")
titanic <- read.csv(file_path, stringsAsFactors = FALSE)

# 1. Extrahiere den Titel von der Namen Spalte
titanic$Title <- sub(".*(Mr\\.|Mrs\\.|Miss\\.|Master\\.|Dr\\.|Mlle|Ms\\.|Rev\\.|Major\\.|Col\\.|Capt\\.|Don\\.|Lady\\.|Jonkheer\\.|Sir\\.|Mme).*", "\\1", titanic$Name)

# Behandle Fälle, in denen kein Titel gefunden wurde
titanic$Title[!grepl("Mr\\.|Mrs\\.|Miss\\.|Master\\.|Dr\\.|Mlle|Ms\\.|Rev\\.|Major\\.|Col\\.|Capt\\.|Don\\.|Lady\\.|Jonkheer\\.|Sir\\.|Mme", titanic$Name)] <- "titleless"

# Kombiniere äquivaltente Titel
titanic$Title <- gsub("Miss\\.|Mlle", "Miss", titanic$Title)
titanic$Title <- gsub("Ms\\.", "Miss", titanic$Title)
titanic$Title <- gsub("Mme", "Mrs", titanic$Title)

# Füge die extrahierten Titel als neue Spalte für die Analyse hinzu
titanic$Title_Group <- ifelse(titanic$Title %in% c("Mr.", "Mrs.", "Miss", "Master"), titanic$Title, "Other")

# 2. Relevante Spalten in Faktoren umwandeln
# - Wandle "Survived" in einen Faktor um
#   1 = Ja (überlebt), 0 = Nein (nicht überlebt)
titanic$Survived <- factor(titanic$Survived, levels = c(0, 1), labels = c("No", "Yes"))

# - Konvertiere "Sex" zu einem Faktor
titanic$Sex <- factor(titanic$Sex, levels = c("male", "female"))

# - Konvertiere "Embarked" zu einem Faktor
titanic$Embarked <- factor(titanic$Embarked, levels = c("C", "Q", "S"), labels = c("Cherbourg", "Queenstown", "Southampton"))

# 3. Wandle "Pclass" in einen geordneten Faktor um
titanic$Pclass <- factor(titanic$Pclass, levels = c(3, 2, 1), labels = c("third", "second", "first"))

# 4. Imputiere fehlende Werte mit der Anrede (Median oder Arithmetisches Mittel)

# Runde das ari Mittel ab
mean_title <- tapply(titanic$Age, titanic$Title, function(x) floor(mean(x, na.rm = TRUE)))
titanic$Age <- ifelse(is.na(titanic$Age), mean_title[titanic$Title], titanic$Age)

# 5. 
# iii) Setze die ohne Angaben als NAs
titanic$Cabin[titanic$Cabin == ""] <- NA

# i) Backbord(gerade) Steuerbord(ungerade)
# Funktion zum Extrahieren der Nummern und damit der Seite der Kabine
Side <- function(c){
  # Extrahiere die Nummer
  Number <- regexpr("\\d+", c)
  cabin_num <- as.numeric(regmatches(c, Number))
  
  # Prüfe, ob eine Nummer in der Kabine vorhanden ist, falls nicht:
  if (length(cabin_num) == 0 || is.na(cabin_num)) {
    return(NA)
  }
  # Ungerade = Steuerbord
  if (cabin_num %% 2 == 1) {
    return("Starboard")
  } 
  else { # Gerade = Backbord
    return("Larboard")
  }
}

titanic$Side <- sapply(titanic$Cabin, Side) # Generiere Titanic$Side im DF

# ii)
# Funktion zum Extrahieren der Buchstaben der Kabine
Deck <- function(x){
  if (is.null(x)) { 
    return(NA)
  }
  # Extrahiere den Buchstaben
  Letter <- gsub("\\d.*", "", x)
  return(Letter)
}
titanic$Deck <- sapply(titanic$Cabin, Deck) # generate Titanic$Deck in df

# 6. Entferne unnötige Spalten
# Behalte nur: PassengerId, Survived, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked, Title
# Und die neu hinzugefügten Spalten: "Side", "Deck"
titanic <- titanic[, c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked", "Title", "Side", "Deck")]

# 7. Speichere den bereinigten Datensatz
# Speichere in data/processed/titanic_cleaned.csv
output_path <- here("data", "processed", "titanic_cleaned.csv")
write.csv(titanic, output_path, row.names = FALSE)
