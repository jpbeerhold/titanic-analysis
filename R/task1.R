# Load the `here` package for robust file path handling
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
library(here)

# 0. Read the Titanic dataset
# File is located in data/raw/titanic.csv
file_path <- here("data", "raw", "titanic.csv")
titanic <- read.csv(file_path, stringsAsFactors = FALSE)

# 1. Extract the title from the Name column
titanic$Title <- sub(".*(Mr\\.|Mrs\\.|Miss\\.|Master\\.|Dr\\.|Mlle|Ms\\.|Rev\\.|Major\\.|Col\\.|Capt\\.|Don\\.|Lady\\.|Jonkheer\\.|Sir\\.|Mme).*", "\\1", titanic$Name)

# Handle cases where no title is found
titanic$Title[!grepl("Mr\\.|Mrs\\.|Miss\\.|Master\\.|Dr\\.|Mlle|Ms\\.|Rev\\.|Major\\.|Col\\.|Capt\\.|Don\\.|Lady\\.|Jonkheer\\.|Sir\\.|Mme", titanic$Name)] <- "titleless"

# Combine equivalent titles
titanic$Title <- gsub("Miss\\.|Mlle", "Miss", titanic$Title)
titanic$Title <- gsub("Ms\\.", "Miss", titanic$Title)
titanic$Title <- gsub("Mme", "Mrs", titanic$Title)

# Add the extracted titles as a column for analysis
titanic$Title_Group <- ifelse(titanic$Title %in% c("Mr.", "Mrs.", "Miss", "Master"), titanic$Title, "Other")

# 2. Convert relevant columns to factors
# - Convert "Survived" to a factor
#   1 = Yes (survived), 0 = No (did not survive)
titanic$Survived <- factor(titanic$Survived, levels = c(0, 1), labels = c("No", "Yes"))

# - Convert "Sex" to a factor
titanic$Sex <- factor(titanic$Sex, levels = c("male", "female"))

# - Convert "Embarked" to a factor
titanic$Embarked <- factor(titanic$Embarked, levels = c("C", "Q", "S"), labels = c("Cherbourg", "Queenstown", "Southampton"))

# 3. Convert "Pclass" to an ordered factor
titanic$Pclass <- factor(titanic$Pclass, levels = c(3, 2, 1), labels = c("third", "second", "first"))

# 4. Imputiere fehlende Werte mit der Anrede (Median oder Arithmetisches Mittel)
# 4. Imputate missing Values with title

# floor the mean for logical purposes
mean_anrede <- tapply(titanic$Age, titanic$Title, function(x) floor(mean(x, na.rm = TRUE)))
titanic$Age <- ifelse(is.na(titanic$Age), mean_anrede[titanic$Title],titanic$Age)

# hier drunter noch die Median Version falls wir die nutzen wollen

# Median Version
# Median_anrede <- tapply(titanic$Age, titanic$Title, function(x) median(x, na.rm = TRUE))
# titanic$Age <- ifelse(is.na(titanic$Age), Median_anrede[titanic$Title],titanic$Age)

# 5. 
# iii) Set those that have nothing noted as NAs
titanic$Cabin[titanic$Cabin == ""] <- NA

# i) Backbord(gerade) Steuerbord(ungerade)
# Function to get the Numbers thus the side of the Cabin
Side <- function(c){
  # extract Number
  Number <- regexpr("\\d+", c)
  Cabin_num <- as.numeric(regmatches(c, Number))
  
  # check if there is a number in Cabin if not:
  if (length(Cabin_num) == 0 || is.na(Cabin_num)) {
    return(NA)
  }
  # Uneven = Steuerbord = Starboard
  if (Cabin_num %% 2 == 1) {
    return("Starboard")
  } 
  else { # Even = Backbord = Larboard
    return("Larboard")
  }
}

titanic$Side <- sapply(titanic$Cabin, Side) # generate Titanic$Side in df

# ii)
# Function to get the letters of the Cabin
Deck <- function(x){
  if (is.null(x)) { 
    return(NA)
  }
  # Extract the letter
  Letter <- gsub("\\d.*", "", x)
  return(Letter)
}
titanic$Deck <- sapply(titanic$Cabin, Deck) # generate Titanic$Deck in df


# 6. Remove unnecessary columns
# Keep only: PassengerId, Survived, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked, Title
# And the newly generated ones added: "Side", "Deck"
titanic <- titanic[, c("PassengerId", "Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked", "Title", "Side", "Deck")]

# 7. Save the cleaned dataset
# Save to data/processed/titanic_cleaned.csv
output_path <- here("data", "processed", "titanic_cleaned.csv")
write.csv(titanic, output_path, row.names = FALSE)