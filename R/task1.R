# Load the `here` package for robust file path handling
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
library(here)

# 1. Read the Titanic dataset
# File is located in data/raw/titanic.csv
file_path <- here("data", "raw", "titanic.csv")
titanic <- read.csv(file_path, stringsAsFactors = FALSE)

# 2. Extract the title from the Name column
titanic$Title <- sub(".*(Mr\\.|Mrs\\.|Miss\\.|Master\\.|Dr\\.|Mlle|Ms\\.|Rev\\.|Major\\.|Col\\.|Capt\\.|Don\\.|Lady\\.|Jonkheer\\.|Sir\\.|Mme).*", "\\1", titanic$Name)

# Handle cases where no title is found
titanic$Title[!grepl("Mr\\.|Mrs\\.|Miss\\.|Master\\.|Dr\\.|Mlle|Ms\\.|Rev\\.|Major\\.|Col\\.|Capt\\.|Don\\.|Lady\\.|Jonkheer\\.|Sir\\.|Mme", titanic$Name)] <- "titleless"

# Combine equivalent titles
titanic$Title <- gsub("Miss\\.|Mlle", "Miss", titanic$Title)
titanic$Title <- gsub("Ms\\.", "Miss", titanic$Title)
titanic$Title <- gsub("Mme", "Mrs", titanic$Title)

# Add the extracted titles as a column for analysis
titanic$Title_Group <- ifelse(titanic$Title %in% c("Mr.", "Mrs.", "Miss", "Master"), titanic$Title, "Other")

# 3. Convert relevant columns to factors
# - Convert "Survived" to a factor
#   1 = Yes (survived), 0 = No (did not survive)
titanic$Survived <- factor(titanic$Survived, levels = c(0, 1), labels = c("No", "Yes"))

# - Convert "Sex" to a factor
titanic$Sex <- factor(titanic$Sex, levels = c("male", "female"))

# - Convert "Embarked" to a factor
titanic$Embarked <- factor(titanic$Embarked, levels = c("C", "Q", "S"), labels = c("Cherbourg", "Queenstown", "Southampton"))

# 4. Convert "Pclass" to an ordered factor
titanic$Pclass <- factor(titanic$Pclass, levels = c(3, 2, 1), labels = c("third", "second", "first"))

# 5. Remove unnecessary columns
# Keep only: PassengerId, Survived, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked, Title
titanic <- titanic[, c("PassengerId", "Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked", "Title")]

# 6. Save the cleaned dataset
# Save to data/processed/titanic_cleaned.csv
output_path <- here("data", "processed", "titanic_cleaned.csv")
write.csv(titanic, output_path, row.names = FALSE)
