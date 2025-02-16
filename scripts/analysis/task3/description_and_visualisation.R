data <- read.csv("/Users/deinname/Downloads/titanic_cleaned.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

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

