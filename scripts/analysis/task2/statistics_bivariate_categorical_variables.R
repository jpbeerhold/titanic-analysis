library(DescTools)
library(e1071)

##data <- read.csv("/Users/deinname/Downloads/titanic_cleaned.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

## iii)

##Dokumentation
## bivariate_two_categorial   - Funktion, welche deskriptive bivariate Statistiken 
##                              für den Zusammenhang zwischen zwei kategorialen 
##                              Variablen berechnet und ausgibt
## input                      - data: aus "titanic_cleaned.csv"
##                              x: eine nominalskalierte Datenreihe aus data
##                              -> Pclass ist ordinalskaliert (ϕ, C und λ nicht optimal)
##                              y: eine nominalskalierte Datenreihe aus data
##                              -> Pclass ist ordinalskaliert (ϕ, C und λ nicht optimal)
## output                     - benannte Liste mit:
##                              Kontingenztafel
##                              Chi_Quadrat_Test
##                              Phi_Koeffizient (ϕ)
##                              Cramers_V
##                              Kontingenzkoeffizient (C)
##                              Lambda_Koeffizient (λ)
##                              Tschuprows_T

bivariate_two_categorial <- function(data, x, y) {
  
  if(is.character(data[[x]]) && is.character(data[[y]])) {
    ## Kontingenztafel
    contingency_table <- table(data[[x]], data[[y]])
    ## Werte aus der Kontingenztafel
    n <- sum(contingency_table)
    k <- min(dim(contingency_table))
    ## χ²-Test
    chisq_test <- chisq.test(contingency_table, correct = FALSE)
    ## Wert aus dem Chi-Quadrat-Test
    s <- chisq_test$statistic
    ## ϕ-Koeffizient
    phi <- Phi(contingency_table)
    ## Cramérs V
    V <- CramerV(contingency_table)
    ## Kontingenzkoeffizient
    C <- sqrt(s / (s + n))
    ## Lambda-Koeffizient (λ)
    lambda <- Lambda(contingency_table, direction = "symmetric")
    ## Tschuprows
    Tsch <- TschuprowT(contingency_table)
    
    ## Ausgabe
    return(list(Kontingenztafel = contingency_table, 
                Chi_Quadrat_Test = chisq_test,
                Phi_Koeffizient = phi,
                Cramers_V = V,
                Kontingenzkoeffizient = C,
                Lambda_Koeffizient = lambda,
                Tschuprows_T = Tsch))
  } else {
    stop("Metrische Datenreihen sind nicht kategorial!")
  }
  
}

## Kategorisierung
data$Survived #nominal

data$Sex #nominal

data$Embarked #nominal

data$Title #nominal

data$Pclass #ordinal

## Testbeispiele
bivariate_two_categorial(data, "Sex", "Embarked")

bivariate_two_categorial(data, "Title", "Sex")

bivariate_two_categorial(data, "Survived", "Embarked")

bivariate_two_categorial(data, "Pclass", "Survived")

bivariate_two_categorial(data, "Title", "Pclass")

bivariate_two_categorial(data, "Sex", "SibSp") # Test einer nicht kategorialen Variable