library(DescTools)
library(e1071)

##data <- read.csv("/Users/deinname/Downloads/titanic_cleaned.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

## i)

##Dokumentation
## metric_variables   - Funktion, welche deskriptive Statistiken für metrische 
##                      Variablen berechnet und ausgibt
## input              - x: Eine Spalte einer metrischen Variable aus 
##                        "titanic_cleaned.csv"
## output             - benannte Liste mit:
##                      ArithmetischesMittel
##                      Median
##                      Modus
##                      Spannweite
##                      Interquantilsabstand (IQR)
##                      Varianz
##                      Standardabweichung (SD)
##                      Variationskoeffizient (CV)
##                      Schiefe (skewness)
##                      Kurtosis (Wölbung)
##                      Min./Max.
##                      Quartile
##                      Perzentile

metric_variables <- function(x) {
## zentrale Tendenz
## arithmetisches Mittel
  mean <- mean(x, na.rm = TRUE)
## median
  med <- median(x, na.rm = TRUE)
## modus
  mod <- Mode(x, na.rm = TRUE)
  
  
## Streuungsmaße
## Spannweite (range)
  rng <- range(x, na.rm = TRUE)
## Interquantilsabstand
  iqr <- IQR(x, na.rm = TRUE)
## Varianz
  var <- var(x, na.rm = TRUE)
## Standardabweichung (SD)
  sd <- sd(x, na.rm = TRUE)
## Variationskoeffizient (CV)
  cv <- (sd / mean) * 100
## Verteilungsmaße
## Schiefe (skewness)
  skns <- skewness(x, na.rm = TRUE)
## Kurtosis (Wölbung)
  krts <- kurtosis(x, na.rm = TRUE)
  
  
## Position/Perzentile
## Min./Max.
  min <- min(x, na.rm = TRUE)
  max <- max(x, na.rm = TRUE)
##Quartile
  quartile <- quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
##Perzentile
  perzentile <- quantile(x, probs = c(0.05, 0.10, 0.90, 0.95), na.rm = TRUE)
  
  
## Ausgabe
return(list(ArithmetischesMittel = mean, 
            Median = med, 
            Modus = mod, 
            Spannweite = rng, 
            Interquantilsabstand = iqr, 
            Varianz = var, 
            Standardabweichung = sd, 
            Variationskoeffizient = cv, 
            Schiefe = skns, 
            Kurtosis = krts, 
            Minimum = min, 
            Maximum = max, 
            Quartile = quartile, 
            Perzentile = perzentile))
}

## Kategorisierung
data$Age #metrisch

data$Fare #metrisch

data$SibSp #ordinal

data$Parch #ordinal

## Testbeispiele
metric_variables(data$Age)
metric_variables(data$Fare)
metric_variables(data$SibSp)
metric_variables(data$Parch)




