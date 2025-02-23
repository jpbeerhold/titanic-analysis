library(dplyr)
library(tidyr)
library(here)
library(rstudioapi)
library(moments)
library(DescTools)
library(ggplot2)

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

# ii)

# Hauptfunktion zur Berechnung beschreibender Statistiken für kategoriale Variablen
#
# Argumente:
#   data: Eingabedaten als Data Frame.
#   categorical_vars: Vektor mit Strings als Variablennamen.
#
# Rückgabe:
#   Data Frame
calculate_categorical_stats <- function(data, categorical_vars) {
  # Überprüfen, ob die angegebenen Variablen im Data Frame existieren
  if (!all(categorical_vars %in% names(data))) {
    stop("Einige kategoriale Variablen sind nicht im Data Frame enthalten.")
  }
  
  # Eine Liste initialisieren, um die Ergebnisse zu speichern
  stats_list <- list()
  
  # Schleife durch jede kategoriale Variable
  for (var in categorical_vars) {
    # Berechnung der Statistiken mit der internen Hilfsfunktion
    stats <- internal_categorical_summary(data[[var]])
    stats_list[[var]] <- stats
  }
  
  # Umwandlung der Liste in einen Data Frame für bessere Lesbarkeit
  stats_df <- bind_rows(stats_list, .id = "Variable")
  
  return(stats_df)
}


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

# iv)

punktbiseriale_korrelation <- function(metrische_variable, dichotome_variable) {
  # Korrelatiosfunktion zwischen den Zwei dichtomen Gruppen berechnen
  
  # Sicherstellen, dass die dichotome Variable nur zwei Werte hat
  unique_values <- unique(dichotome_variable)
  if (length(unique_values) != 2) {
    stop("Die dichotome Variable muss genau zwei Werte enthalten.")
  }
  
  # Dichotome Variable in numerische Werte umkodieren
  dichotome_variable <- as.numeric(dichotome_variable == unique_values[2])
  
  # Gruppengrößen bestimmen
  n1 <- sum(dichotome_variable == 1)
  n0 <- sum(dichotome_variable == 0)
  n <- n1 + n0
  
  # Mittelwerte der Gruppen berechnen
  mean1 <- mean(metrische_variable[dichotome_variable == 1])
  mean0 <- mean(metrische_variable[dichotome_variable == 0])
  
  # Standardabweichung der metrischen Variable berechnen
  sd_metrisch <- sd(metrische_variable)
  
  # Punktbiseriale Korrelation berechnen
  r_pb <- (mean1 - mean0) / sd_metrisch * sqrt((n1 * n0) / n^2)
  
  # Ergebnis ausgeben
  return(r_pb)
}

# v)

## das ist für b)

##Dokumentation
## check_variables    - Hilfsfunktion, welche überprüft, ob Variablen im 
##                      Datensatz existieren
## input              - data: aus "titanic_cleaned.csv"
##                      vars: Ein Vektor mit den Namen der Variablen, die 
##                            geprüft werden sollen
## output             - Fehler, falls eine Variable fehlt.

check_variables <- function(data, vars) {
  # Fehlende Variablen identifizieren
  missing_vars <- vars[!vars %in% colnames(data)]
  # Fehler ausgeben, falls Variablen fehlen
  if (length(missing_vars) > 0) {
    stop(paste("Die folgenden Variablen fehlen im Datensatz:", paste(missing_vars, collapse = ", ")))
  }
}

##Dokumentation
## visualize_categorical    - Funktion, welche zwei bis vier kategoriale 
##                            Variablen visualisiert
##                            Zwei Variablen: Balkendiagramm mit Gruppierung
##                            Drei Variablen: Facettierte Balkendiagramme 
##                            (nach der dritten Variable)
##                            Vier Variablen: Facettierte Balkendiagramme mit 
##                            zwei Facettenebenen
## input                    - data: aus "titanic_cleaned.csv"
##                            var1: Name erste kat. Variable 
##                            var2: Name zweite kat. Variable
##                            var3: (Optional) Name dritte kat. Variable
##                            var4: (Optional) Name vierte kat. Variable
## output                   - Visualisierung, je nachdem, wie viele Variablen
##                            angegeben wurden

visualize_categorical <- function(data, var1, var2, var3 = NULL, var4 = NULL) {
  ## Alle verwendeten Variablen sammeln
  vars <- c(var1, var2)
  if (!is.null(var3)) vars <- c(vars, var3)
  if (!is.null(var4)) vars <- c(vars, var4)
  
  ## Überprüfung, ob alle Variablen existieren
  check_variables(data, vars)
  
  ## Ploterstellung abhängig von der Anzahl der Variablen
  if (is.null(var3) && is.null(var4)) {
    ## Zwei kategoriale Variablen -> Balkendiagramm mit Gruppierung
    p <- ggplot(data) +
      geom_bar(aes_string(x = var1, fill = var2), position = "dodge") +
      labs(
        title = paste("Visualisierung von", var1, "und", var2),
        x = var1,
        fill = var2
      ) +
      theme_minimal()
    
  } else if (!is.null(var3) && is.null(var4)) {
    ## Drei kategoriale Variablen -> Facettierte Balkendiagramme
    p <- ggplot(data) +
      geom_bar(aes_string(x = var1, fill = var2), position = "dodge") +
      facet_wrap(as.formula(paste("~", var3))) +
      labs(
        title = paste("Visualisierung von", var1, ",", var2, "und", var3),
        x = var1,
        fill = var2
      ) +
      theme_minimal()
    
  } else if (!is.null(var4)) {
    ## Vier kategoriale Variablen -> Facettierte Diagramme mit zwei Facettenebenen
    p <- ggplot(data) +
      geom_bar(aes_string(x = var1, fill = var2), position = "dodge") +
      facet_grid(as.formula(paste(var3, "~", var4))) +
      labs(
        title = paste("Visualisierung von", paste(vars, collapse = ", ")),
        x = var1,
        fill = var2
      ) +
      theme_classic()
  }
  
  return(p)
}



# Interne Hilfsfunktion zur Berechnung zusammenfassender Statistiken für eine einzelne kategoriale Variable.
# 
# Argumente:
#   variable: String als Variablenname.
#
# Rückgabe:
#   Data Frame.
internal_categorical_summary <- function(variable) {
  # Erstellen einer Häufigkeitstabelle
  freq_table <- table(variable)
  
  # Berechnung der Anteile
  proportions <- prop.table(freq_table)
  
  # Erstellen eines zusammenfassenden Data Frames
  summary_df <- data.frame(
    Kategorie = names(freq_table),
    Häufigkeit = as.integer(freq_table),
    Anteil = as.numeric(proportions)
  )
  
  return(summary_df)
}
