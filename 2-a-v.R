library(ggplot2)

##data <- read.csv("/Users/deinname/Downloads/titanic_cleaned.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

## v)

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

## Testbeispiele
visualize_categorical(data, "Embarked", "Survived")
visualize_categorical(data, "Title", "Sex")
visualize_categorical(data, "Title", "Embarked", "Survived")
visualize_categorical(data, "Embarked", "Title", "Sex", "Survived")

