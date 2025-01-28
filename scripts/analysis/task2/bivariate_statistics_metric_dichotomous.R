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