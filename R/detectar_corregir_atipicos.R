# Función para manejo de outliers por IQR
detectar_filtrar_outliers <- function(x, multiplicador = 1.5, remover = TRUE) {
  # Calcular Q1, Q3 e IQR
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr_val <- q3 - q1

  # Definir los límites inferior y superior
  limite_inferior <- q1 - (multiplicador * iqr_val)
  limite_superior <- q3 + (multiplicador * iqr_val)

  # Identificar posición de los valores atípicos
  n_atipico <- x < limite_inferior | x > limite_superior
  cantidad_atipicos <- sum(n_atipico, na.rm = TRUE)

  cat("Cantidad de valores atípicos detectados y corregidos:", cantidad_atipicos, "\n")

  if (remover) {
    # Retorna el vector sin los valores atípicos
    return(x[!n_atipico])
  } else {
    # Retorna el vector lógico (TRUE/FALSE)
    return(n_atipico)
  }
}
