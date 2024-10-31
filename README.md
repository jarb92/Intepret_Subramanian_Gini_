# Intepret_Subramanian_Gini_




# Generando variables aleatorias.


# Función para generar datos aleatorios de renta e individuos
generar_datos <- function(n) {
  # Generar IDs de los individuos
  individuos <- 1:n
  
  # Generar rentas aleatorias
  # Puedes ajustar la distribución según necesites; aquí usamos una distribución normal
  renta <- rnorm(n, mean = 1e6, sd = 500000)  # Rentas con media 50000 y desviación estándar 10000
  
  # Crear un dataframe con los datos generados
  datos <- data.frame(Individuos = individuos, Renta = renta)
  
  return(datos)
}


set.seed(123)  # Fijar semilla para reproducibilidad
datos <- generar_datos(10000000)
gini <- Gini(datos$Renta)
print(paste("El índice de Gini es:", round(gini, 4)))



S<-(1-gini)/2






# Función para calcular la proporción de la renta del 50% más pobre
proporcion_renta_50_mas_pobre <- function(datos) {
  # Asegurarse de que el dataframe está ordenado por renta en orden ascendente
  datos <- datos[order(datos$Renta), ]
  
  # Calcular el número total de individuos y el total de renta
  n <- nrow(datos)
  renta_total <- sum(datos$Renta)
  
  # Calcular el índice que corresponde al 50% de la población
  mitad <- floor(n / 2)
  
  # Sumar la renta del 50% más pobre
  renta_50_mas_pobre <- sum(datos$Renta[1:mitad])
  
  # Calcular la proporción de la renta del 50% más pobre respecto al total
  proporcion <- renta_50_mas_pobre / renta_total
  
  return(proporcion)
}

# Usando el dataframe 'datos' existente
proporcion <- proporcion_renta_50_mas_pobre(datos)
print(paste("La proporción de la renta del 50% más pobre es:", round(proporcion * 100, 2), "%"))
print(paste("Sigma de Subramanian es:", round(S* 100, 2),"%"))





