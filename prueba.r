library(ggplot2)

# Función para calcular la suma parcial de la serie de Taylor del coseno
taylor_cos <- function(x, n) {
  suma <- 0
  for (i in 0:(n-1)) {
    suma <- suma + (-1)^i * x^(2*i) / factorial(2*i)
  }
  return(suma)
}

# Generar valores de x
x <- seq(-pi, pi, 0.1)

# Evaluar la serie de Taylor para diferentes órdenes
y1 <- taylor_cos(x, 2)  # Primeros 2 términos
y2 <- taylor_cos(x, 4)  # Primeros 4 términos
y3 <- taylor_cos(x, 6)  # Primeros 6 términos

# Crear un data frame
df <- data.frame(x = x, cosx = cos(x), y1 = y1, y2 = y2, y3 = y3)

# Graficar
ggplot(df, aes(x = x)) +
  geom_line(aes(y = cosx), color = "blue") +
  geom_line(aes(y = y1), color = "red") +
  geom_line(aes(y = y2), color = "green") +
  geom_line(aes(y = y3), color = "purple") +
  labs(x = "x", y = "y", title = "Serie de Taylor del Coseno")

