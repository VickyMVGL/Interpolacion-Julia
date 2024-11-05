dias <- 1:365
temperaturas <- runif(365, min = 20, max = 35)
n_puntos <- 365  # Número de puntos interpolados
dias_interpolados <- seq(1, 365, length.out = n_puntos)
temperaturas_interpoladas_manual <- numeric(n_puntos)

for (i in 2:length(dias)) {

    dias_tramo <- dias[i-1]:dias[i]
    m <- (temperaturas[i] - temperaturas[i-1]) / (dias[i] - dias[i-1])
    b <- temperaturas[i-1] - m * dias[i-1]
    temperaturas_interpoladas_manual[dias_tramo] <- m * dias_tramo + b

}

interpolacion_spline <- spline(x = dias, y = temperaturas, n = 365)
plot(dias, temperaturas, type = "p", col = "blue", pch = 16, main = "Temperaturas diarias y predicciones interpoladas en Valencia, Venezuela", xlab = "Día del año", ylab = "Temperatura (°C)")
lines(dias_interpolados, temperaturas_interpoladas_manual, col = "red", lwd = 2)
lines(interpolacion_spline, col = "green", lwd = 2)
legend("topright", legend = c("Datos Originales", "Interpolación Manual", "Interpolación Spline"), col = c("blue", "red", "green"), pch = c(16, NA, NA), lty = c(NA, 1, 1), lwd = 2)