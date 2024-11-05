library(ggplot2)

dias <- c(1:365)
temperaturas <- c(15 + 10 * sin(2 * pi * dias / 365) + rnorm(365, mean = 0, sd = 2))# nolint: line_length_linter.

dias
temperaturas

n <- length(temperaturas)

pol_base <- function(t, dias) {
  l <- c(0)
  for (i in 1:n){
    l[i] <- 1
    for (j in 1:n) {
      if (i != j) {
        l[i] <- l[i] * ((t - dias[j]) / (dias[i] - dias[j]))
      }
    }
  }
  return(l)
}

pol_inte <- function(temperaturas, l) {
  p <- 0
  for (i in 1:n){
    p <- p + (temperaturas[i] * l[i])
  }
  return(p)
}

l <- c(0)
p <- 0
dias[1]
n <- length(temperaturas)
dias[n]
x <- seq(dias[1], dias[n], length = 365)

f <- 0

for (k in 1:365){
  t <- x[k]
  lb <- pol_base(t, dias)
  pi <- pol_inte(temperaturas, lb)
  f[k] <- pi
}

data_plot <- data.frame(Días = dias, Temperaturas = temperaturas)
data_interp <- data.frame(Días = x, Temperatura_Interpolada = f)


ggplot() +
  geom_point(data = data_plot, aes(x = Días, y = Temperaturas), color = "red") + # nolint: object_name_linter, line_length_linter.
  geom_line(data = data_interp, aes(x = Días, y = Temperatura_Interpolada), color = "blue") + # nolint: line_length_linter, object_name_linter.
  labs(title = "Temperaturas y Polinomio Interpolado", x = "Días", y = "Temperatura") + # nolint: line_length_linter.
  xlim(c(dias[1], dias[n])) + ylim(c(0, 35)) + theme_bw() # nolint: line_length_linter.
