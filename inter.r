library(Deriv)
library(polynom)

# Se crea una semilla
set.seed(123)

# Parámetros
media <- 20  # Temperatura promedio en grados Celsius
desv_std <- 5  # Desviación estándar

# Se crea un vector para los dias de la semana
dias <- rep(1:7)

# Se crea el vector con las temperaturas
temperaturas <- rnorm(n = length(dias), mean = media, sd = desv_std)

for (elemento in temperaturas) {
  elemento <- round(elemento, digits = 2)
}
# Funcion para derivar
derivar <- function(poli, n){
  if (n == 0){
    return (poli)
  } else {
    derivada <- deriv(poli, "x")
    return (derivar(derivada,n-1))
  }
  
}

taylor <- function (polinomio, orden, x0, x){
  n <- 0
  suma <- ""
  for (i in 1:(orden+1)){
    print(i)
    if (n == 0) {
      numero<- predict(polinomio, x)
      suma <-  numero
      n <- n+1
      print (i)

    } else{
      derivada <- derivar (polinomio, i-1)
      numero <- predict(derivada, x)
      suma <- suma + (( numero/factorial(i-1))* (x-x0)^(i-1))
      n<- n+1
      print (i)
    }
    
  }

  return (suma)
}

for (i in length(temperaturas)){
  print(taylor(polinomio, 6, 20, temperaturas[i]))
}



