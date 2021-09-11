#Weibull

inverse_weibull <- function(k, lambda, U){
  y <- lambda * (-log(1-U))^(1/k)
  return(y)
}

simulate_weibull <- function(k, lambda, N){
  U <- runif(N)
  values <- inverse_weibull(k, lambda, U)
  return(values)
}|

plot_weibull <- function(k, lambda, N){
  values = simulate_weibull(k, lambda, N)
  hist(values, breaks = 100)
}

#Pareto

inverse_pareto <- function(a, b, U){
  y <- ((b)*(1-(1-U)^(1/a)))/((1-U)^(1/a))
  return(y)
}

simulate_pareto <- function(a, b, N){
  U <- runif(N)
  values <- inverse_pareto(a, b, U)
  return(values)
}

plot_pareto <- function(a, b, N){
  values = simulate_pareto(a, b, N)
  hist(values, breaks = 100)
}

#Generar gráficas
#Pendiente: dejar bonitos los histogramas.
plot_weibull(1, 5, 50000)
plot_pareto(10, 3, 50000)

# Histograma Weibull(1,5),

# Funcion de distribución Weibull
f1 <- function(x, k, lamda){
  (k/lamda)*((x/lamda)^(k-1))*exp(-(x/lamda)^k)
}

hist(Weibull(1, 5, 100000), breaks = 100, probability = TRUE)
x1 <- seq(0, 50, by=0.01)
lines(x1, f1(x1, 1, 5), col = 'blue')

# Histograma Pareto

# Función de distribución Pareto
f2 <- function(x, a, b){
  (a*b^a)/((b+x)^(a+1))
}

par(new = TRUE, lines(x2, f(x2, 10, 3), col = 'blue'))
x2 <- seq(0, 5, by=0.01)
lines(x2, f2(x2, 10, 3), col = 'blue')

#####

# Pareto(10,3)
# X = ((b)(1-(1-u)^(1/a)))/((1-u)^(1/a))

Pareto <- function(a, b, N){
  U <- runif(N, min = 0, max = 1)

  X <- ((b)*(1-(1-U)^(1/a)))/((1-U)^(1/a))
  print(X)
}
Pareto(1, 3, 10)
hist(Pareto(10, 3, 50000), breaks = 100)
