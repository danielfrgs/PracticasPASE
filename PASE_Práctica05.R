#Weibull

inverse_weibull <- function(k, lambda, U){
  y <- lambda * (-log(1-U))^(1/k)
  return(y)
}

simulate_weibull <- function(k, lambda, N){
  U <- runif(N)
  values <- inverse_weibull(k, lambda, U)
  return(values)
}

dens_weibull <- function(k, lamda, x){
  (k/lamda)*((x/lamda)^(k-1))*exp(-(x/lamda)^k)
}

plot_weibull <- function(k, lambda, N){
  values = simulate_weibull(k, lambda, N)
  hist(values, breaks = 100, probability = TRUE, 
       xlim=c(0,30), ylim= c(0, 0.23), 
       main = NULL, xlab = NULL, ylab = NULL,)
  title(main = 'Weibull')
  text(15, 0.15, labels = 'n = 10000')
  x1 <- seq(0, 50, by=0.01)
  par(new = TRUE, lines(x1, dens_weibull(k, lambda, x1), col = 'blue'))
}

#Pareto

inverse_pareto <- function(a, b, U){
  y <- b*(1/((1-U)^(1/a))-1)
  return(y)
}

simulate_pareto <- function(a, b, N){
  U <- runif(N)
  values <- inverse_pareto(a, b, U)
  return(values)
}

dens_pareto <- function(a, b, x){
  (a*b^a)/((b+x)^(a+1))
}

plot_pareto <- function(a, b, N){
  values = simulate_pareto(a, b, N)
  hist(values, breaks = 100, probability = TRUE, 
       xlim=c(0, 2.5), ylim= c(0, 3.5), 
       main = NULL, xlab = NULL, ylab = NULL,)
  title(main = 'Pareto')
  text(1, 2.5, labels = 'n = 100')
  x2 <- seq(0, 5, by=0.01)
  par(new = TRUE, lines(x2, dens_pareto(a, b, x2), col = 'blue'))
}


#Generar graficas
plot_weibull(1, 5, 10000)
plot_pareto(10, 3, 10000)
