# Practica2
gotas_dentro <- function(n){
set.seed(cat(Sys.time(),"\n"))
  x <- runif(n, -1,1)
  y <- runif(n,-1,1)
  contador_dentro <- 0
  for(i in 1:n){
    if(x[i]^2+y[i]^2 <= 1){
      contador_dentro = contador_dentro + 1
    }
  }
  return(contador_dentro)
}

grafica <- function(n){
  x=1:n
  y=c()
  for(i in 1:n){
    y[i]= (4*gotas_dentro(i))/i
  }
  plot(x,y, pch=16, cex=.5, col="red")
  abline(0,0,pi,0, col="blue")
}

generador <- function(a,c,m,semilla,n){
  x = c(semilla)
  for(i in 2:(n+1)){
    x[i] = (a*x[i-1]+c)%%m

  }
  return(x[2:(n+1)])
}

# Histograma 1
histrograma1 = generador(34, 0, 100, 15, 16)
hist(histrograma1)
