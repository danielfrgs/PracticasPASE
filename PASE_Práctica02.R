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

# Practica3
generador <- function(a,c,m,semilla,n){
  x = c(semilla)
  norm=c(x)
  for(i in 2:(n+1)){
    x[i] = (a*x[i-1]+c)%%m
    norm[i]=x[i]/m
  }
  return(norm[2:(n+1)])
}

# Histograma 1 cuando c = 0 con n = 500
histrograma1 = generador(34, 0, 100, 15, 500)
hist(histrograma1)

# Histograma 2 cuando c!= 0 con n = 500
histograma2 = generador(34, 16, 100, 15, 500)
hist(histograma2)


# Ahora con los valores
a <- 7^5
m <- 2^31 - 1
nums_pseudo_c0 = generador(a, 0, m, 15, 500)
hist(nums_pseudo_c0, probability = TRUE)

nums_pseudo_c = generador(a, 16, m, 15, 500)
hist(nums_pseudo_c,probability = TRUE)



# Conocer el ciclo
# Regresa en que posicion se va volver a repetir
# nums es un vector que contiene los numeros a analizar

conocer_ciclos <- function(nums) {
  visitados = c(nums[1])
  for (i in 2:length(nums)) {
    if (nums[i] %in% visitados) {
      return(i)
    }
    visitados[i] = nums[i]
  }
}



 generadorsemauto <- function(a,c,m,n){
   # Sys.sleep(1)
   semilla = as.numeric(Sys.time())
   x = c(semilla)
   norm=c(x)
   for(i in 2:(n+1)){
     x[i] = (a*x[i-1]+c)%%m
     norm[i]=x[i]/m

   }   
   return(norm[2:(n+1)])
 }

 r1 <- generadorsemauto(13,65,56,1000)
 r2 <- generadorsemauto(13,65,78.7,1000)
 r3 <- generadorsemauto(45,65,98.9,1000)
library(plotly)
plot_ly(x=r1,y=r2,z=r3,type = "scatter3d"type = "scatter3d",mode="markers",size=50)
# para los valores constantes de a y m
r1c <- generadorsemauto(7**5,65,2**(31)-1,1000)
r2c <- generadorsemauto(7**5,65,2**(31)-1,1000)
r3c <- generadorsemauto(7**5,65,2**(31)-1,1000)
plot_ly(x=r1,y=r2,z=r3,type = "scatter3d",mode="markers",size=50,color = "red")


gotas_dentro2 <- function(n){
  x <- generadorsemauto(15, 17,50,n)
  y <- generadorsemauto(15,17,50,n)
  contador_dentro <- 0
  for(i in 1:n){
    if(x[i]^2+y[i]^2 <= 1){
      contador_dentro = contador_dentro + 1
    }
  }
  return(contador_dentro)
}

grafica2 <- function(n){
  x=1:n
  y=c()
  for(i in 1:n){
    y[i]= (4*gotas_dentro2(i))/i
  }
  plot(x,y, pch=16, cex=.5, col="red")
  abline(0,0,pi,0, col="blue")
}
