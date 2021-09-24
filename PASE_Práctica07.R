#Ejercicio 1

plt_ex01 <- function(r, n, lbd){
  p = lbd/n
  plot(dbinom(r,n,p), type = 's', col = 'blue')
  lines(dpois(r,lbd), type = 's', col = 'red')
}

r = c(0:40)
n1 = 200
n2 = 500
n3 = 1000
lbd1 = 5
lbd2 = 10

#Graficas Ej1
plt_ex01(r,n1,lbd1)
plt_ex01(r,n2,lbd1)
plt_ex01(r,n3,lbd1)

plt_ex01(r,n1,lbd2)
plt_ex01(r,n2,lbd2)
plt_ex01(r,n3,lbd2)

error <- function(r, n, lbd){
  p = lbd/n
  max(abs(dbinom(r,n,p) - dpois(r,lbd)))
}

#Ejercicio 2
