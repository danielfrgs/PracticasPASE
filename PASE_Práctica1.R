# Problema 1
matriz_hilbert <- function(n){
  matriz = matrix(nrow = n, ncol = n)
  for(i in 1:n){
    for(j in 1:n){
      matriz[i,j] = 1/(i+j-1)
    }
  }
  return(matriz)
}

print(matriz_hilbert(5))

for(i in 1:40){
  if (det(matriz_hilbert(i)) == 0) {
    print(i)
    break
  }
}
