dist_beta <- function(x, alpha, beta){
  if(x <= 0 || x >= 1){
    return(0)
  }
  out <- ((x^(alpha - 1))*(1 - x)^(beta - 1))/beta(alpha, beta)
  return(out)
}

generate_p <- function(top){
  start <- 0
  end <- 1
  x = runif(1, start, end)
  y = runif(1, 0, top)
  return(c(x, y))
}

define_top <- function(alpha, beta){
  if(alpha < 1 || beta < 1){
    print('alpha or beta lesser than 1')
    return(0)
  }
  mode <- (alpha - 1)/(alpha + beta - 2)
  top <- dist_beta(mode,alpha,beta)
  return(top)
}

sim_beta <- function(n, alpha, beta){
  top <- define_top(alpha, beta)
  count_attempts <- 0
  count_successes <- 0
  pointlist <- c()
  while(count_attempts < n){
    point <- generate_p(top)
    print(point)
    count_attempts <- count_attempts + 1
    if(point[2] <= dist_beta(point[1], alpha, beta)){
      pointlist <- append(pointlist, point[1])
      count_successes <- count_successes + 1
    }
  }
  return(pointlist)
}

sim_beta2 <- function(n, alpha, beta){
  top <- define_top(alpha, beta)
  res <- rep(0,n)
  x <- runif(n)
  y <- runif(n)*top
  fx <- dist_beta(x,alpha,beta)
  over <- y > fx
  pendant <- seq(1,n)[over]
  while(any(over))
  {
    npendant <-  length(pendant)
    x[pendant] <- runif(npendant)
    y[pendant] <- runif(npendant)*top
    fx[pendant] <- dist_beta(x[pendant],alpha,beta)

    over <- y[pendant] > fx[pendant]
    pendant <- pendant[over]
  }
  return(x)
}

hist(sim_beta2(40000, 2, 5), 20)


iteraciones <- function(n, alpha, beta){
  top <- define_top(alpha, beta)
  res <- rep(0,n)
  x <- runif(n)
  y <- runif(n)*top
  fx <- dist_beta(x,alpha,beta)
  #print(x)
  #print(y)
  #print(fx)
  over <- y > fx
  pendant <- seq(1,n)[over]
  its <- n
  while(any(over))
  {

    #print(pendant)
    npendant <-  length(pendant)
    x[pendant] <- runif(npendant)
    y[pendant] <- runif(npendant)*top
    fx[pendant] <- dist_beta(x[pendant],alpha,beta)

    its <- its+npendant
    over <- y[pendant] > fx[pendant]
    pendant <- pendant[over]
  }
return(its)
}
