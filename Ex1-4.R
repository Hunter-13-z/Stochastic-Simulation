c <- 1
xi <- 1
mu <- c/xi
lambda <- c / xi^3

generate_geo <- function(n){
  U <- runif(n)
  samples <- -log(U)
  
  return(samples)
}

generate_noraml <- function(n){
  repeat{
    Y1 = generate_geo(n)
    Y2 = generate_geo(n)
    print(1)
    if( any(Y2 >= (Y1-1)^2 / 2) ){
      Z <- Y1
      break
    }
  }
  U = runif(n)
  for(i in 1:n){
    if(U[i] <= 0.5){
      Z[i] = -Z[i]
    }
  }
  return(Z)
}
n <- 10000
normal <- generate_noraml(n)

hist(normal, breaks = 50, probability = TRUE, 
     main = "Histogram of Simulated Normal Random Variables",
     xlab = "X", col = "lightblue", border = "black")


