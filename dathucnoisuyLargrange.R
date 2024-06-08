LargCoef <- function(xi, x, j){
  N <- length(xi)
  Lj <- 1;
  for (k in 1:N) {
    if (j != k){
      Lj <- Lj*(x - xi[k])/(xi[j]-xi[k])
    }
  }
  return(Lj)
}
LargInter <- function(xi, Fi, x){
  N <- length(xi)
  P <- 0;
  for (j in 1:N){
    P <- P + LargCoef(xi, x, j)*Fi[j]
  }
  return(P)
}

xi <- 0 : 3
#yi <- besselJ(xi, nu = 0)
# LargInter(xi,yi,x = 0.5)
Fi <- c(1.0000000000, 0.7651976866, 0.2238907791, -0.2600519549) #J1(p)
plot(xi, Fi, col = "red")
x <- seq(min(xi), max(xi), by = 0.1)
y <- LargInter(xi, Fi, x)
lines(x,y)

#btvn hieu code 