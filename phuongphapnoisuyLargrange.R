LargCoef <- function(xi,x,j)
  #xi la danh sach cac diem du lieu goc 
{
  N <- length(xi)
  Lj <- 1;
  for (k in 1 : N){
    if (j != k){
      Lj <-  Lj*(x-xi[k]/(xi[j]-xi[k]))
    }
  }
  return(Lj)
}

LargInter <- function (xi, Fi,x){
  N <- length(xi)
  P <- 0;
  for (j in 1: N){
    P <- P + LargCoef(xi, x, j)*Fi[j]
  }
  return(P)
}
xi <- 4 :7
Fi <- c(-0.0660433280, -0.3275791376, -0.2766838581, - 0.0046828235)
plot(xi, Fi, col = "red")
x <- seq(min(xi), max(xi), by = 0.1)
y <- LargInter(xi, Fi, x)
lines(x,y)
