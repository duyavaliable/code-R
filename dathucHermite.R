LargCoef <- function(xi, x, j)
{
  N <- length(xi)
  Lj <- 1;
  for (k in 1:N){
    if (j != k){
      Lj <- Lj*(x-xi[k])/(xi[j]-xi[k])
    }
  }
  return(Lj)
}

DiffLj <- function(xi, x, j)
{
  N <- length(xi)
  dx = 1e-6
  Ljp <- LargCoef(xi, x+dx, j)
  Ljm <- LargCoef(xi, x-dx, j)
  dLj = (Ljp - Ljm)/2/dx
}

HermiteInterp <- function(xi, Fi, dFi, x)
{
  N <- length(xi)
  P <- 0;
  for (j in 1:N){
    Lj <- LargCoef(xi, x, j)
    dLj <- DiffLj(xi, x, j)
    Hj = (1-2*(x-xi[j])*dLj)*Lj*Lj
    Hbarj = (x-xi[j])*Lj*Lj
    P = P + Hj*Fi[j] + Hbarj*dFi[j]
  }
  return(P)
}

xi <- 4:7
J0i <- c(-0.3971498099, -0.1775967713, 0.1506452573, 0.3000792705)
J1i <- c(-0.0660433280, -0.3275791376, -0.2766838581, -0.0046828235)
J2i <- c(0.3641281459, 0.0465651163, -0.2428732100, -0.3014172201)
dJ1i <- c()
for (i in 1:length(xi)){
  dJ1i[i] = (J0i[i]-J2i[i])/2
}

plot(xi, J1i, col = "red", ylim = c(-0.6,0), pch = 16)
x <- seq(min(xi), max(xi), by = 0.05)
y <- HermiteInterp(xi, J1i, dJ1i, x)
lines(x,y, col = "black")

y1 <- LargInter(xi, J1i, x)
lines(x, y1, col = "blue")

y2 <- besselJ(x, nu = 1)
lines(x, y2, col = "green")

for (idx in 1:(length(xi)-1)){
  xx <- seq(xi[idx], xi[idx+1], by = 0.05)
  yy <- HermiteInterp(xi[idx:(idx+1)],J1i[idx:(idx+1)],dJ1i[idx:(idx+1)],xx)
  lines(xx,yy, col = "yellow")
}
#TIM HIEU ve noi suy va xap xi ham
#noi suy la suy ra 1 diem nam ben trong du lieu