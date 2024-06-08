LargCoef <- function(xi,x,j)
  #xi la danh sach cac diem du lieu goc 
{
  N <- length(xi)
  Lj <- 1;
  for (k in 1 : N){
    if (j != k){
      Lj <-  Lj* (x-xi[k]) / (xi[j]-xi[k]) 
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
xi <-  -1 : 9
Fi <- c(2.2177, 0.954331, 0.176641, -1.83642,-1.99559,-2.90529,-3.45014,-4.24675,-5.60628,-6.89665,-7.88336) #Fi = y tuc vi du ve 4 diem khi thay 
#vao ham ra ket qua 
plot(xi, Fi, col = "red")
x <- seq(min(xi), max(xi), by = 1)
y <- LargInter(xi, Fi, x)
print(y) #la khi minh thay x chay tu 0 den 3 co khoang cach la 0.1 o moi diem 
points(x,y)
lines(x,y)

