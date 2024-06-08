goldesactmax <- function(f,a,b,tol =1e-15, m = 100){
  iter <- 0
  phi <- (sqrt(5)-1)/2 #con thuc tinh GD
  a.star <- b - phi*abs(b-a)
  b.star <- a + phi*abs(b -a)
  while (abs(b -a) > tol){
    iter <- iter + 1
    if (iter > m) {
      warning("iteractions maxium exceeded")
      break
    }
    if (f(a.star)> f(b.star)){
      b <- b.star
      b.star <- a.star
      a.star <- b - phi*abs(b-a)
    }else{
      a <- a.star
      a.star <- b.star
      b.star <- a + phi * abs(b-a)
    }
  }
print(iter)
return((a+b)/2)
}
f <- function(x) {-(x^2 - 2*x + 3)}
result <- goldesactmax(f, 0, 4, tol = 1e-15)
print(result)

#ve diem roi noi duong thang 
xi <- 4:7
Fi <- c(-0.0660433280, -0.3275791376, -0.2766838581, -0.0046828235)
plot(xi, Fi, x)
plot(xi, Fi, col="red", ylim = c(-0.35, 0.0))
x <- seq(min(xi), max(xi), by=0.05)
y <- LargInter(xi, Fi, x)
print(y)
