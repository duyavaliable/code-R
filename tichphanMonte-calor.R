MCInt1D <- function(fun,a,b,N){
  x <- runif(N, min = a, max = b)
  Averfun <- sum(fun(x))/N
  Integ <- (b-a)*Averfun
  
  Averfun2 <- sum(fun(x)^2)/N
  Sig <- sqrt((Averfun2 - Averfun^2)/(N-1))
  print(c(Integ, Sig), digits = 4)
  
  return(Integ)
}
MCInt1D(fun = sin,a =0,b=pi,100)
MCInt1D(fun = sin,a =0,b=pi,1000)
MCInt1D(fun = sin,a =0,b=pi,10000)
curve(expr = x^(-1/3)+x/10)
#monte carlo voi 3 lop  (tich phan nhieu lan)
MCInt3D <- function(fun, a, b, c, d, N) {
  x <- runif(N, min = a, max = b)  
  y <- runif(N, min = c, max = d)  
  
  Averfun <- sum(fun(x, y)) / N  
  Integ <- (b - a) * (d - c) * Averfun  
  
  Averfun3 <- sum(fun(x, y)^2) / N  
  Sig <- sqrt((Averfun - Averfun^2) / (N - 1))  
  
  print(c(Integ, Sig), digits = 4)  
  
  return(Integ)  
}

MCInt3D(function(x, y) sin(x), a=0, b=pi, c=0, d=1, 100)
MCInt3D(function(x, y) sin(x), a=0, b=pi, c=0, d=1, 1000)
#monte carlo vs 2 lop
MCInt2D <- function(fun, a, b, c, d, N) {
  x <- runif(N, min = a, max = b)  
  y <- runif(N, min = c, max = d)  
  Averfun <- sum(fun(x, y)) / N  
  Integ <- (b - a) * (d - c) * Averfun 
  Averfun2 <- sum(fun(x, y)^2) / N  
  Sig <- sqrt((Averfun2 - Averfun^2) / (N - 1))  
  print(c(Integ, Sig), digits = 4)  
#integ la gia tri xap xi cua tich phan 
  #Đây là ước lượng trung bình của hàm sin(x) * cos(y) trên hình chữ nhật [0, pi] x [0, 1], 
  #nhân với diện tích của hình chữ nhật đó.
#Sig la do lech chuan cua uoc luong tich phan
  return(Integ)  
}
MCInt2D(function(x, y) exp(-(x^2 + y^2)), -1, 1, -1, 1, 100000)



#tinh tich phan e^-x^2 tu -vo vung den +vo cung, tol = 1e^-6
#(chua dung)
MCIntGaussian <- function(fun, N) {
  x <- rnorm(N)  
  Averfun <- sum(fun(x)) / N
  Integ <- Averfun * sqrt(pi)  
  Averfun2 <- sum(fun(x)^2) / N   
  Sig <- sqrt((Averfun2 - Averfun^2) / (N - 1)) 
  print(c(Integ, Sig), digits = 6)  
  return(Integ) 
}
fun <- function(x) exp(-x^2)
result <- MCIntGaussian(fun, 1000)
