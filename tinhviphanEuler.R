MyEuler <- function(f, x0, xN, y0, dx =(xN-x0)/10){
#dx =  h trong ct Euler tinh bang cach lay diem bat dau truc X la x0 chia cho den xN
#N la so buoc va day chon la 10 tuc la tu 0 den 10  
  xi <- c()
  yi <- c()
  i <- 1
  xi[1] <- x0
  yi[1] <- y0
  while (xi[i] < xN){
    xi[i+1] <- xi[i] + dx #tinh xi tiep theo bang cach cong xi trc do vs h (dx)
    yi[i+1] <- yi[i] + f(xi[i], yi[i])*dx #ct Euler
    i <- i + 1
  }
  return(data.frame(x = xi, y=yi))
}
f <- function(x,y) {
  return(x^2) #noi viet phuong trinh
}
x0 <- 0
xN <- 1
y0 <- 1
out <- MyEuler(f, x0, xN, y0)
print(out)
plot(out$x, out$y)
lines(out$x, out$x^3/3+1)
#plot(out$x, out$y)
#xi <- seq(0,1, by= 0.01)
#yi <- (xi^3)/3 + 1
#lines(xi,yi)

#doan code cai tien euler
MyEulerImpr <- function(f, x0, xN, y0, dx = (xN-x0)/10)
{
  xi <- c()
  yi <- c()
  i <- 1
  xi[1] <- x0
  yi[1] <- y0
  while (xi[i] < xN){
    xi[i+1] <- xi[i] + dx
    f_start <- f(xi[i], yi[i])
    yi[i+1] <- yi[i] + f_start*dx
    f_end <- f(xi[i+1], yi[i+1])
    yi[i+1] <- yi[i] + (f_start + f_end)/2*dx
    f_err <- 1
    # lặp lại bước tính đạo hàm tại điểm cuối nhiều lần
    # cho đến khi sai số giữa đạo hàm tại điểm cuối mới và cũ < giá trị nào đó
    # i là số lần lặp đến khi hội tụ
    while (f_err > 1e-9){
      f_end_2 <- f(xi[i+1], yi[i+1])
      yi[i+1] <- yi[i] + (f_start + f_end_2)/2*dx
      f_err <- abs(f_end_2 - f_end)
      f_end <- f_end_2
    }
    i <- i+1
  }
  return(data.frame(x=xi, y=yi))
}

f <- function(x, y) {
  return(x^2)
}

x0 <- 0
xN <- 1
y0 <- 1
out <- MyEulerImpr(f, x0, xN, y0)
print(out)
plot(out$x, out$y)
lines(out$x, out$x^3/3+1)
#data.frame(out, Sol = exp(out$x^2/2), Err = exp(out$x^2/2) - out$y, ErrRel = (exp(out$x^2/2) - out$y)/exp(out$x^2/2))

#phuong phap euler cai tien (phuong phap Heun )
ImprovedEuler <- function(f, x0, xN, y0, h =(xN-x0)/10) {
  # Initialize the vectors for x and y values
  x <- c()
  y <- c()
  
  
  xi <- x0
  yi <- y0
  while (xi < xN) {
    k1 <- f(xi, yi)
    y_pred <- yi + h * k1 #yi + 1
    k2 <- f(xi + h, y_pred) #xi + 1, yi + 1
    
    yi <- yi + (h / 2) * (k1 + k2) #ct 
    xi <- xi + h
    
    # Append new xi and yi to the vectors
    x <- c(x, xi) 
    y <- c(y, yi)
  
  }
  return(data.frame(x = x, y = y))
}
f <- function(x, y) {
  return(x^2)
}
x0 <- 0
xN <- 1
y0 <- 1

result <- ImprovedEuler(f, x0, xN, y0)
print(result)

plot(result$x, result$y)
lines(result$x, (result$x^3)/3 + 1)
