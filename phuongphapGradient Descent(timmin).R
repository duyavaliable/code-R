GradDsc <- function(fp, x, h = 1e-3,tol = 1e-6  ,m = 1e5){
#h la kich thuoc buoc, muc do thay x sau moi lan lap (learning rate)
#tol la sai so, m la so lan lap 
  iter <- 0
  oldx <- x #gia tri cua x tu lan lap trc do
  x = x - h * fp(x)
  while (abs(x - oldx) > tol){
    iter <- iter + 1
    if( iter > m)
      stop ("No solution found")
    oldx <- x
    x = x - h * fp(x)
  }
  print(iter)
  return(x)
}
f <- function(x) {sin(x)}

# Sử dụng hàm GradDsc với hàm sin(x)
result <- GradDsc(fp, x = 2.5)

# In kết quả
print(result)

curve(sin(x), from  = -2*pi , to = 2*pi)
points(result, f(result), col = "red", pch = 19)

# Vẽ biểu đồ thể hiện
x_values <- seq(-2*pi, 2*pi, length.out = 100)
y_values <- f(x_values)
plot(x_values, y_values, type = "l", col = "blue", lwd = 2,
     main = "Graph of f(x) = sin(x)",
     xlab = "x", ylab = "f(x)")
grid()
points(result, f(result), col = "red", pch = 16)
#su dung curve
curve(sin(x), from  = -2*pi , to = 2*pi)
points(result, f(result), col = "red", pch = 19)