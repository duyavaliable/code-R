library(ggplot2)
MultTrapeInt <- function(fun, a, b, tol){
#a: gioi han duoi cua khoang tich phan, b: gioi han tren, tol: sai so mong muon  
  N <- 1 #so luong diem tich phan bd 
  h <- b - a #khoang cach giua cac diem tich phan 
#gia tri xap xi ban dau cua tich phan 
  Integ1 <-  h/2*(fun(a)+fun(b)) #dc tinh bang cong thuc hinh thang 
  print(Integ1)   
#cai thien xap xi tich phan trong moi lan lap cua vong lap while, bang cach: 
  N <- 2*N #tang gap doi so luong diem tich phan 
  h <- h/2 #giam moi buoc tich phan di 1 nua (khoang cach giua cac diem di 1 nua)
  xj <- (a+h) #cap nhap gia tri xj la diem tiep theo can tinh de tinh toan xap xi moi cua tich phan
#diem xj la diem giua  
  Integ2 <- Integ1/2 + h*fun(xj) #su dung cong thuc hinh thang vs du lieu gia tri xapxi
  
#ban dau de tinh ra gia tri xap xi moi
  Err <- abs(Integ2 - Integ1) #sai so giua tich phan moi va cu de ktra dk vs tol
  while (Err > tol) {
    N <- 2*N
    h <- h/2
    Integ1 <- Integ2
    Integ2 <- Integ1/2
    for (j in seq(1, N-1, by = 2)){
      xj <- a + j*h
      Integ2 <- Integ2 + h*fun(xj)
    }
    Err <- abs(Integ2 - Integ1)
    print(N)
  }
  return(Integ2)
}
f <- function(x) {sin(x)}
result <- MultTrapeInt(f, 0, 1, 0.001)
print(result)
  
# Vẽ đồ thị hàm sin(x) và các đoạn hình thang
x_values <- seq(0, 1, length.out = 100)
y_values <- f(x_values)

ggplot() +
  geom_line(aes(x = x_values, y = y_values), color = "blue") +
  geom_segment(aes(x = x_values, y = 0, xend = x_values, yend = y_values), color = "red", alpha = 0.5) +
  labs(x = "x", y = "f(x)", title = "Graph of f(x) = sin(x) and Trapezoidal Rule") +
  theme_minimal()

