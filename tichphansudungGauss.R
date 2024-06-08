#vs 2 diem 
GaussLegendreInt <- function(fun ,a, b,tol){
# a, b la gioi han duoi va tren cua tich phan
#fun la tich phan
 J <- (b-a)/2 # he so cua doi bien chuyen doi phep tich phan tu khoang a,b sang khoang [-1,1]
#x = a + (b-a)/2*(y+1) = (b+a)/2 + (b-a)/2*y
#dx = (b-a)/2*dy --> J = dx/dy = (b-a)/2
 coe <- 1/sqrt(3) #gia tri cua he so 
 p1 <- (b+a)/2 + (b-a)/2*(-coe) #gia tri 
 p2 <- (b+a)/2 + (b-a)/2*(coe)
 
 Integ <- J*(fun(p1) + fun(p2)) #ta phai nhan lai vs J de bao toan do độ đo 
 #(độ đo do su thay doi khi thay doi a,b thi khoang cach cua chung thay doi vi vay phai bao toan)
 #vd [-5,5] -> [-1,1] thi khoang cach 1 la 5  nhung khoang cach 2 la 1
 return(Integ)
}
GaussLegendreInt(sin, 0, pi)

#co sd sai so 
GaussLegendreInt <- function(fun ,a, b, tol){
  J <- (b-a)/2
  coe <- 1/sqrt(3)
  
  # Tính toán giá trị ban đầu của phép tích phân
  p1 <- (b+a)/2 + (b-a)/2*(-coe)
  p2 <- (b+a)/2 + (b-a)/2*(coe)
  Integ_old <- J*(fun(p1) + fun(p2))
  
  # Khởi tạo biến để theo dõi sự thay đổi giữa các lần tính toán
  delta <- tol
  
  # Số lượng đoạn chia
  N <- 1
  
  while (delta >= tol) {
    # Tăng số lượng đoạn chia
    N <- 2*N
    h <- (b - a) / N
    Integ_new <- 0
    
    for (j in seq(0, N-1)){
      x1 <- a + j*h
      x2 <- a + (j+1)*h
      Integ_new <- Integ_new + GaussLegendreInt2(fun, x1, x2)
    }
    
    # Tính toán sự thay đổi giữa hai lần tính toán
    delta <- abs(Integ_new - Integ_old)
    
    # Cập nhật giá trị cũ của phép tích phân
    Integ_old <- Integ_new
  }
  
  return(Integ_old)
}

GaussLegendreInt2 <- function(fun ,a, b){
  J <- (b-a)/2
  coe <- 1/sqrt(3)
  p1 <- (b+a)/2 + (b-a)/2*(-coe)
  p2 <- (b+a)/2 + (b-a)/2*(coe)
  Integ <- J*(fun(p1) + fun(p2))
  return(Integ)
}
GaussLegendreInt(sin, 0, 1,1e-6)


#vs 3 diem 
GaussLegendreInt3 <- function(fun, a, b) {
  J <- (b - a) / 2
  coe <- -sqrt(3 / 5)#trong so 
#cac diem w0 = 8/9 , w1 =5/9
  
  p1 <- (b + a) / 2 + (b - a) / 2 * (-coe)
  p2 <- (b + a) / 2
  p3 <- (b + a) / 2 + (b - a) / 2 * (coe)
  
  Integ <- J * (5 / 9 * fun(p1) + 8 / 9 * fun(p2) + 5 / 9 * fun(p3))
  return (Integ)
}

GaussLegendreInt3(sin, 0, pi)

#vs 4 diem 
GaussLegendreInt4 <- function(fun, a, b) {
  J <- (b - a) / 2
  coe <- sqrt(3 / 7 + 2 / 7 * sqrt(6 / 5))
  coe2 <- sqrt(3 / 7 - 2 / 7 * sqrt(6 / 5))
  p1 <- (b + a) / 2 + (b - a) / 2 * (-coe)
  p2 <- (b + a) / 2 + (b - a) / 2 * (-coe2)
  p3 <- (b + a) / 2 + (b - a) / 2 * (coe2)
  p4 <- (b + a) / 2 + (b - a) / 2 * (coe)
  Integ <- J * ((18 + sqrt(30)) / 36 * fun(p1) + (18 - sqrt(30)) / 36 * fun(p2) +
                  (18 - sqrt(30)) / 36 * fun(p3) + (18 + sqrt(30)) / 36 * fun(p4))
  return(Integ)
}

GaussLegendreInt4(sin, 0, pi)

#vs 5 diem 
GaussLegendreInt5 <- function(fun, a, b) {
  J <- (b - a) / 2
  coe <- sqrt(5 + 2 * sqrt(10 / 7)) / 3
  coe2 <- sqrt(5 - 2 * sqrt(10 / 7)) / 3
  p1 <- (b + a) / 2 + (b - a) / 2 * (-coe)
  p2 <- (b + a) / 2 + (b - a) / 2 * (-coe2)
  p3 <- (b + a) / 2
  p4 <- (b + a) / 2 + (b - a) / 2 * (coe2)
  p5 <- (b + a) / 2 + (b - a) / 2 * (coe)
  Integ <- J * ((128 / 225) * fun(p1) + (322 + 13 * sqrt(70)) / 900 * fun(p2) +
                  128 / 225 * fun(p3) + (322 - 13 * sqrt(70)) / 900 * fun(p4) +
                  128 / 225 * fun(p5))
  return(Integ)
}

GaussLegendreInt5(sin, 0, pi)

