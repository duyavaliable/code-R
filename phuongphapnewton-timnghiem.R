mynewton <- function(f, fp, x, tol = 0.001 , m = 100){# f la ham so ban dau,
#fp la dao ham cua ham so ban dau, tol la sai so cho phep, m la so lan lap toi da
  iter <- 0
  err <- 10*tol # khoi tao bien sai so bang 10 lan sai so cho phep
#(co the la phep tinh bat ky mien sao > tol)
  while (err > tol){
    iter <- iter + 1
    xnew <- x - f(x) / fp(x) # cap nhat gia tri moi cua x sau moi lan lap theo cong thuc
    if (iter > m)
      stop("NO solution found after", m, "loops")
    err <- abs(xnew - x ) # cap nhat err de kiem tra xem co < tol ko neu ko 
  #-> tuc x chua hoi tu den nghiem vs do chinh xac mong muon (hay sai so cho phep)
    x <- xnew
  }
  return(x)
}
f <- function(x) {cos(x) - x}
fp <- function(x) {-sin(x) - 1}
x <- 1
mynewton(f, fp, x)
