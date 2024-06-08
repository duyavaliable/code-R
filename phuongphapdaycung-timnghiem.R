# Định nghĩa phương pháp dây cung
secant <- function(f, x0, x1, tol = 1e-16, m = 100) 
  #x1,x2 tuong tu nhu a,b trong phuong phap chia doi.Tuy nhien x0, x1 ko co dinh va 
  #dc cap nhat sau moi lan lap 
  #tol la sai so cho phep, m la so lan lap toi da
{
  iter <- 0 # so lan lap 
  err <- 10*tol # khoi tao sai so 
  #Iters <- c()
  #Errs <- c()
  #Iters va Errs de luu tru du lieu de so sanh voi tol 
  while (err > tol) {
    iter <- iter + 1
    if (iter > m){
      warning("iterations maximum exceeded")
      break
    }
    xnew <- x1 - f(x1)*(x1 - x0) / (f(x1) - f(x0))
    err <- abs(xnew - x1)
 #   Iters[iter] = iter;
  #  Errs[iter] = err;
  #cap nhat x0, x1  
    x0 <- x1 
    x1 <- xnew
  }
  #hien thi so lan lap va cac sai so (so sanh do hoi tu)
  #out <- list(Iters, Errs) # khoi tao bien luu tru ket qua danh sach lap
  #return(out) # tra ve bien luu tru ket qua danh sach la
  return(x1)
}

# Định nghĩa hàm số
f <- function(x) {sin(x) - x/2}

# Gọi hàm secant với hàm số f, x0 = 0 và x1 = 1
root <- secant(f, pi/2, pi)

# In giá trị của root
print(root)
#out1 <- secant(f, pi/2, pi)
#ve bieu do hoi tu
#plot(out1[[1]], log(out1[[2]]), type = 'o')
#plot(out1[[1]], out1[[2]], type = 'o')
curve(sin(x)- x/2, from = pi/2, to = pi)
points(root, f(root), col = "red", pch = 19)

