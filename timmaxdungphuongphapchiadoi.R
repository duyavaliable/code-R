#tim max bang phuong phap chia doi (toi uu code )
bisctmax  <- function(f, a, b, tol = 1e-6, m = 100){
  iter <- 0 
  a.star <- a + 0.5*(b - a)
  b.star <- a + 0.75*(b - a)
  while (abs(b - a) > tol){
    iter <- iter + 1
    if (iter > m){
      warning("Iteration maximum exceeded")
      break
    }
    if (f(a.star) > f(b.star)){
      b <- b.star
    }else{
      a <- a.star
    }
    a.star <- a + 0.5 * abs(b -a)
    b.star <- a + 0.75 * abs(b- a)
  }
  print(iter)
  return((a+b)/2)
}

#tinh toan
f2 <- function(x){-x^2+4*x+1}
curve(f2,1, 3)
bisctmax(f2,1, 3,tol = 1e-9)

f3 <- function(x){sin(x) + 1}
curve(f3,0, pi)
bisctmax(f3, 1, 3,tol = 1e-9)

f4 <- function(x) {x^2-1}
curve(f4,1,3)
bisctmax(f4, 1, 3, tol = 1e-09)
