myBisect2 <- function (f, a, b, tol = 1e-6, m =100){
  iter <- 0
  f.a <- f(a)
  f.b <- f(b)
  while (abs(b - a)/ max(abs(c(a,b))) > tol ){
    iter <- iter + 1
    if (iter > m){
      warning("iterations maxium exeeded")
      break
    }
    xmid <- (a + b)/2
    ymid <- f(xmid)
    if (f.a * ymid > 0){
      a <- xmid 
      f.a <- ymid
    }
    else {
      b <- xmid 
      f.b <- ymid
    }
  }
  root <- (a + b)/2
  return(root)
}
f <- function(x) {
  return (cos(x) - x)
}
root <- myBisect2(f, 0, 1)
print(root)
