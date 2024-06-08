GradAsc <- function(fp, x , h = 1e-3, tol = 1e-6, m = 1e5){
  iter <- 0
  oldx <- x
  x = x + h*fp(x)
  while (abs(x - oldx) > tol ){
    iter <- iter + 1
    if (iter > m)
      stop ("No solution found")
    oldx <- x
    x = x + h*fp(x) #tim gia tri cuc dai
  }
  print(iter)
  return (x)
}
f <- function(x) {sin(x)}
fp <- function(x) {cos(x)}
 
result <- GradAsc(fp, x = 2.5, m = 1e5) #hien thi so lan lap
print(result)
