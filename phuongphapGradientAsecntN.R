GradAscN <- function(fp, x, h = 1e-3, tol = 1e-6, m = 1e5){
  iter <- 0
  oldx <- x
  x = x + h * fp(x)
  dx <- 1e-6
  while (abs(x - oldx) > tol) {
    iter <- iter + 1
    if( iter > m)
      stop ("No solution found")
    oldx <- x
    h <- 1/abs( (fp(x + dx) - fp(x - dx) )/ (2*dx) )
    x = x + h * fp(x)
  }
  print(iter)
  return(x)
} 
f <- function(x) {sin(x)}
fp <- function(x) {cos(x)}

result <- GradAscN(fp, x = 2.5) #ket qua la so lan lap de ra ket qua 
print(result)

#bieu do the hien su hoi tu

