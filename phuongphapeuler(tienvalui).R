MyEuler <- function(f, x0, xN, y0, dx =(xN-x0)/10){
  
  xi <- c()
  yi <- c()
  i <- 1
  xi[1] <- x0
  yi[1] <- y0
  while (xi[i] < xN){
    xi[i+1] <- xi[i] + dx 
    yi[i+1] <- yi[i] + f(xi[i], yi[i])*dx 
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

#lui
MyEuler <- function(f, x0, xN, y0, dx =(xN-x0)/10){
  
  xi <- c()
  yi <- c()
  i <- 1
  xi[1] <- x0
  yi[1] <- y0
  while (xi[i] < xN){
    xi[i+1] <- xi[i] + dx 
    yi[i+1] <- yi[i] + f(xi[i], yi[i])*dx 
    yi[i+1] <- yi[i] + f(xi[i + 1], yi[i+1]) *dx
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
