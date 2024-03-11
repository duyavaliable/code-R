#phuong phap chia doi
myBiset <- function(f, a, b, tol = 1e-15, m = 100)
{
  iter <- 0
  f.a <- f(a)
  f.b <- f(b)
  err <- 10*tol
  Iters <- c()
  Errs <- c()
  
  
  while (err > tol){ 
    iter <- iter + 1 
    err <- abs(b -a)
    Iters[iter] = iter;
    Errs[iter] = err;
    if (iter > m){
      warning("iterations maximum exceeded")
      break
    }
    
    
    xmid <- (a+b)/2
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
  root <- (a+ b)/2
  out <- list(Iters, Errs)
  return(out)
}
myBiset(function(x){cos(x)- x},0,1,tol = 1e-9)-> out1
plot(out1[[1]], log(out1[[2]]), type = 'o')#do thi cua sai so tuyet doi
plot(out1[[1]], out1[[2]], type = 'o')

#phuong phap newton
myNewtonRaphson <- function(f, fp, x, tol = 1e-15, m = 100)
{
  iter <- 0
  err <- 10*tol
  Iters <- c()
  AbsErrs <- c()
  
  while (err > tol){ 
    iter <- iter + 1 
    xnew <- x - f(x) / fp(x)
    err <- abs(xnew - x)
    Iters[iter] = iter;
    AbsErrs[iter] = err;
    if (iter > m){
      warning("iterations maximum exceeded")
      break
    }
    x <- xnew
  }
  out <- list(Iters, AbsErrs)
  return(out)
}
myNewtonRaphson(function(x){cos(x)- x}, function(x){-sin(x) - 1}, 0.5, tol = 1e-9)-> out2
plot(out2[[1]], log(out2[[2]]), type = 'o')#????
plot(out2[[1]], out2[[2]], type = 'o')
