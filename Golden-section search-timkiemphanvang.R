goldensection = function(f, a,b, tol){
  gr = (sqrt(5) - 1)/2
  
  x1 = b - gr*abs(b - a) #a.star
  x2 = x1 + gr*abs(b-x1) #b.star
  
  #gan ham
  f1 = f(x1)
  f2 = f(x2)
  
  #khoi tao vong lap
  while (abs(b-a) > tol){
    if (f1 > f2){
      a = x1 #x1 becomes new b
      x1 = x2 #x2 becomes new x1
      x2 = a + gr*abs(b-a)
    }else {
      b = x2
      x2 = x1
      x1 = b - gr*abs(b-a)
    }
  }
  return ((a+b)/2)
}
f = function(x) {x*x - 6*x + 15}
result = goldensection(f, a= 0, b = 4, tol = 1e-100)
print(result)
