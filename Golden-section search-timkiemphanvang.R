goldensection = function(f, a,b, tol){
  gr = (sqrt(5) + 1)/2
  
  d = gr*(b-a)
  x1 = a + d
  x2 = b -d
  
  #gan ham
  f1 = f(x1)
  f2 = f(x2)
  
  #khoi tao vong lap
  while (abs(b-a) > tol){
    if (f1 > f2){
      b =x1 #x1 becomes new b
      x1 = x2 #x2 becomes new x1
      f1 = f2
      x2 = b -d
      f2 = f(x2)
    }else {
      a = x2
      f1 = f2
      x2 = x1
      x1 = a + d
      f2 = f(x1)
    }
    d = gr*(b-a)
  }
  return ((a+b)/2)
}
f = function(x) {x*x - 6*x + 15}
result = goldensection(f, a= 0, b = 6, tol = 1e-16)
print(result)
#chua xong code van chua hoan thien van xay ra loi
