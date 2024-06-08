#muon ket qua nghiem chinh xac hon ta giam tol
#khong che sai so cua so va ham so 
#co the dan toi ket qua khong chinh xac va ko hoi tu
myBiset <- function(f, a, b, tol = 1e-15, m = 100)
#tol la sai so chap nhan dc trong khoang a,b. tol cang nho thi do chinh xac cang cao
#m la so lan lap toi da
{
  iter <- 0 #so lan lap
  f.a <- f(a)
  f.b <- f(b)
  #trong dk vong lap while la cong thuc sai so tuyet doi
  #convergence <- c()
  
#b3+ b4: thu hep khoang phan ly cho den khi no nho hon sai so

  while (abs(b - a) > tol){#b-a la do lon cua khoang chua nghiem, neu b -a > tol tuc la 
  #khoang chua nghiem so vs sai so con lon va van can tim nghiem hay van can lap tiep
    iter <- iter + 1#sau moi lan lap inter tang len 
    if (iter > m){
      warning("iterations maximum exceeded")
      break
    }

#B2    
    xmid <- (a+b)/2#diem o giua ( duoc hieu xmid = c)
    ymid <- f(xmid)#ham o giua ( ymid = f(c))
  #so sanh dau cua ham so tai gia tri tb cua khoang phan ly nghiem vs dau cua ham so 
    if (f.a * ymid > 0){#f.a la ve trai (hay a = c)
      a <- xmid 
      f.a <- ymid
    }
    else { #f.b ve trai hay ( b = c)
      b <- xmid
      f.b <- ymid 
    }
#luu tru gia tri cua a, b sau moi lan lap
  #convergence <- c(convergence, abs(b-a))
  }
  root <- (a+ b)/2
  return (root)
  #return (list(root = root, convergence = convergence))
}
#Dinh nghia ham so 
f <- function(x) {
  return (sin(x) - x/2)
}

# Goi ham myBiset với hàm số f, khoảng [a, b] hay la khoang phan ly nghiem = [1, 2]
root <- myBiset(f, pi/2, pi)

# In giá trị của root
print(root)#check xem ket qua dung khong 

curve(sin(x) - x/2, from  = pi/2, to = pi) #do thi sinx co gioi han
points(root, f(root), col = "red", pch = 19)
curve(sin(x) - x/2)#do thi sinx  ko gioi han



#bieu do phuong phap hoi tu ta dung convergence de luu tru gia tri cua a, b sau moi lan lap
plot(root$convergence, type = "l", main ="Toc do hoi tu cua phuong phap chia doi",
     xlab = "so lan lap", ylab = "abs(b - a)")
#khi muon bieu dien toc do hoi tu chuyen return(root) thanh return(list...)
