#cach cua thay
ti = seq(0,1,by=0.1);
vi = c(-0.10290, 0.37364, 2.43748, 3.93836, 3.31230, 5.49472, 5.43325, 6.39321, 9.06048, 9.36416, 9.52066)
plot(ti,vi)

# tinh toan cac yeu to de thiet lap phuong trinh 
V <- matrix(nrow = 2, ncol = 2)
Y <- vector()
V[1,1] <- length(ti); V[1,2] <- sum(ti); V[2,1] <- sum(ti); V[2,2] <- sum(ti*ti)
Y[1] <- sum(vi); Y[2] <- sum(vi*ti)

Coe <- solve(V,Y)
Coe
t <- seq(0,1,by=0.01)
v <- Coe[1] + Coe[2]*t
lines(t,v)
