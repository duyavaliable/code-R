#bai tap vn 2
a <- rnorm(10, 100, 15)
a

time <- c(0.00, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1.00)
velocity <- c(-0.10290, 0.37364, 2.43748, 3.98386, 3.31230, 5.49472, 5.43325, 6.39321, 9.06048, 9.36416, 9.52066)

# Tạo biểu đồ
plot(time, velocity,
     xlab = "Time",
     ylab = "Velocity",
     main = "Veclocity and time",
     ylim = c(0,10))

#bai 1
# Nhập dữ liệu
ti = seq(0,1,by=0.1);
vi = c(-0.10290, 0.37364, 2.43748, 3.93836, 3.31230, 5.49472, 5.43325, 6.39321, 9.06048, 9.36416, 9.52066)
# Minh họa dữ liệu va ve bieu do
plot(ti,vi)

# Thiết lập hệ phương trình
n <- 2
V <- matrix(nrow = n+1, ncol = n+1)
Y <- vector()
for(i in 0:n)
{
  for(j in 0:n)
  {
    V[i+1, j+1] <- sum(ti^(j+i))
  }
  Y[i+1] <- sum(vi*ti^i)
}
# Giải hệ phương trình ở đây
Coe <- solve(V,Y)


# Vẽ đường thẳng
t <- seq(0,1,by=0.01)
v <- Coe[1]
for( i in 1:n)
{
  v <- v + Coe[i+1]*t^i
}
lines(t,v)
model <- lm(vi ~ ti)
summary(model)
#RSS = vi - (a+b*i)
#chi2 <- sum((vi-a-b*ti)^2)
