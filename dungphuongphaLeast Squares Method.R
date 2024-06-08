# Ve bieu do tu du lieu
ti <- seq(0, 1, by = 0.1)
vi <- c(-0.10290, 0.37364, 2.43748, 3.93836, 3.31230, 5.49472, 5.43325, 6.39321, 9.06048, 9.36416, 9.52066)

# so luong diem du lieu
n <- length(ti)

# Tinh tong cac gia tri
sum_ti <- sum(ti)
sum_vi <- sum(vi)
sum_ti_squared <- sum(ti^2)
sum_ti_times_vi <- sum(ti * vi)
sum_ti_times_sum_vi <- sum_ti * sum_vi

# Tinh he so a (do doc)
a <- (n * sum_ti_times_vi - sum_ti_times_sum_vi) / (n * sum_ti_squared - sum_ti^2)

# Tinh b (chan)
b <- (sum_vi - a * sum_ti) / n


paste("He so  a:", a, "\n")
paste("He so b:", b, "\n")

# Tinh gia tri y tu pt
y <- a * ti + b

# Ve bieu do
plot(ti, vi, type = "p",  pch = 16, xlab = "Time", ylab = "Velocity")
lines(ti, y)
model <- lm(vi ~ ti)
summary(model)


chi2 <- sum((vi - a - b*ti)^2)
sqrt(sum((vi - a - b*ti)^2)/(length(ti)-2)/sum((ti-mean(ti))^2)) -> sig_b
sig_b*sqrt(mean(ti^2)) -> sig_a
sig_a
sig_b
#1Q va 3Q la goc phan tu thu 1 va thu 3
#do lech chuan cua he so a va b
# Tính độ lệch chuẩn của hệ số a

#code cua tuan 
#dat ten so lieu va ve bieu do
ti <- seq(0,1,by=0.1);
vi <- c(-0.10290, 0.37364, 2.43748, 3.93836, 3.31230, 5.49472, 5.43325, 6.39321, 9.06048, 9.36416, 9.52066)
plot(ti,vi)

V <- matrix(nrow = 2, ncol = 2)
Y <- vector()
V[1,1] <- length(ti)
V[1,2] <- sum(ti)
V[2,1] <- sum(ti) #ma tran chuyen vi cua ti hay x gach
V[2,1]
V[2,2] <- sum(ti * ti) #tong binh phuong ti
Y[1] <- sum(vi)
Y[2] <- sum(vi*ti)

coe <- solve(V,Y) 
tmp1 <- -V[1,1] / V[2,1] * V[2,2] + V[1,2]
tmp1
tmp2 <- -V[1,1] / V[2,1] * Y[2] + Y[1]
tmp2

b <- tmp2/tmp1
b
a <- (Y[1] - b*V[1,2]) / V[1,1]
a
t <- seq(0,1,by = 0.01)
v <- coe[1] + coe[2]*t
coe[1]
lines(t,v)

out <- lm(vi ~ ti)
summary(out)
#tinh chi-square
chi2 <- sum((vi - a - b*ti)^2)

sqrt(sum((vi - a - b*ti)^2)/(length(ti)-2)/sum((ti-mean(ti))^2)) -> sig_b
sig_b*sqrt(mean(ti^2)) -> sig_a
sig_a
sig_b

#???
#as va bsla 2 vecto 
as = c()
bs = c()
#hoi quy tuyen tinh
for (k in 1:1e3)
{
  idx <- sample(1:length(ti), replace = T)
  ts <- ti[idx]
  vs <- vi[idx]
  bs[k] <- cov(ts,vs)/var(ts)
  as[k] <- mean(vs) - b*mean(ts)
}
mean(as)
sd(as)
mean(bs)
sd(bs)
hist(as)
hist(bs)
#??/
t <- seq(0,1,by=0.1)
for (k in 1:length(as))
{
  v <- as[k] + bs[k]*t
  lines(t,v)
}

#Giả sử y = f(x) = P(x) là 1 đa thức bậc 3, hãy tìm giá trị các hệ số của đa thức P(x) phù hợp nhất với dữ liệu đã cho
# Dữ liệu
xi <- 0:10
yi <- c(2.8888718, 3.9888718, 4.8888718, 4.9888718, 3.6888718, 0.3888718, -5.5111282, -14.6111282, -27.5111282, -44.8111282, -67.1111282)

# Ma trận Vandermonde
X <- cbind(1, xi, xi^2, xi^3)

# Giải hệ phương trình
coefficients <- solve(t(X) %*% X, t(X) %*% yi)

# Hiển thị các hệ số
coefficients

