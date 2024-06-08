xi <- 0:10
yi <- pi + xi + 0.2*xi^2 - 0.1*xi^3 +rnorm(1, mean = 0, sd =1)

A <- matrix(nrow = 4, ncol = 4)
b <- vector()
A[1,1] <- length(xi); A[1,2] <- sum(xi); A[1,3] <- sum(xi^2); A[1,4] <- sum(xi^3)
A[2,1] <- sum(xi); A[2,2] <- sum(xi^2); A[2,3] <- sum(xi^3); A[2,4] <- sum(xi^4)
A[3,1] <- sum(xi^2); A[3,2] <- sum(xi^3); A[3,3] <- sum(xi^4); A[3,4] <- sum(xi^5)
A[4,1] <- sum(xi^3); A[4,2] <- sum(xi^4); A[4,3] <- sum(xi^5); A[4,4] <- sum(xi^6)

b[1] <- sum(yi); b[2] <-sum(xi*yi) ; b[3] <- sum(xi^2*yi) ; b[4] <- sum(xi^3*yi)

Coe <- solve(A,b)
print(solve(A,b)) #ra he so cua phuong trinh
plot(xi,yi)
x <- seq(0, 10, by =0.1)
lines(x, Coe[1] + Coe[2]*x + Coe[3]*x^2 + Coe[4]*x^3, col = 'red')
