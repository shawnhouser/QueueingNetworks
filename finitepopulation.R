################################################################################
# Finite Population Single Server Model (N >= 1)
################################################################################

lambda <- 0.025
mu <- 1

a <- lambda / mu
N <- 4
N_fact <- factorial(N)
bottleneck <- matrix(1, nrow = N+1, ncol = 1)
for (n in 1:N) {
   print(N-n+1)
   bottleneck[n+1,1] <- bottleneck[n, 1]*(N-n+1)*a
}
p0 <- (1/sum(bottleneck))
p0
pn <- c(bottleneck*p0)

sum(pn)
print(pn)

L_q <- N - ((lambda + mu)/lambda)*(1-p0)
L_q
L <- N - (mu/lambda)*(1-p0)
L
W <- N/(mu*(1-p0)) - 1/lambda
W
W_q <- L_q/(lambda*(N-L))
W_q
lambda_eff <- mu*(1-p0)
lambda_eff

################################################################################
# how to fix this?
factorial(200)

# 'Multiple Precision Floating-Point Reliably'
# Pro: we can do this for very large N
# Con: we have to depend on an external package that may change in the future
install.packages('Rmpfr')
library(Rmpfr)

N <- 300
N_fact <- factorialZ(N)

bottleneck <- mpfr(matrix(1, nrow = N+1, ncol = 1), 300)
for (n in 1:N) {
   bottleneck[n+1,1] <- bottleneck[n, 1]*mpfr((N-n+1)*a, 300)
}
p0 <- (1/sum(bottleneck))
pn <- c(bottleneck*p0)

sum(pn)
print(pn)

L_q <- as.numeric(N - ((lambda + mu)/lambda)*(1-p0))
L_q
L <- as.numeric(N - (mu/lambda)*(1-p0))
L
W <- as.numeric(N/(mu*(1-p0)) - 1/lambda)
W
W_q <- as.numeric(L_q/(lambda*(N-L)))
W_q
lambda_eff <- as.numeric(mu*(1-p0))
lambda_eff


















