################################################################################
# Closed Jackson Networks
################################################################################
# find v given the routing matrix
R = matrix(c(0, 0.75, 0.25,
             0.6666666667, 0, 0.3333333333, 
             1, 0, 0), nrow = 3, ncol = 3, byrow = TRUE)
n = nrow(R)
m = ncol(R)
I = diag(x = 1, nrow = n, ncol = m)
Q = R - I
Q = cbind(Q, rep(1, n))
b = matrix(c(rep(0, n), 1), nrow = 1, ncol = (m+1))
v = (b%*%t(Q))%*%solve(Q%*%t(Q))
################################################################################
# Convolution
# solution to vR = v
v = c(0.444444, 0.333333, 0.222222)
# service rates
mu = c(4, 1, 3)
N = 3
K = 3
# Find tau_1, ..., tau_k
tau = v/mu
# Find G
G = matrix(1, nrow = (N+1), ncol = K)
for(i in 2:(N+1)){
   G[i, 1] = tau[1]^(i-1)
}
# i iterates over rows
for(i in 2:(N+1)){
   # j iterates over columns
   for(j in 2:(K)){
      G[i, j] = G[i, j-1] + tau[j]*G[i-1, j]
   }
}
lambda = G[N,K]/G[(N+1), K]
lambda_i = v*lambda
rho_i = tau*lambda

# Find L_i, W_i
p_bar = matrix(0, K, K)
# iterates over n_i
for(n_i in 1:K){
   # iterates over tau_i
   for(i in 1:K){
      p_bar[n_i, i] = (tau[i]^n_i)*(G[((N+1)-n_i),K]/G[(N+1), K])
   }
}
L_i = colSums(p_bar)
W_i = L_i/lambda_i
################################################################################
# Mean Value Analysis algorithm
# solution to vR = v
v = c(0.444444, 0.333333, 0.222222)
# service rates
mu = c(4, 1, 3)
N = 3
K = 3
# first row corresponds to L_{i}(0)
# second to L_{i}(1) and so on
L = matrix(0, N, K)
# stores the W_{i}(n) values
W = matrix(0, N, K)
# stores the lambda(n) values
lambda = rep(0, N)
for(n in 1:N){
   if(n == 1){
      for(i in 1:K){
         W[n, i] = (1/mu[i])
      }
   }
   else{
   for(i in 1:K){
      W[n, i] = (1/mu[i])*(1 + L[n-1, i])
   }
      }
   lambda[n] = n/(sum(v*W[n,]))
   for(j in 1:K){
      L[n, j] = lambda[n]*v[j]*W[n, j]
   }
}
# Output at convergence:
W_i = W[3,]
L_i = L[3,]
# Little's law : L = lambda * W
lam = L_i/W_i































