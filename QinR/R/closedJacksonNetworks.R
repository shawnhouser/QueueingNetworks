#' Solution to Routing Matrix
#'
#' Finds v given the routing matrix R, solution to vR = v.
#'
#' @param R Matrix of routing probabilites.
#'
#' @return Returns vR=v such that the sum of v_i's = 1.
#' @export
#'
#' @examples R = matrix(c(0, 0.75, 0.25,0.6666666667, 0, 0.3333333333,
#' 1, 0, 0), nrow = 3, ncol = 3, byrow = TRUE)
#' v = solve.routing(R)
#' print(v)
#'           [,1]      [,2]      [,3]
#' [1,] 0.4444444 0.3333333 0.2222222
solve.routing <- function(R){
   n = nrow(R)
   m = ncol(R)
   I = diag(x = 1, nrow = n, ncol = m)
   Q = R - I
   Q = cbind(Q, rep(1, n))
   b = matrix(c(rep(0, n), 1), nrow = 1, ncol = (m+1))
   v = (b%*%t(Q))%*%solve(Q%*%t(Q))
}
#' Closed Single-Server Jackson Network
#'
#' A system where N entities continuously travel inside the network. When convolution
#' is set to true, the function uses Buzen’s algorithm to solve for the summary statistics.
#'  Otherwise the function will use Mean Value Analysis.
#'
#' @param v Solution to vR = v
#' @param mu Array of service rates
#' @param N Number of customers in the network
#' @param K Number of nodes
#' @param convolution Boolean. If true, uses convolution algorithm,
# otherwise Mean Value Analysis will be used.
#'
#' @return A summary of the measures of performance.
#' @export
#'
#' @examples
#' R = matrix(c(0, 0.75, 0.25,0.6666666667, 0, 0.3333333333,1, 0, 0), nrow = 3, ncol = 3, byrow = TRUE)
#' v = solve.routing(R)
#' mu = c(4, 1, 3)
#' N = 3
#' K = 3
#'
#' Using the convolution algorithm
#' q.convolution <- cjn.summary(v, mu, N, K, convolution = TRUE)
#'
#'q.convolution$marginal_probabilities
#'[,1]       [,2]        [,3]
#'[1,] 0.68248175 0.04744526 0.788321168
#'[2,] 0.22554745 0.12481752 0.170802920
#'[3,] 0.07226277 0.29562044 0.035036496
#'[4,] 0.01970803 0.53211679 0.005839416
#'
#'q.convolution$res
#'Definition node 1 node 2 node 3
#'X            Node i throughput 1.2701 0.9526  0.635
#'X.1      Utilization at node i 0.3175 0.9526 0.2117
#'X.2 Probability node i is idle 0.6825 0.0474 0.7883
#'X.3        Mean time at node i 0.3379 2.4276 0.4069
#'L_i      Mean number at node i 0.4292 2.3124 0.2584
#'
#'q.convolution$system_throughput
#'[1] 2.857664
#'q.convolution$throughput_i
#'[,1]      [,2]      [,3]
#'[1,] 1.270073 0.9525547 0.6350365
#'
#' q.convolution$rho_i
#'[,1]      [,2]      [,3]
#'[1,] 0.3175182 0.9525547 0.2116788
#'
#'q.convolution$W_i
#'[,1]     [,2]      [,3]
#'[1,] 0.337931 2.427586 0.4068966
#'
#'q.convolution$L_i
#'[1] 0.4291971 2.3124088 0.2583942
#'
#'
#' Same inputs as above with the MVA Algorithm
#' q.MVA <- cjn.summary(v, mu, N, K, convolution = FALSE)
#'
#' q.MVA$res
#'Definition node 1 node 2 node 3
#'lambda_i          Node i throughput 1.2701 0.9526  0.635
#'rho_i         Utilization at node i 0.3175 0.9526 0.2117
#'I_i      Probability node i is idle 0.6825 0.0474 0.7883
#'W_i             Mean time at node i 0.3379 2.4276 0.4069
#'L_i           Mean number at node i 0.4292 2.3124 0.2584
#'
#'q.MVA$system_throughput
#'[1] 2.857664
#'
#'q.MVA$throughput_i
#'[1] 1.2700730 0.9525547 0.6350365
#'
#'q.MVA$rho_i
#'[1] 0.3175182 0.9525547 0.2116788
#'
#'q.MVA$W_i
#'[1] 0.3379310 2.4275862 0.4068966
#'
#'q.MVA$L_i
#'[1] 0.4291971 2.3124088 0.2583942

cjn.summary <- function(v, mu, N, K, convolution = TRUE){
   if(convolution == TRUE){
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
      system_throughput = G[N,K]/G[(N+1), K]
      lambda_i = v*system_throughput
      rho_i = tau*system_throughput
      I_i = 1-rho_i
      # Find L_i, W_i
      p_bar = matrix(0, K, K)
      # iterates over n_i
      for(n_i in 1:N){
         # iterates over tau_i
         for(i in 1:K){
            p_bar[n_i, i] = (tau[i]^n_i)*(G[((N+1)-n_i),K]/G[(N+1), K])
         }
      }
      L_i = colSums(p_bar)
      W_i = L_i/lambda_i
      Lq_i = L_i - (lambda_i / mu)
      Wq_i = Lq_i / lambda_i
      p_n = matrix(0, N+1, K)
      # iterates over n_i
      for(n_i in 0:(N-1)){
         # iterates over tau_i
         for(i in 1:K){
            p_n[(n_i + 1), i] = (tau[i]^n_i)*((G[((N+1)-n_i),K] - tau[i]*G[((N+1)- n_i - 1),K])/G[(N+1), K])
         }
      }
      p_n[(N+1),] = (tau^N)/G[(N+1), K]


      rnames <- rbind("Node i throughput", "Utilization at node i",
                      "Probability node i is idle", "Mean time at node i","Mean number at node i",
                      "Mean queue time at node i", "Mean number in queue at node i")
      res <- data.frame(cbind(rnames, round(rbind(lambda_i, rho_i, I_i, W_i, L_i, Wq_i, Lq_i), 4)))
      names(res)<- c("Definition", sprintf("node%2d",seq(1:K)))
      row.names(res) <- seq(1:7)
      return(list('marginal_probabilities' = p_n, 'res' = res, 'system_throughput' = system_throughput,
                  'lambda_i' = lambda_i, 'rho_i' = rho_i, 'idle' = I_i,
                  'W_i' = W_i, 'L_i' = L_i, 'Lq_i' = Lq_i, 'Wq_i' = Wq_i))
   }
   ################################################################################
   # Mean Value Analysis algorithm
   else{
      # Find tau_1, ..., tau_k
      tau = v/mu
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
      W_i = W[N,]
      L_i = L[N,]
      # Little's law : L = lambda * W
      # node i throughput
      lambda_i = L_i/W_i
      Lq_i = L_i - (lambda_i / mu)
      Wq_i = Lq_i / lambda_i
      # node i utilization
      rho_i = lambda_i / mu
      I_i = 1 - rho_i
      system_throughput = mean(rho_i/tau)
      rnames <- rbind("Node i throughput", "Utilization at node i",
                      "Probability node i is idle", "Mean time at node i","Mean number at node i",
                      "Mean queue time at node i", "Mean number in queue at node i")
      res <- data.frame(cbind(rnames, round(rbind(lambda_i, rho_i, I_i, W_i, L_i,  Wq_i, Lq_i), 4)))
      names(res)<- c("Definition", sprintf("node%2d",seq(1:K)))
      row.names(res) <- seq(1:7)
      return(list('res' = res, 'system_throughput' = system_throughput,
                  'lambda_i' = lambda_i, 'rho_i' = rho_i, 'idle' = I_i,
                  'W_i' = W_i, 'L_i' = L_i, 'Lq_i' = Lq_i, 'Wq_i' = Wq_i))
   }
}





















