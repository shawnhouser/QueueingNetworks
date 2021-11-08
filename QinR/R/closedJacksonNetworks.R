#' Solution to Routing Matrix
#'
#' Finds v given the routing matrix R, solution to vR = v.
#'
#' @param R Matrix of routing probabilites
#'
#' @return Returns vR=v such that  ∑vi=1
#' @export
#'
#' @examples R = matrix(c(0, 0.75, 0.25,0.6666666667, 0, 0.3333333333,
#' 1, 0, 0), nrow = 3, ncol = 3, byrow = TRUE)
#' v = solve.routing(R)
#' print(v)
#'
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
#' @param v solution to vR = v
#' @param mu Array of service rates
#' @param N number of customers in the network
#' @param K number of nodes
#' @param convolution If true, uses convolution algorithm,
# otherwise Mean Value Analysis will be used.
#'
#' @return A summary of the measures of performance.
#' @export
#'
#' @examples # given service rates mu = c(4, 1, 3)
#' number of customers in the system N = 3
#' number of nodes in the network K = 3
#' given service rates
#' mu = c(4, 1, 3)
#' N = 3
#' K = 3
#'
#' First the using the convolution algorithm
#' q <- cjn.summary(v, mu, N, K, convolution = TRUE)
#'
#'qt <- q$res
#'qt %>%
#'   gt() %>%
#'   tab_header(
#'     title = md("Measures of Performance for Closed Jackson Networks"))
#' marginal distribution
#' print(q$marginal_probabilities)
#'
#'           [,1]       [,2]        [,3]
#' [1,] 0.68248175 0.04744526 0.788321168
#' [2,] 0.22554745 0.12481752 0.170802920
#' [3,] 0.07226277 0.29562044 0.035036496
#' [4,] 0.01970803 0.53211679 0.005839416
#'
#' system throughput
#' print(q$system_throughput)
#'
#' [1] 2.857664
#'
#' using same inputs as above with the MVA Algorithm
#' q <- cjn.summary(v, mu, N, K, convolution = FALSE)
#'
#' qt <- q$res
#' qt %>%
#'   gt() %>%
#'   tab_header(
#'      title = md("Measures of Performance for Closed Jackson Networks"))
#'
#' print(q$system_throughput)
#' [1] 2.857664

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

      p = matrix(0, N+1, K)
      # iterates over n_i
      for(n_i in 0:(N-1)){
         # iterates over tau_i
         for(i in 1:K){
            p[(n_i + 1), i] = (tau[i]^n_i)*((G[((N+1)-n_i),K] - tau[i]*G[((N+1)- n_i - 1),K])/G[(N+1), K])
         }
      }
      p[(N+1),] = (tau^N)/G[(N+1), K]


      rnames <- rbind("Node i throughput", "Utilization at node i",
                      "Probability node i is idle", "Mean time at node i","Mean number at node i")
      res <- data.frame(cbind(rnames, round(rbind(lambda_i, rho_i, I_i, W_i, L_i), 4)))
      names(res)<- c("Definition", sprintf("node%2d",seq(1:K)))

      return(list('marginal_probabilities' = p, 'res' = res, 'system_throughput' = system_throughput,
                  'throughput_i' = lambda_i, 'rho_i' = rho_i, 'idle' = I_i,
                  'W_i' = W_i, 'L_i' = L_i))
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
      # node i utilization
      rho_i = lambda_i / mu
      I_i = 1 - rho_i
      system_throughput = mean(rho_i/tau)
      rnames <- rbind("Node i throughput", "Utilization at node i",
                      "Probability node i is idle", "Mean time at node i","Mean number at node i")
      res <- data.frame(cbind(rnames, round(rbind(lambda_i, rho_i, I_i, W_i, L_i), 4)))
      names(res)<- c("Definition", sprintf("node%2d",seq(1:K)))

      return(list('res' = res, 'system_throughput' = system_throughput,
                  'throughput_i' = lambda_i, 'rho_i' = rho_i, 'idle' = I_i,
                  'W_i' = W_i, 'L_i' = L_i))
   }
}





























