
#' Open Jackson Networks
#'
#' @param R Routing matrix R.
#' @param gamma Mean arrival rate to each node.
#' @param mu Mean service rate of each node.
#'
#' @return Measures of performance.
#' @export
#'
#' @examples R = matrix(c(1/3, 1/4, 0.000, 1/4, 0.000, 1/6, 0.000,
#' 1/3, 1/4, 0.000, 1/3, 0.000, 0.000, 0.000,
#' 0.000, 0.000, 1/3, 1/3, 1/3, 0.000, 0.000,
#' 1/3, 0.000, 1/3, 0.000, 1/3, 0.000, 0.000,
#' 0.000, 0.000, 0.000, 4/5, 0.000, 0.000, 1/6,
#' 1/6, 0.000, 1/6, 1/6, 1/6, 1/6, 0.000,
#' 0.000, 1/6, 1/6, 1/6, 1/6, 0.000, 1/6),
#' nrow = 7, ncol = 7, byrow = TRUE)
#' gamma = c(0, 5, 0, 5, 0, 0, 0)
#' mu = c(85, 85, 120, 120, 70, 20, 20)
#'
#' q <- ojn.summary(R, gamma, mu)
#'
#' qt <- q$res
#' qt %>%
#'    gt() %>%
#'    tab_header(
#'       title = md("Measures of Performance for Open Jackson Networks"))
#'
#'

ojn.summary <- function(R, gamma, mu){
   n = nrow(R)
   m = ncol(R)
   K = length(mu)
   I = diag(x = 1, nrow = n, ncol = m)
   Q = I - R
   lambda_i = gamma%*%(solve(Q))
   rho_i = lambda_i / mu
   I_i = 1-rho_i
   Lq_i = (rho_i)^2 / I_i
   L_i = (rho_i)/ I_i
   W_i = L_i / lambda_i
   Wq_i = Lq_i / lambda_i
   L = sum(L_i)
   W = L/(sum(gamma))
   # are the marginals just rho_i ^ n?
   rnames <- rbind("Node i throughput", "Utilization at node i",
                   "Probability node i is idle", "Mean time at node i","Mean number at node i",
                   "Mean queue time at node i", "Mean number in queue at node i")
   res <- data.frame(cbind(rnames, round(rbind(lambda_i, rho_i, I_i, W_i, L_i, Wq_i, Lq_i), 4)))
   names(res)<- c("Definition", sprintf("node%2d",seq(1:K)))
   # 'marginal_probabilities' = p,
   return(list('res' = res,
               'throughput_i' = lambda_i, 'rho_i' = rho_i, 'idle' = I_i,
               'W_i' = W_i, 'L_i' = L_i, 'Wq_i' = Wq_i, 'Lq_i' = Lq_i,
               'W' = W, 'L' = L))
}



























