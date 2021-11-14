#' M/M/c Markovian Model
#'
#' Given inputs arrival rate, service rate, and number of servers c=1 the function
#' mm1.summary() returns a data frame of performance measures for the queue and
#' an array with the discrete distribution of number of customers in system.
#'  Entering “plot_pn = TRUE”, the function plots the distribution.
#'  For m/m/1 queues, by entering "plot_waitSys = TRUE" the function plots the probability
#'  density function of the waiting time in system, "plot_waitQ = TRUE" plots
#'  the probability density function of the waiting time in queue.
#'  The function stops finding additional probabilities when the sum of the pn is less than 1 - tol.
#'
#' @param lambda Arrival Rate
#' @param mu Service Rate
#' @param c Number of Servers. c= 1 is a
#' @param plot_pn Boolean set to true, plots the distribution.
#' @param plot_waitSys Boolean set to true for a M/M/1, will plot the probability density function of the waiting time in system
#' @param plot_waitQ Boolean set to true for a M/M/c, plot the probability density function of the waiting time in queue
#'
#' @return Returns a data frame of performance measures, an array with the discrete distribution of
#' number of customers in system and plots of waiting times.
#' @export
#'
#' @examples M/M/1 Markovian model
#' mm1.queue <- mmc.summary(lambda = 4, mu = 4.4545, c = 1,
#'tol = 0.01,
#'plot_pn = TRUE,
#'plot_waitQ = TRUE,
#'plot_waitSys = TRUE)
#'
#'[1] "System is stable."
#'
#'mm1.queue$pn
#'[1] 0.102031653 0.091621195 0.082272933 0.073878490 0.066340545 0.059571710
#'[7] 0.053493510 0.048035478 0.043134339 0.038733271 0.034781252 0.031232463
#'[13] 0.028045763 0.025184208 0.022614621 0.020307214 0.018235235 0.016374664
#'[19] 0.014703930 0.013203664 0.011856472 0.010646737 0.009560433 0.008584966
#'[25] 0.007709028 0.006922463 0.006216152 0.005581908 0.005012377 0.004500956
#'[31] 0.004041716 0.003629333 0.003259026 0.002926502 0.002627906 0.002359777
#'[37] 0.002119005 0.001902799 0.001708653 0.001534317 0.001377768 0.001237192
#'[43] 0.001110959

#' mm1.queue$res
#'Definition Result
#'rho           Utilization  0.898
#'I               Idle time  0.102
#'W     Mean time in system 2.2002
#'W_q    Mean time in queue 1.9757
#'L   Mean number in system 8.8009
#'L_q  Mean number in queue 7.9029
#' mm1.queue$rho
#'[1] 0.8979683
#' mm1.queue$p0
#'[1] 0.1020317
#' mm1.queue$W
#'[1] 2.20022
#' mm1.queue$W_q
#'[1] 1.975728
#' mm1.queue$L
#'[1] 8.80088
#' mm1.queue$L_q
#'[1] 7.90291
#'
#' Markovian queueing models with parallel channels: M/M/c/∞/∞ queues
#'
#' mmc.queue <- mmc.summary(lambda = 4, mu = 4.4545, c = 2, tol = 0.01, plot_pn = TRUE)
#'[1] "System is stable."
#' mmc.queue$pn
#'[1] 0.38027733 0.34147700 0.15331777 0.06883725 0.03090684 0.01387668
#'[7] 0.00623041
#'
#'mmc.queue$res
#'Definition Result
#'U                                      Utilization  0.449
#'I                                        Idle time 0.3803
#'B                         Mean number busy servers  0.898
#'W                              Mean time in system 0.2812
#'W_q                             Mean time in queue 0.0567
#'L                            Mean number in system 1.1247
#'L_q                           Mean number in queue 0.2267
#'delayed Probability arriving entity waits in queue 0.2782
#'
#' mmc.queue$rho
#'[1] 0.4489842

#' mmc.queue$p0
#'[1] 0.3802773
#' mmc.queue$W
#'[1] 0.2811728
#' mmc.queue$W_q
#'[1] 0.05668072
#' mmc.queue$L
#'[1] 1.124691
#' mmc.queue$L_q
#'[1] 0.2267229
#'

mmc.summary <- function(lambda, mu, c, tol,
                        plot_pn = FALSE,
                        plot_waitSys = FALSE,
                        plot_waitQ = FALSE){
   if(c == 1){
      rho = lambda/mu
      if(rho < 1) print("System is stable.")
      else{stop("System is unstable.")
      }

      p_n = c()
      iter = 0
      while(sum(p_n) < (1-tol)){
         p_n = append(p_n, (rho^iter)*(1-rho))
         iter = iter + 1
      }
      if(plot_pn == TRUE){

         num_ent = c(0, 1:(length(p_n)-1))
         plot(num_ent, p_n, main="Distribution of Number of Customers in System",
              xlab="Number of customers in System",
              ylab="Probability", pch = 19, col = "blue")
      }
      t = seq(1, 12*rho, by = 1)
      waitSys_probabilities = (mu - lambda)*exp(-(mu - lambda)*t)
      if(plot_waitSys == TRUE){
         plot(t, waitSys_probabilities, main="Waiting Time in System",
              xlab="Time Spent in System",
              ylab="Probability", pch = 19, col = "blue")
         lines(t, waitSys_probabilities, type = "b", col = "blue")
      }
      t = seq(1,12*rho, by = 1)
      waitQ_probabilities = c(1-rho, (mu*rho*(1-rho))*exp(-mu*(1 - rho)*t))
      if(plot_waitQ == TRUE){
         plot(c(0, t), waitQ_probabilities, main= "Waiting Time in Queue",
              xlab="Time Spent in Queue",
              ylab="Probability", pch = 19, col = "blue")
         lines(t, (mu*rho*(1-rho))*exp(-mu*(1 - rho)*t), type = "b", col = "blue")
      }
      L = rho/(1-rho)
      L_q = rho^2/(1-rho)
      W = 1/(mu - lambda)
      W_q = lambda/(mu*(mu - lambda))
      U = 1 - p_n[1]
      I = 1 - rho

      rnames <- rbind("Utilization",
                      "Idle time", "Mean time in system",
                      "Mean time in queue", "Mean number in system", "Mean number in queue")
      res <- data.frame(cbind(rnames, round(rbind(rho, I, W, W_q, L, L_q), 4)))
      names(res)<- c("Definition", "Result")
      return(list('pn' = p_n, 'res' = res,
                  'rho' = rho, 'p0' = I, 'W' = W, 'W_q' = W_q, 'L' = L, 'L_q' = L_q,
                  'prob_T' = waitSys_probabilities, 'prob_T_q' = waitQ_probabilities))
   }
   else{
      rho = lambda/(c*mu)
      if(rho < 1) print("System is stable.")
      else{stop("System is unstable.")}
      # a := offered load
      a = lambda/mu
      # find p0
      l = 0
      for(n in 0:(c-1)){
         l <- l + (a^n)/(factorial(n))
      }
      r = (a^c)/(factorial(c)*(1-rho))
      p0 = 1/(l + r)
      # find pn
      p_n = c(p0)
      for(n in 1:c){
         p_n = append(p_n, ((a^n)/factorial(n))*p0)
      }
      #for(n in (c+1):(10*c)){
      #   p_n = append(p_n, ((a^n)/(factorial(c)*c^(n-c)))*p0)
      #}

      iter = c+1
      while(sum(p_n) < (1-tol)){
         p_n = append(p_n, ((a^iter)/(factorial(c)*c^(iter-c)))*p0)
         iter = iter + 1
      }

      if(plot_pn == TRUE){
         num_ent = length(p_n)
         plot(c(1:num_ent-1), p_n, main="Distribution of Number of Customers in System",
              xlab="Number of customers in System",
              ylab="Probability", pch = 19, col = "blue")
      }
      L_q = ((a^c)*rho/(factorial(c)*(1-rho)^2))*p0
      W_q = ((a^c)/(factorial(c)*(c*mu)*(1-rho)^2))*p0
      W = W_q + (1/mu)
      L = L_q + (lambda/mu)
      U = rho
      B = a
      I = p0
      delayed = ((a^c)*p0)/(factorial(c)*(1-rho))
      rnames <- rbind("Utilization",
                      "Idle time", "Mean number busy servers", "Mean time in system",
                      "Mean time in queue", "Mean number in system", "Mean number in queue",
                      "Probability arriving entity waits in queue")
      res <- data.frame(cbind(rnames, round(rbind(U, I, B, W, W_q, L, L_q, delayed), 4)))
      names(res)<- c("Definition", "Result")
      return(list('pn' = p_n, 'res' = res,
                  'rho' = rho, 'p0' = I, 'r' = B, 'W' = W, 'W_q' = W_q, 'L' = L, 'L_q' = L_q))
   }
}
