
mmc.summary <- function(lambda, mu, c, 
                        plot_transitions = FALSE,
                        plot_waitSys = FALSE,
                        plot_waitQ = FALSE){
   if(c == 1){
      rho = lambda/mu
      if(rho < 1) print("System is stable.")
      else{stop("System is unstable.")
      }
      c = 50*rho
      p_n = integer(c)
      for(iter in 1:c){
         p_n[iter] = (rho^iter)*(1-rho)
         iter = iter + 1
      }
      if(plot_transitions == TRUE){
         num_ent = c(0, 1:(c-1))
         plot(num_ent, p_n, main="Transition Probabilities",
              xlab="Number of customers in System",
              ylab="Probability", pch = 19, col = "blue")
      }
      t = seq(1, 12*rho, by = 1)
      waitSys_probabilities = (mu - lambda)*exp(-(mu - lambda)*t)
      if(plot_waitSys == TRUE){
         plot(t, waitSys_probabilities, main="pdf of T (waiting time in system)",
              xlab="Time Spent in System",
              ylab="Probability", pch = 19, col = "blue")
      }
      t = seq(1,12*rho, by = 1)
      waitQ_probabilities = c(1-rho, (mu*rho*(1-rho))*exp(-mu*(1 - rho)*t))
      if(plot_waitQ == TRUE){   
         plot(c(0, t), waitQ_probabilities, main= "pdf of T_q (waiting time in queue)",
              xlab="Time Spent in Queue",
              ylab="Probability", pch = 19, col = "blue")
      }
      L = rho/(1-rho)
      L_q = rho^2/(1-rho)
      W = 1/(mu - lambda)
      W_q = lambda/(mu*(mu - lambda))
      U = 1 - p_n[1]
      I = 1 - rho
      
      rnames <- rbind("Average arrival rate", "Utilization", 
                      "Idle time", "Average time in system",
                      "Average time in queue", "Average number in system", "Average number in queue")
      res <- data.frame(cbind(rnames, round(rbind(lambda, rho, I, W, W_q, L, L_q), 4)))
      names(res)<- c("Definition", "Result")
      return(list('transition_prob' = p_n, 'res' = res, 'lambda' = lambda,
                  'rho' = rho, 'p0' = I, 'W' = W, 'W_q' = W_q, 'L' = L, 'L_q' = L_q,
                  'prob_T' = waitSys_probabilities, 'prob_T_q' = waitQ_probabilities))
   }
   else{
      print("c = ?")
   }
   return(1)
}































