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
