## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----gg1summary, echo=FALSE---------------------------------------------------
gg1.summary <- function(arrivals, service_times, interarrival = FALSE){
   if(length(arrivals) != length(service_times))
   {
      stop('Arrays must have equal length')
   }
   n <- length(arrivals)
   if(interarrival == TRUE){
      interarrival_times = c(arrivals, NA)
      service_times = c(NA, service_times)
      arrival_times = c(0)
      arrival_times <- append(arrival_times, interarrival_times[1])
      for(i in 2:n){
         arrival_times <- append(arrival_times, arrival_times[i] + interarrival_times[i])
      }
      starts_service <- c(NA, arrival_times[2])
      departure_times <- c(NA, service_times[2] + arrival_times[2])
      for(i in 3:(n+1)){
         starts_service <- append(starts_service, max(departure_times[i-1], arrival_times[i]))
         departure_times <- append(departure_times, starts_service[i] + service_times[i])
      }
      queue_times <- c(starts_service - arrival_times)
      system_times <- c(queue_times + service_times)
      df <- data.frame(customers = c(0, seq(1:n)), arrival_times = arrival_times,
                       service_times = service_times, 
                       interarrival_times = interarrival_times, starts_service = starts_service,
                       departure_times = departure_times, queue_times = queue_times, 
                       system_times = system_times)
      
      W_q <- mean(queue_times, na.rm = TRUE)
      W <- mean(system_times, na.rm = TRUE)
      lambda <- n/departure_times[n+1]
      L_q <- lambda*W_q
      L <- lambda*W
      rho <- L - L_q
      p0 <- 1 - rho
      rnames <- rbind("Average arrival rate", "Utilization", 
                      "Idle time", "Average time in system",
                      "Average time in queue", "Average number in system", "Average number in queue")
      res <- data.frame(cbind(rnames, round(rbind(lambda, rho, p0, W, W_q, L, L_q), 4)))
      names(res)<- c("Definition", "Result")
      return(list('bookkeeping' = df, 'res' = res, 'lambda' = lambda,
                  'rho' = rho, 'p0' = p0, 'W' = W, 'W_q' = W_q, 'L' = L, 'L_q' = L_q))
      
   }
   else{
      interarrival_times <- c(diff(arrivals), NA)
      starts_service <- c(0)
      departure_times <- c(service_times[1] - arrivals[1])
      for(i in 2:n){
         starts_service <- append(starts_service, max(departure_times[i-1], arrivals[i]))
         departure_times <- append(departure_times, starts_service[i] + service_times[i])
      }
      queue_times <- starts_service - arrivals
      system_times <- queue_times + service_times
      df <- data.frame(customers = seq(1:n), arrivals = arrivals,
                       service_times = service_times, 
                       interarrival_times = interarrival_times, starts_service = starts_service,
                       departure_times = departure_times, queue_times = queue_times, 
                       system_times = system_times)
      
      W_q <- mean(queue_times)
      W <- mean(system_times)
      lambda <- n/departure_times[n]
      L_q <- lambda*W_q
      L <- lambda*W
      rho <- L - L_q
      p0 <- 1 - rho
      rnames <- rbind("Average arrival rate", "Utilization", 
                      "Idle time", "Average time in system",
                      "Average time in queue", "Average number in system", "Average number in queue")
      res <- data.frame(cbind(rnames, round(rbind(lambda, rho, p0, W, W_q, L, L_q), 4)))
      names(res)<- c("Definition", "Result")
      return(list('bookkeeping' = df, 'res' = res, 'lambda' = lambda,
                  'rho' = rho, 'p0' = p0, 'W' = W, 'W_q' = W_q, 'L' = L, 'L_q' = L_q))
   }
}

## ----ex1----------------------------------------------------------------------
# Arrival times
arrival <- c(0, 2, 3, 6, 7, 8, 12, 14, 19, 20, 24, 26)
# Service times
service <- c(1, 3, 6, 2, 1, 1, 1, 2, 5, 1, 1, 3)
# Run gg1.summary() on this data
q <- gg1.summary(arrival, service)
# Use gt package for nice tables
library(gt)
# Get bookkeeping results
pt <- q$bookkeeping
pt %>%
   gt() %>%
   tab_header(
      title = md("Event-Oriented Bookkeeping for G/G/1 Queues"))
# Get performance results
qt <- q$res
qt %>%
   gt() %>%
   tab_header(
      title = md("Measures of Performance for G/G/1 Queues"))

## ----ex2----------------------------------------------------------------------
# Interarrival times
inter <- c(1, 9, 6, 4, 7, 9, 5, 8, 4, 10, 6, 12, 6, 8, 
           9, 5, 7, 8, 8, 7)
# Service times
serv <- c(3, 7, 9, 9, 10, 4, 8, 5, 5, 3, 6, 3, 5, 4, 
          9, 9, 8, 6, 8, 3)
# Run gg1.summary() on this data
q2 <- gg1.summary(inter, serv, interarrival = TRUE)
pt2 <- q2$bookkeeping
pt2 %>%
   gt() %>%
   tab_header(
      title = md("Event-Oriented Bookkeeping for G/G/1 Queues"))
# Get performance results
qt2 <- q2$res
qt2 %>%
   gt() %>%
   tab_header(
      title = md("Measures of Performance for G/G/1 Queues"))

## ----BD.solve, echo=FALSE-----------------------------------------------------
BD.solve <- function(lambda, mu){
   if(length(lambda) != length(mu))
   {
      stop('Arrays must have equal length')
   }
   temp = lambda/mu
   k = length(lambda)
   s = 0
   for(j in 1:k){
      s = s + prod(temp[1:j])
   }
   p0 = 1/(1+s)
   pn = c(p0)
   for(j in 1:k){
      pn <- append(pn, temp[j]*pn[j])
   }
   return(pn)
}

## ----ex8----------------------------------------------------------------------
mu = c(1, 2, 2)
lambda = c(3, 2, 1)
pn = BD.solve(lambda, mu)
print(pn)

## ----mm1summary, echo=FALSE---------------------------------------------------
mmc.summary <- function(lambda, mu, c, 
                        plot_pn = FALSE,
                        plot_waitSys = FALSE,
                        plot_waitQ = FALSE){
   if(c == 1){
      rho = lambda/mu
      if(rho < 1) print("System is stable.")
      else{stop("System is unstable.")
      }
      c = 25*rho
      pn = integer(c)
      for(iter in 1:c){
         pn[iter] = (rho^iter)*(1-rho)
      }
      if(plot_pn == TRUE){
         num_ent = c(0, 1:(c-1))
         plot(num_ent, pn, main="Distribution of Number of Customers in System",
              xlab="Number of customers in System",
              ylab="Probability", pch = 19, col = "blue")
      }
      t = seq(1, 12*rho, by = 1)
      waitSys_probabilities = (mu - lambda)*exp(-(mu - lambda)*t)
      if(plot_waitSys == TRUE){
         plot(t, waitSys_probabilities, main="Waiting Time in System",
              xlab="Time Spent in System",
              ylab="Probability", pch = 19, col = "blue")
      }
      t = seq(1,12*rho, by = 1)
      waitQ_probabilities = c(1-rho, (mu*rho*(1-rho))*exp(-mu*(1 - rho)*t))
      if(plot_waitQ == TRUE){   
         plot(c(0, t), waitQ_probabilities, main= "Waiting Time in Queue",
              xlab="Time Spent in Queue",
              ylab="Probability", pch = 19, col = "blue")
      }
      L = rho/(1-rho)
      L_q = rho^2/(1-rho)
      W = 1/(mu - lambda)
      W_q = lambda/(mu*(mu - lambda))
      U = 1 - pn[1]
      I = 1 - rho
      
      rnames <- rbind("Average arrival rate", "Utilization", 
                      "Idle time", "Average time in system",
                      "Average time in queue", "Average number in system", "Average number in queue")
      res <- data.frame(cbind(rnames, round(rbind(lambda, rho, I, W, W_q, L, L_q), 4)))
      names(res)<- c("Definition", "Result")
      return(list('pn' = pn, 'res' = res, 'lambda' = lambda,
                  'rho' = rho, 'p0' = I, 'W' = W, 'W_q' = W_q, 'L' = L, 'L_q' = L_q,
                  'prob_T' = waitSys_probabilities, 'prob_Tq' = waitQ_probabilities))
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
      pn = c(p0)
      for(n in 1:c){
         pn = append(pn, ((a^n)/factorial(n))*p0)
      }
      for(n in (c+1):(10*c)){
         pn = append(pn, ((a^n)/(factorial(c)*c^(n-c)))*p0)
      }
      if(plot_pn == TRUE){
         num_ent = length(pn)
         plot(c(1:num_ent-1), pn, main="Distribution of Number of Customers in System",
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
      rnames <- rbind("Average arrival rate", "Utilization", 
                      "Idle time", "Mean number busy servers", "Average time in system",
                      "Average time in queue", "Average number in system", "Average number in queue")
      res <- data.frame(cbind(rnames, round(rbind(lambda, U, I, B, W, W_q, L, L_q), 4)))
      names(res)<- c("Definition", "Result")
      return(list('pn' = pn, 'res' = res, 'lambda' = lambda,
                  'rho' = rho, 'p0' = I, 'B' = B, 'W' = W, 'W_q' = W_q, 'L' = L, 'L_q' = L_q))
   }
}

## ----ex3----------------------------------------------------------------------
q <- mmc.summary(lambda = 4, mu = 4.4545, c = 1, plot_pn = TRUE,
                 plot_waitQ = TRUE,
                 plot_waitSys = TRUE)
library(gt)
qt <- q$res
qt %>%
   gt() %>%
   tab_header(
      title = md("Measures of Performance for M/M/1 Queues"))

## ----ex4----------------------------------------------------------------------
k = 2
prob_k_system <- (q$rho)^k
print(prob_k_system)

## ----ex5----------------------------------------------------------------------
mu = 4.4545
t <- 2
pr <- exp(-mu*(1 - q$rho)*t)
print(pr)

## ----ex6----------------------------------------------------------------------
mu = 4.4545
t <- 2
pr2 <- (q$rho)*exp(-mu*(1 - q$rho)*t)
print(pr2)

## ----ex7----------------------------------------------------------------------
q <- mmc.summary(lambda = 4, mu = 2, c = 3, plot_pn = TRUE)
library(gt)
qt <- q$res
qt %>%
   gt() %>%
   tab_header(
      title = md("Measures of Performance for M/M/c Queues"))

