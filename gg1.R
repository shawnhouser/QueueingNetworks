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
