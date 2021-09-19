########
# Bookkeeping for G/G/1 queues. The analysis is obtained under the assumption
# of a single server with FCFS discipline. 
########

arrival <- c(0, 2, 3, 6, 7, 8, 12, 14, 19, 20, 24, 26)
service <- c(1, 3, 6, 2, 1, 1, 1, 2, 5, 1, 1, 3)

gg1.summary <- function(arrivals, service_times, interarrival = FALSE){
   if(length(arrivals) != length(service_times))
      {
      stop('Arrays must have equal length')
   }
   n <- length(arrivals)
   if(interarrival == TRUE){
      interarrival_times = arrivals
      arrival_times = c(0)
      arrival_times <- append(arrival_times, interarrival_times[1])
      for(i in 2:(n-1)){
         arrival_times <- append(arrival_times, arrival_times[i] + interarrival_times[i])
      }
      starts_service <- c(0)
      departure_times <- c(service_times[1] - arrival_times[1])
      for(i in 2:n){
         starts_service <- append(starts_service, max(departure_times[i-1], arrival_times[i]))
         departure_times <- append(departure_times, starts_service[i] + service_times[i])
      }
      queue_times <- starts_service - arrival_times
      system_times <- queue_times + service_times
      df <- data.frame(customers = seq(1:n), arrival_times = arrival_times,
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
      return(list('df' = df, 'W_q' = W_q, 'W' = W, 'lambda' = lambda,
                  'L_q' = L_q, 'L' = L, 'rho' = rho, 'p0' = p0))
      
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
   df <- data.frame(customers = seq(1:n), arrivals = arrival_times,
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
   return(list('df' = df, 'W_q' = W_q, 'W' = W, 'lambda' = lambda,
                   'L_q' = L_q, 'L' = L, 'rho' = rho, 'p0' = p0))
   }

}

inter <- c(1, 9, 6, 4, 7, 9, 5, 8, 4, 10, 6, 12, 6, 8, 
           9, 5, 7, 8, 8, 7)
serv <- c(3, 7, 9, 9, 10, 4, 8, 5, 5, 3, 6, 3, 5, 4, 
          9, 9, 8, 6, 8, 3)

res <- gg1.summary(arrival, service)
res2 <- gg1.summary(inter, serv, interarrival = TRUE)

res$W_q
res$W

res$L_q
res$L

res$lambda

pt <- res$df
pt %>% gt()

pt %>%
   gt() %>%
   tab_header(
      title = md("Event-Oriented Bookkeeping for G/G/1 Queues"))




