#'Event-Oriented Bookkeeping for G/G/1 queues.
#'
#'Bookkeeping for G/G/1 queues. The analysis is obtained under the assumption
#'of a single server with FCFS discipline.
#'
#' @param arrivals Arrival time of customer
#' @param service_times Service time of customer
#' @param interarrival Boolean set to true, if arrival time is an interarrival time.
#'
#' @return Outputs Measures of Effectiveness for a G/G/1 queues.
#' @export
#'
#' @examples arrival <- c(0, 2, 3, 6, 7, 8, 12, 14, 19, 20, 24, 26)
#'service <- c(1, 3, 6, 2, 1, 1, 1, 2, 5, 1, 1, 3)
#'
#'inter <- c(1, 9, 6, 4, 7, 9, 5, 8, 4, 10, 6, 12, 6, 8,
#'           9, 5, 7, 8, 8, 7)
#'serv <- c(3, 7, 9, 9, 10, 4, 8, 5, 5, 3, 6, 3, 5, 4,
#'          9, 9, 8, 6, 8, 3)

#'res <- gg1.summary(arrival, service)
#'res2 <- gg1.summary(inter, serv, interarrival = TRUE)
#'
#'
#'
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
