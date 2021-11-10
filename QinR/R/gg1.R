#'Event-Oriented Bookkeeping for G/G/1 queues.
#'
#'Bookkeeping for G/G/1 queues. The analysis is obtained under the assumption
#'of a single server with FCFS discipline.
#'
#' @param arrivals Array of arrival times of customer.
#' @param service_times Array of service times of customer.
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
#'
#'res$bookkeeping
#'customers arrivals service_times interarrival_times starts_service departure_times queue_times system_times
#'1          1        0             1                  2              0               1           0            1
#'2          2        2             3                  1              2               5           0            3
#'3          3        3             6                  3              5              11           2            8
#'4          4        6             2                  1             11              13           5            7
#'5          5        7             1                  1             13              14           6            7
#'6          6        8             1                  4             14              15           6            7
#'7          7       12             1                  2             15              16           3            4
#'8          8       14             2                  5             16              18           2            4
#'9          9       19             5                  1             19              24           0            5
#'10        10       20             1                  4             24              25           4            5
#'11        11       24             1                  2             25              26           1            2
#'12        12       26             3                 NA             26              29           0            3

#'res$res
#'             Definition Result
#'lambda     Average arrival rate 0.4138
#'rho                 Utilization  0.931
#'p0                    Idle time  0.069
#'W        Average time in system 4.6667
#'W_q       Average time in queue 2.4167
#'L      Average number in system  1.931
#'L_q     Average number in queue      1
#'
#'
#' res$lambda
#'[1] 0.4137931
#'
#' res$rho
#'[1] 0.9310345
#'
#' res$p0
#'[1] 0.06896552
#'
#' res$W
#'[1] 4.666667
#'
#' res$W_q
#'[1] 2.416667
#'
#' res$L
#'[1] 1.931034
#'
#' res$L_q
#'[1] 1

#'results2 <- gg1.summary(inter, serv, interarrival = TRUE)
#'results2$bookkeeping
#'customers arrival_times service_times interarrival_times starts_service departure_times queue_times system_times
#'1          0             0            NA                  1             NA              NA          NA           NA
#'2          1             1             3                  9              1               4           0            3
#'3          2            10             7                  6             10              17           0            7
#'4          3            16             9                  4             17              26           1           10
#'5          4            20             9                  7             26              35           6           15
#'6          5            27            10                  9             35              45           8           18
#'7          6            36             4                  5             45              49           9           13
#'8          7            41             8                  8             49              57           8           16
#'9          8            49             5                  4             57              62           8           13
#'10         9            53             5                 10             62              67           9           14
#'11        10            63             3                  6             67              70           4            7
#'12        11            69             6                 12             70              76           1            7
#'13        12            81             3                  6             81              84           0            3
#'14        13            87             5                  8             87              92           0            5
#'15        14            95             4                  9             95              99           0            4
#'16        15           104             9                  5            104             113           0            9
#'17        16           109             9                  7            113             122           4           13
#'18        17           116             8                  8            122             130           6           14
#'19        18           124             6                  8            130             136           6           12
#'20        19           132             8                  7            136             144           4           12
#'21        20           139             3                 NA            144             147           5            8
#' results2$res
#'Definition Result
#'lambda     Average arrival rate 0.1361
#'rho                 Utilization 0.8435
#'p0                    Idle time 0.1565
#'W        Average time in system  10.15
#'W_q       Average time in queue   3.95
#'L      Average number in system  1.381
#'L_q     Average number in queue 0.5374
#'
#' results2$lambda
#'[1] 0.1360544
#'
#' results2$rho
#'[1] 0.8435374
#'
#' results2$p0
#'[1] 0.1564626
#'
#' res$W
#'[1] 4.666667
#'
#' res$W_q
#'[1] 2.416667
#'
#' res$L
#'[1] 1.931034
#'
#' res$L_q
#'[1] 1


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
