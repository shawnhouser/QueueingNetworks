########
# Bookkeeping for G/G/1 queues. The analysis is obtained under the assumption
# of a single server with FCFS discipline. 
########

arrival <- c(0, 2, 3, 6, 7, 8, 12, 14, 19, 20, 24, 26)
service <- c(1, 3, 6, 2, 1, 1, 1, 2, 5, 1, 1, 3)

gg1.summary <- function(arrival_times, service_times){
   if(length(arrival_times) != length(service_times))
      {
      stop('Array of arrival and service times must have equal length')
   }
   n <- length(arrival_times)
   interarrival_times <- c(diff(arrival_times), NA)
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
   return(df)
}

df <- gg1.summary(arrival, service)




#########
# A queueing process is described by a series of symbols and slashes
# A/B/X/Y/Z where A := interarrival-time distribution, 
# B := service-time distribution, X := number of parallel servers,
# Y := system capacity, Z := queue discipline.
#########

#########
# G/G/c queue
# That is, the interarrival-time distribution and the service-time distribution
# are both assumed to be any general distribution while c denotes the number
# of parallel servers.
#########

























