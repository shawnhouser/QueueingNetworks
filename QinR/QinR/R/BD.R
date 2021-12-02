#' Birth Death Process
#'
#' The Birth Death Process is used to solve the flow balance equation for a
#' birth-death type queue with finite capacity n.
#'
#' @param lambda Array of arrival (birth) rates.
#' @param mu Array of service (death) rates.
#'
#' @return The discrete distribution of number of customers in system.
#' @export
#'
#' @examples mu <- c(1, 2, 2)
#' lambda <- c(3, 2, 1)
#' pn = BD.solve(lambda, mu)
#' print(pn)
#'
#' [1] 0.1176471 0.3529412 0.3529412 0.1764706

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
















