BD.transitions <- function(lambda, mu){
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
















