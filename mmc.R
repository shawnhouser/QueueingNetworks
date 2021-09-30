
mmc.summary <- function(lambda = 4, mu = 8, c){
   if(c == 1){
      rho = lambda/mu
      transitions = replicate(100, 0)
      iter = 1
      while(sum(transitions) != 1 || iter <= 100){
          transitions[i] = (rho^i)*(1-rho)
          iter = iter + 1
      }
   }
   else{
      
   }
}








































































