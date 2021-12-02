################################################################################
# Finite Population Single Server Model (N >= 1)
################################################################################
# lambda := arrival rate
# mu := service rate
# N := system capacity
finitepopulation.summary <- function(lambda, mu, N){
   a <- lambda / mu
   if(N <= 100){
      bottleneck <- matrix(1, nrow = N+1, ncol = 1)
      for (n in 1:N) {
         bottleneck[n+1,1] <- bottleneck[n, 1]*(N-n+1)*a
      }
      p0 <- (1/sum(bottleneck))
      p_n <- c(bottleneck*p0)
      
      L_q <- N - ((lambda + mu)/lambda)*(1-p0)
      L <- N - (mu/lambda)*(1-p0)
      W <- N/(mu*(1-p0)) - 1/lambda
      W_q <- L_q/(lambda*(N-L))
      lambda_eff <- mu*(1-p0)
      I <- p0
      rnames <- rbind("Idle time", "Mean time in system",
                      "Mean time in queue", "Mean number in system", 
                      "Mean number in queue", "Effective arrival rate")
      res <- data.frame(cbind(rnames, round(rbind(I, W, W_q, L, L_q, lambda_eff), 4)))
      names(res)<- c("Definition", "Result")
      return(list('pn' = p_n, 'res' = res, 'p0' = I, 'W' = W, 'W_q' = W_q, 'L' = L, 
                  'L_q' = L_q, 'lambda_eff' = lambda_eff))
   }
   else{
      # 'Multiple Precision Floating-Point Reliably'
      # Pro: we can do this for very large N
      # Con: we have to depend on an external package that may change in the future
      
      if("Rmpfr" %in% rownames(installed.packages()) == FALSE) {install.packages("Rmpfr")}
      suppressWarnings(suppressMessages(library(Rmpfr)))
      bottleneck <- mpfr(matrix(1, nrow = N+1, ncol = 1), 300)
      for (n in 1:N) {
         bottleneck[n+1,1] <- bottleneck[n, 1]*mpfr((N-n+1)*a, 300)
      }
      p0 <- (1/sum(bottleneck))
      p_n <- c(bottleneck*p0)
      p_n <- as.numeric(p_n)
      
      L_q <- as.numeric(N - ((lambda + mu)/lambda)*(1-p0))
      L <- as.numeric(N - (mu/lambda)*(1-p0))
      W <- as.numeric(N/(mu*(1-p0)) - 1/lambda)
      W_q <- as.numeric(L_q/(lambda*(N-L)))
      lambda_eff <- as.numeric(mu*(1-p0))
      I <- as.numeric(p0)
      rnames <- rbind("Idle time", "Mean time in system",
                      "Mean time in queue", "Mean number in system", 
                      "Mean number in queue", "Effective arrival rate")
      res <- data.frame(cbind(rnames, round(rbind(I, W, W_q, L, L_q, lambda_eff), 4)))
      names(res)<- c("Definition", "Result")
      return(list('pn' = p_n, 'res' = res,'p0' = I, 'W' = W, 'W_q' = W_q, 'L' = L, 
                  'L_q' = L_q, 'lambda_eff' = lambda_eff))
   }
}
