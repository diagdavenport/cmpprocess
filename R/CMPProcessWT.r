CMPProcessWT <- function(dispersion,t)
{
  LambdaNuPair <- function(par)
  {
    ((com.var(par[1],par[2]))/(com.mean(par[1],par[2]))-dispersion)^2 + (com.compute.z(par[1],par[2],exp(-20)) - (mean(t)/(mean(t)-1)))^2  
  }

  LambdaNuEst <- nlminb(start = c(1,1), LambdaNuPair, lower = c(exp(-100),exp(-100)), upper =c(Inf, Inf))
  lambda <- LambdaNuEst$par[1]
  nu <- LambdaNuEst$par[2]
  cat("lambda:",lambda,"\n")
  cat("nu:",nu,"\n")
  #cat("real dispersion:",dispersion,"\n")
  #cat("calculated dispersion:",com.var(lambda,nu)/com.mean(lambda,nu),"\n")
  #cat("real Z:",1/(1-1/mean(t)),"\n")
  #cat("calculated Z:",com.compute.z(lambda,nu),"\n")
  cat("waiting time distribution is geometric with parameter:", 1-1/(com.compute.z(lambda,nu)),"\n") 
  return(list(lambda=lambda, nu=nu,convergence=LambdaNuEst$convergence))
}
