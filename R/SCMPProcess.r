SCMPProcess <- function(s,counts)
{
  LambdaNuPair <- function(par)
  {
print(par)
    w <- 1
    total <- 0
    while(w <= length(counts))
    {
      total <- total +log(dSCMP(s,par[1],par[2],counts[w]))
      w <- w + 1
    }
    #(log(total))^2
    -total
  }
  LambdaNuEst <- nlminb(start = c(.01,.01), LambdaNuPair,lower = c(exp(-20),exp(-20)), upper =c(Inf, Inf),control=list(trace=1))
  # H <- hessian(LambdaNuPair, LambdaNuEst$par)
  # se <- sqrt(diag(solve(H)))
  lambda <- LambdaNuEst$par[1]
  nu <- LambdaNuEst$par[2]
  loglike <- -LambdaNuEst$objective
  cat("log likelihood:",loglike,"\n")
  cat("lambda:",lambda,"\n")
  cat("nu:",nu,"\n")
  cat("lambda se:",se[1],"\n")
  cat("nu se:",se[2],"\n")
  cat("waiting time distribution is geometric with parameter:", 1-1/(com.compute.z(lambda,nu)),"\n") 
  cat("AIC:",4-2*loglike,"\n")
  # return(list(se=se,H=H,lambda=lambda, nu=nu, ll=loglike, aic=(4-2*loglike)))
  return(list(lambda=lambda, nu=nu, ll=loglike, aic=(4-2*loglike)))
}
