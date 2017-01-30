CMPProcess <- function(counts)
{
  LambdaNuPair <- function(par)
  {
    w <- 1
    total <- 0

    print(proc.time())
    total <- sum(log(factorial(counts)))
    print(proc.time())


    print(proc.time())
    while(w <= length(counts))
    {
      total <- total + log(factorial(counts[w]))
      w <- w + 1
    }
    print(proc.time())

    -(log(par[1]) * sum(counts) - par[2] * total - length(counts) * com.compute.log.z(par[1],par[2],exp(-20)))
  }
  print("check1")
  LambdaNuEst <- nlminb(start = c(1,1), LambdaNuPair, lower = c(exp(-10),exp(-10)), upper =c(Inf, Inf))
  print("check")
  # H <- -optimHess(LambdaNuEst$par, LambdaNuPair)
  H <- hessian(LambdaNuPair, LambdaNuEst$par)
se <- sqrt(diag(solve(H)))
  lambda <- LambdaNuEst$par[1]
  nu <- LambdaNuEst$par[2]
  Z <- com.compute.z(lambda,nu)
  loglike <- -LambdaNuEst$objective
  cat("log likelihood:",loglike,"\n")
  cat("lambda:",lambda,"\n")
  cat("nu:",nu,"\n")
  cat("lambda se:",se[1],"\n")
  cat("nu se:",se[2],"\n")
  #cat("implied Z:",Z,"\n")
  cat("real dispersion:", var(counts)/mean(counts),"\n")
  #cat("implied dispersion:",com.var(lambda,nu)/com.mean(lambda,nu),"\n")
  cat("waiting time distribution is geometric with parameter:", 1-1/Z,"\n")
  cat("AIC:",4-2*loglike,"\n")
  return(list(se=se, H=H, lambda=lambda, nu=nu, Z=Z, ll=loglike, aic=(4-2*loglike)))
}
