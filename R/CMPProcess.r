#' COM Poisson Process Parameter Estimation.
#'
#' \code{CMPProcess} computes the maximum likelihood estimates of a COM-Poisson process for given count data.
#'
#' @param counts A vector (-like object) of counts.
#' @param s The time interval corresponding to the levels of the counts vector.
#' @param h.out A boolean indicating if the information matrix and associated standard errors are returned.
#'     This can have a significant impact on processing time and should be left at the default unless needed
#' @return \code{CMPProcess} will return a list of seven elements: [1] se (Standard errors), [2] H (Information matrix), [3] lambda,
#'     [4] nu, [5] Z, [6] ll (Log likelihood), and [7] aic.
#'
#' @examples
#' ## Standard usage
#' data(floodcount)
#' CMPProcess(floodcount$Counts)
#'
#' ## Aggregate to s-unit = 3
#' three.year.bins <- list(floor((floodcount$Year-min(floodcount$Year))/3))
#' collapsed.floodcount <- aggregate( x = floodcount, by = three.year.bins, FUN=sum)
#' CMPProcess(collapsed.floodcount$Counts, s= 3)
#'
#' @import compoisson
#' @import numDeriv
#' @import stats
#'
#' @export

CMPProcess <- function(counts, s=1, h.out = FALSE) {
  if (s == 1) {
    CMPProc(counts, h.out)
  }
  else {
    SCMPProc(counts, h.out)
  }
}

#' COM Poisson Process Parameter Estimation, time interval = 1.
#'
#' \code{CMPProc} is strictly a helper function that is called by \code{CMPProcess}
#'
#' @param counts A vector (like object) of counts
#' @param h.out A boolean indicating if the information matrix and associated standard errors are returned. This can have a significant impact on processing time and should be left at the default unless needed
#'
#' @return Will return a list of seven elements to \code{CMPProcess}

CMPProc <- function(counts, h.out= F)
{
  LambdaNuPair <- function(par)
  {
    total <- sum(log(factorial(counts)))
    -(log(par[1]) * sum(counts) - par[2] * total - length(counts) * com.compute.log.z(par[1],par[2],exp(-20)))
  }
  
  LambdaNuEst <- nlminb(start = c(1,1), LambdaNuPair, lower = c(exp(-10),exp(-10)), upper =c(Inf, Inf))
  
  if (h.out == TRUE) H <- hessian(LambdaNuPair, LambdaNuEst$par)
  else               H <- NA
  
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
  cat("real dispersion:", var(counts)/mean(counts),"\n")
  cat("waiting time distribution is geometric with parameter:", 1-1/Z,"\n")
  cat("AIC:",4-2*loglike,"\n")
  
  return(list(se=se, H=H, lambda=lambda, nu=nu, Z=Z, ll=loglike, aic=(4-2*loglike)))
}

#' COM Poisson Process Parameter Estimation, time interval > 1.
#'
#' \code{CMPProc} is strictly a helper function that is called by \code{CMPProcess}
#'
#' @param counts A vector (like object) of counts
#' @param s The time interval corresponding to the levels of the counts vector
#' @param h.out A boolean indicating if the information matrix and associated standard errors are returned. This can have a significant impact on processing time and should be left at the default unless needed
#'
#' @return Will return a list of seven elements to \code{CMPProcess}

SCMPProc <- function(counts, s, h.out = F)
{
  LambdaNuPair <- function(par)
  {
    
    w <- 1
    total <- 0
    while(w <= length(counts))
    {
      total <- total +log(dSCMP(s,par[1],par[2],counts[w]))
      w <- w + 1
    }
    
    -total
  }
  
  LambdaNuEst <- nlminb(start = c(.01,.01), LambdaNuPair,lower = c(exp(-20),exp(-20)), upper =c(Inf, Inf),control=list(trace=1))
  
  if (h.out == TRUE)   H <- hessian(LambdaNuPair, LambdaNuEst$par)
  else                 H <- NA
  
  se <- sqrt(diag(solve(H)))
  lambda <- LambdaNuEst$par[1]
  nu <- LambdaNuEst$par[2]
  loglike <- -LambdaNuEst$objective
  Z <- com.compute.z(lambda,nu)
  
  cat("log likelihood:",loglike,"\n")
  cat("lambda:",lambda,"\n")
  cat("nu:",nu,"\n")
  cat("lambda se:",se[1],"\n")
  cat("nu se:",se[2],"\n")
  cat("waiting time distribution is geometric with parameter:", 1-1/(com.compute.z(lambda,nu)),"\n")
  cat("AIC:",4-2*loglike,"\n")
  
  return(list(se=se, H=H, lambda=lambda, nu=nu, Z=Z, ll=loglike, aic=(4-2*loglike)))
}

#' Compute SCMP probabilities.
#'
#' \code{CMPProc} is strictly a helper function that is called by \code{CMPProcess}. This code computes P(X=count) for a sCOM-Poisson(lambda, Nu, NuOfVar) distribution.
#'
#' @param NuOfVar Nu of Var
#' @param Lambda Lambda
#' @param Nu Nu
#' @param count Count
#'
#' @return Will return a probability, given the supplied the parameters

dSCMP <- function(NuOfVar,Lambda,Nu,count)
{
  
  lambda <- rep.int(Lambda,NuOfVar)
  nu <- rep.int(Nu,NuOfVar)
  
  if(length(lambda) == 1)
  {
    print("NuOfVar must be > 1. If interested in computing probability where NuOfVar = 1, run dcom in compoisson package.")
    return("TRY AGAIN!")
  }
  
  m <- 1
  while(m <= length(lambda))
  {
    if((lambda[m] <= 0)|(nu[m] <= 0))
    {
      print("lambda and nu have to be greater than 0.")
      return("TRY AGAIN!")
    }
    m <- m + 1
  }
  
  a <- numeric(0)
  keep <- numeric(0)
  total <- 0
  
  if (length(lambda) == 2)
  {
    a[1] <- 0
    a[2] <- count
    while(a[1] <= a[2])
    {
      total <- ((lambda[1])^(a[1])*(lambda[2])^(a[2]-a[1])/(((factorial(a[1]))^(nu[1])*(factorial(a[2]-a[1]))^(nu[2]))))
      keep <- c(keep, total)
      a[1] <- a[1] + 1
    }
    result <- 1/(com.compute.z(lambda[1],nu[1],exp(-30))*(com.compute.z(lambda[2],nu[2],exp(-30))))* sum(keep)
    return(result)
  }
  
  recur <- function(number,iterator)
  {
    a <- numeric(0)
    t <- number
    a[length(lambda)] <- count
    storage <- vector(mode = "list", length = t)
    storage[[1]] <- numeric(0)
    sum1 <- 0
    sum2 <- 0
    
    if (number == 2)
    {
      a[1] <- 0
      a[2] <- iterator
      storage[[1]] <- numeric(0)
      while(a[1] <= a[2])
      {
        sum1 <- ((lambda[1])^(a[1])*(lambda[2])^(a[2]-a[1])/(((factorial(a[1]))^(nu[1])*(factorial(a[2]-a[1]))^(nu[2]))))
        storage[[1]] <- c(storage[[1]], sum1)
        a[1] <- a[1] + 1
      }
      result <- 1/(com.compute.z(lambda[1],nu[1],exp(-10))*(com.compute.z(lambda[2],nu[2],exp(-10))))* sum(storage[[1]])
      return (sum(storage[[1]]))
    }
    else
    {
      a[t-1] <- 0
      a[t] <- iterator
      while(a[t-1] <= a[t])
      {
        sum2 <- (recur(number - 1,a[t-1]) * (lambda[t])^(a[t]-a[t-1])/(factorial(a[t]-a[t-1]))^(nu[t]))
        storage[[t]] <- c(storage[[t]], sum2)
        a[t-1] <- a[t-1] + 1
      }
      if (t < length(lambda))
      {
        return(sum(storage[[t]]))
      }
    }
    
    w <- 1
    result2 <- 1
    while(w <= length(lambda))
    {
      result2 <- result2 /(com.compute.z(lambda[w],nu[w],exp(-10)))
      w <- w + 1
    }
    result2 <- result2 * (sum(storage[[t]]))
    #cat("lambdas:",lambda,"\n")
    #cat("nus:    ",nu,"\n")
    #cat("Prob of",count,"counts: ", result2,"\n")
    return(result2)
  }
  
  
  
  recur(length(lambda),count)
}