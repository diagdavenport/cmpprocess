#' COM Poisson Process Parameter Estimation
#'
#' \code{cmpprocwt} computes the maximum likelihood estimates of a COM-Poisson process for given wait time data
#'
#' @param dispersion Ratio between the mean and variance of the process
#' @param t A vector (like object) of wait times
#'
#' @return \code{cmpprocwt} will return a list of three elements: [1] lambda, [2] nu, and [3] convergence
#'
#' @examples
#'
#' ## Standard use
#' data(floodcount)
#' cmpprocwt(.8 , mean(floodwait$WT))
#'
#' @import compoisson
#' @import numDeriv
#' @import stats
#'
#' @export

cmpprocwt <- function(dispersion,t)
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
