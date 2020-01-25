#' Bf (legacy function)
#'
#' The following R code is based on the Baguley and Kaye (2010) R code for the Dienes (2008) calculator, changing the likelihood function for the data from a Normal distribution to a t-distribution. This is a way of accounting for the variance of observations being unknown in advance of the data - thus no correction factor need be applied to the SE when this calculator is used.
#' From http://www.lifesci.sussex.ac.uk/home/Zoltan_Dienes/inference/Bf%20with%20t%20likelihood.html
#'
#' @param sd observed standard error
#' @param obtained obtained mean
#' @param dfdata observed degrees of freedom
#' @param uniform is it uniform?
#' @param lower lower-bound (if uniform)
#' @param upper upper-bound (if uniform)
#' @param meanoftheory theory mean
#' @param sdtheory theory SD
#' @param tail number of tails
#'
#' @return list
#' @export
#'

Bf<-function(sd = 0.1, obtained = 0, dfdata = 99, uniform = 0, lower=0, upper=1, meanoftheory=0, sdtheory=1, tail=2) {
  area <- 0
  if(identical(uniform, 1)){
    theta <- lower
    range <- upper - lower
    incr <- range / 2000
    for (A in -1000:1000){
      theta <- theta + incr
      dist_theta <- 1 / range
      height <- dist_theta * stats::dt((obtained-theta)/sd, df=dfdata)
      area <- area + height * incr
    }
  }else{
    theta <- meanoftheory - 5 * sdtheory
    incr <- sdtheory / 200
    for (A in -1000:1000){
      theta <- theta + incr
      dist_theta <- stats::dnorm(theta, meanoftheory, sdtheory)
      if(identical(tail, 1)){
        if (theta <= 0){
          dist_theta <- 0
        } else {
          dist_theta <- dist_theta * 2
        }
      }
      height <- dist_theta * stats::dt((obtained-theta)/sd, df=dfdata)
      area <- area + height * incr
    }
  }
  LikelihoodTheory <- area
  Likelihoodnull <- stats::dt(obtained/sd, df = dfdata)
  BayesFactor <- LikelihoodTheory / Likelihoodnull
  ret <- list("LikelihoodTheory" = LikelihoodTheory, "Likelihoodnull" = Likelihoodnull, "BayesFactor" = BayesFactor)
  ret
}
