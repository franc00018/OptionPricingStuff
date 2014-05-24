# European put option pricing using characteristic function
# 
# Author: Francois Pelletier
#
# LGPL 3.0
###############################################################################

#' European put option pricing using characteristic function 
#' 
#' As seen in Epps (2009)
#' @param strikeprice Strike price vector, relative to a unit stock price
#' @param char.fn Characteristic function of the price level at expiry time
#' @param eval.time Evaluation time
#' @param expiry.time Expiry time
#' @param rate Continuously compounded interest rate (force of interest)
#' @param ... Parameters of the characteristic function
#' @param int.bounds Integration bounds for the integrate() method used. Defaults to infinite bounds.
#' @return European put option price vector
#' @export putEpps
#' @author Francois Pelletier
putEpps <- function(strikeprice,char.fn,param,eval.time,expiry.time,rate,...,int.bounds=c(-Inf,0))
{
  # function to integrate (zhi)
  zhi <- function(x,char.fn,param,strikeprice,eval.time,expiry.time,...)
  {
    Re(strikeprice^{-1i*x} * 
         char.fn(x,param,eval.time,expiry.time,...) / 
         (x*(1i+x)))
  }
  # function to integrate with strike price as first parameter
  integrate.K <- function(strikeprice,zhi,char.fn,param,int.bounds,eval.time,expiry.time,rate,...)
  {
    exp(-rate*(expiry.time-eval.time)) * 
      strikeprice * 
      (.5 - integrate(zhi,int.bounds[1],int.bounds[2],char.fn,param,strikeprice,eval.time,expiry.time,...)$value / (pi))
  }
  lapply(as.list(strikeprice),integrate.K,zhi,char.fn,param,int.bounds,eval.time,expiry.time,rate,...)
}
