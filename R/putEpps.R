# European put option pricing using characteristic function
# 
# Author: Francois Pelletier
#
# LGPL 3.0
###############################################################################

#' European put option pricing using characteristic function 
#' 
#' As seen in Epps (2009)
#' @param char.fn Characteristic function of the price level at expiry time
#' @param strikeprice Strike price, relative to a unit stock price
#' @param eval.time Evaluation time
#' @param expiry.time Expiry time
#' @param rate Continuously compounded interest rate (force of interest)
#' @param ... Parameters of the characteristic function
#' @param int.bounds Integration bounds for the integrate() method used. Defaults to infinite bounds.
#' @return European put option price
#' 
#' @author Francois Pelletier
putEpps <- function(char.fn,strikeprice,eval.time,expiry.time,rate,...,int.bounds=c(-Inf,Inf))
{
  # function to integrate (zhi)
  zhi <- function(x,char.fn,strikeprice,eval.time,expiry.time,rate,...)
  {
    Re(strikeprice^{-1i*x} * 
         char.fn(x,strikeprice,eval.time,expiry.time,rate,...) / 
         (x*(1i+x)))
  }
  # function to integrate with strike price as first parameter
  integrate.K <- function(strikeprice,zhi,int.bounds,eval.time,expiry.time,rate,...)
  {
    exp(-rate*(expiry.time-eval.time)) * 
      strikeprice * 
      (.5 - integrate(zhi,int.bounds[1],int.bounds[2],strikeprice,eval.time,expiry.time,rate,...)$value / (2*pi))
  }
  mclapply(as.vector(strikeprice),integrate.K,zhi,int.bounds,eval.time,expiry.time,rate,...)
}