# European put option pricing using Heston method
# 
# Author: Francois Pelletier
#
# LGPL 3.0
###############################################################################

#' European put option pricing using Heston method
#' @param strikeprice Strike price, relative to a unit stock price
#' @param dist.fn Distribution function for the risk neutral log-price process
#' @param ess.dist.fn Esscher transformed (with h=1) distribution function for the risk neutral log-price process
#' @param eval.time Evaluation time
#' @param expiry.time Expiry time
#' @param rate Continuously compounded interest rate (force of interest)
#' @param ... Parameters of the distribution function dist.fn
#' @return European put option price
#' @export putHeston
#' @author Francois Pelletier
putHeston <- function(param,strikeprice,dist.fn,eval.time,expiry.time,rate)
{
	exp(-rate*(expiry.time-eval.time)) * strikeprice*dist.fn(log(strikeprice),param,hEsscher=0) - 
			dist.fn(log(strikeprice),param,hEsscher=1)
}

putHestonSaddle <- function(param,strikeprice,dist.fn1,dist.fn2,eval.time,expiry.time,rate)
{
	exp(-rate*(expiry.time-eval.time)) * strikeprice*dist.fn1(log(strikeprice),param) - 
			dist.fn2(log(strikeprice),param)
}



