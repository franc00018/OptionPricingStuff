# Damped characteristic function of the call option log-price
# 
# Author: Francois Pelletier
#
# LGPL 3.0
###############################################################################


#' Damped characteristic function of the call option log-price
#' @param u Transform variate
#' @param char.fn Characteristic function of the log-price process
#' @param eval.time Evaluation time
#' @param expiry.time Expiry time
#' @param rate Continuously compounded interest rate (force of interest)
#' @param alpha Damping parameter
#' @param ... Parameters of the characteristic function
#' @param moneyness Boolean for moneyness of call option 
#' (TRUE if strike price is lower than stock price)
#' @return Characteristic function value
#' 
#' @author Francois Pelletier
dampedcfcallCarrMadan <- function(u,char.fn,eval.time,expiry.time,rate,alpha,...,moneyness=TRUE)
{
	if(moneyness)
	{
		exp(-rate*(expiry.time-eval.time))*
				char.fn(u-1i*(alpha+1),expiry.time-eval.time,...) / 
				(alpha^2+alpha-u^2+1i*u*(2*alpha+1))
	}
	else
	{
		auxiliairyf <- function(u,char.fn,eval.time,expiry.time,rate,alpha,...)
		{
			exp(-rate*(expiry.time-eval.time))*
					(1/(1+1i*u)-exp(rate*(expiry.time-eval.time))/
						(1i*u)-char.fn(u-1i,expiry.time-eval.time,...)/(u^2-1i*u))
		}
		(auxiliairyf(u-1i*alpha,char.fn,eval.time,expiry.time,rate,alpha,...)-
				auxiliairyf(u+1i*alpha,char.fn,eval.time,expiry.time,rate,alpha,...))/2
	}
	
}


