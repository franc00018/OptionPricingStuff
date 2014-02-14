# Put-Call Parity
# 
# Author: Francois Pelletier
#
# LGPL 3.0
###############################################################################


#' Put-Call Parity
#' @param optionvalue Option price
#' @param strikeprice Strike price
#' @param eval.time Evaluation time
#' @param expiry.time Expiry time
#' @param rate Continuously compounded interest rate (force of interest)
#' @param toPut Boolean, Call to Put or Put to Call ?
#' @return Option price
#' 
#' @author François Pelletier
parity <- function(optionvalue,strikeprice,eval.time,expiry.time,rate,toPut=TRUE)
{
	if(toPut)
	{
		optionvalue-1+strikeprice*zerobond(eval.time,expiry.time,rate)
		
	}
	else
	{
		1+optionvalue-strikeprice*zerobond(eval.time,expiry.time,rate)
	}
}
