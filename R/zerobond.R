# Evaluate the price of a zero coupon bond
# 
# Author: Francois Pelletier
#
# LGPL 3.0
###############################################################################


#' Evaluate the price of a zero coupon bond
#'
#' Evaluates the actualised price of a bond using interest rate and face value
#' @title zerobond
#' @param eval.time Evaluation time
#' @param expiry.time Expiry time
#' @param rate Continuously compounded interest rate (force of interest)
#' @param face Face value
#' @return Actualised price of bond
#' @export zerobond
#' @author François Pelletier

zerobond <- function(eval.time,expiry.time,rate,face=1)
{
	exp(-rate*(expiry.time-eval.time))*face
}
