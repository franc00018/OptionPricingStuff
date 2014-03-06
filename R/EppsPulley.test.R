# Approximate Epps-Pulley normality test
# 
# Author: Francois Pelletier
#
# LGPL 3.0
###############################################################################

#' Approximate Epps-Pulley normality test
#'
#' An Approximation to the Limit Distribution
#' of the Epps-Pulley Test Statistic for Normality
#' By N. Henze
#' Metrika (1990) 37:7-18
#' @param x Sample
#' @param alpha Tolerance level
#' @return A list containing the test statistics
#' @export EppsPulley.test
#' @author François Pelletier
EppsPulley.test <- function(x,alpha=0.05)
{
	## Statistics
	n <- length(x)
	if (n<10) stop("n must be > 10")
	xbar <- mean(x)
	S <- sd(x)
	## Constants
	gamma <- 3.55295
	delta <- 1.23062
	lambda <- 2.26664
	xi <- -0.020682
	## Calculations
	T <- 2/n*sum(outer(x,x,function(x,y) exp(-0.5*(x-y)^2 / S^2))*
							outer(1:n,1:n,function(x,y) x<y)) -
			sqrt(2)*sum(exp(-0.25*(x-xbar)^2/S^2))+
			n/sqrt(3)+1
	Tmod <- (T - 0.365/n + 1.34/n^2)*(1 + 1.3/n)
	Z <- gamma+delta*log((Tmod-xi)/(xi+lambda-Tmod))
	Pvalue <- 1-pnorm(Z)
	reject <- Pvalue<alpha
	cat(sprintf("\nEpps-Pulley Normality test\n\n T: %f\n T*: %f\np-value: %f\n\n",T,Tmod,Pvalue))
	list(Tstat=T,Tmod=Tmod,Zscore=Z,Pvalue=Pvalue,Reject=reject)
}

