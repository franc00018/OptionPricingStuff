# Call price using the Carr-Madan damping parameter and FFT
# 
# Author: Francois Pelletier
#
# LGPL 3.0
###############################################################################


#' Call price using the Carr-Madan damping parameter and FFT
#' @param strikeprice Vector of strike prices, relative to a unit stock price
#' @param char.fn Characteristic function of the log-price process
#' @param param Characteristic function parameters
#' @param eval.time Evaluation time
#' @param expiry.time Expiry time
#' @param rate Continuously compounded interest rate (force of interest)
#' @param alpha Damping parameter
#' @param ... Parameters of the characteristic function
#' @param fft.control Control parameters list for the FFT discretization
#' @return A European call option price vector
#' @export callCarrMadan
#' @author Francois Pelletier
callCarrMadan <- function(strikeprice,char.fn,param,eval.time,expiry.time,rate,alpha,
		...,fft.control=list(N=2^10,eta=.1))
{
	# Discretization step for Fourier transform
	lambda <- lambda <- (2*pi) / (fft.control$N*fft.control$eta) 
	# Evaluation points of the damped characteristic function of the call option log-price
	u <- seq(0,(fft.control$N-1)*fft.control$eta,fft.control$eta)
	# Upper bound
	b <- (fft.control$N * lambda)/2
	# Vector of indices
	jvec <- 1:fft.control$N
	# Simpson's hypothesis
	simpsonh_money <- ((dampedcfcallCarrMadan(u,char.fn,param,eval.time,expiry.time,rate,alpha,moneyness=TRUE)*
			exp(1i*u*b)*fft.control$eta)/3)*(3+(-1)^jvec+((jvec-1)==0))
	simpsonh_nomoney <- ((dampedcfcallCarrMadan(u,char.fn,param,eval.time,expiry.time,rate,alpha,moneyness=FALSE)*
			exp(1i*u*b)*fft.control$eta)/3)*(3+(-1)^jvec+((jvec-1)==0))
	# Log-price vector
	ku <- seq(-b,(fft.control$N-1)*lambda-b,lambda)
	# Price vector
	Ku <- exp(ku)

	# Log-price of the call option vector
	callvec_money <- Re((exp(-alpha*ku)*fft(simpsonh_money))/pi)
	callvec_nomoney <- Re(fft(simpsonh_nomoney)/(sinh(alpha*ku)*pi))
	callvec <- callvec_money * (Ku < 1) + callvec_nomoney * (Ku >= 1)
	
	# Index to select subset of prices in the strikeprice vector
	Kindex <- Ku>=(min(strikeprice)-1) & Ku<=(max(strikeprice)+1) & abs(callvec) < 10^9 & abs(callvec) > 10^-9
	
	# We use a smooth spline to get the prices for the strikeprice vector
	sp0 <- smooth.spline(x=round(Ku[Kindex],digits=10),y=round(callvec[Kindex],digits=10))
	predict(sp0,strikeprice)$y
}
	

