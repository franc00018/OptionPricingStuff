\name{callCarrMadan}
\alias{callCarrMadan}
\title{Call price using the Carr-Madan damping parameter and FFT}
\usage{
callCarrMadan(strikeprice, char.fn, eval.time, expiry.time, rate, alpha, ...,
  fft.control = list(N = 2^14, eta = 0.1))
}
\arguments{
  \item{strikeprice}{Vector of strike prices, relative to a
  unit stock price}

  \item{char.fn}{Characteristic function of the log-price
  process}

  \item{eval.time}{Evaluation time}

  \item{expiry.time}{Expiry time}

  \item{rate}{Continuously compounded interest rate (force
  of interest)}

  \item{alpha}{Damping parameter}

  \item{...}{Parameters of the characteristic function}

  \item{fft.control}{Control parameters list for the FFT
  discretization}
}
\value{
A European call option price vector
}
\description{
Call price using the Carr-Madan damping parameter and FFT
}
\author{
Francois Pelletier
}

