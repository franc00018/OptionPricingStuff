\name{putEpps}
\alias{putEpps}
\title{European put option pricing using characteristic function}
\usage{
putEpps(strikeprice, char.fn, eval.time, expiry.time, rate, ...,
  int.bounds = c(-Inf, Inf))
}
\arguments{
  \item{strikeprice}{Strike price, relative to a unit stock
  price}

  \item{char.fn}{Characteristic function of the price level
  at expiry time}

  \item{eval.time}{Evaluation time}

  \item{expiry.time}{Expiry time}

  \item{rate}{Continuously compounded interest rate (force
  of interest)}

  \item{...}{Parameters of the characteristic function}

  \item{int.bounds}{Integration bounds for the integrate()
  method used. Defaults to infinite bounds.}
}
\value{
European put option price
}
\description{
As seen in Epps (2009)
}
\author{
Francois Pelletier
}

