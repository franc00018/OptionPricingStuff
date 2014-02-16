\name{putHeston}
\alias{putHeston}
\title{European put option pricing using Heston method}
\usage{
putHeston(strikeprice, dist.fn, ess.dist.fn, eval.time, expiry.time, rate, ...)
}
\arguments{
  \item{strikeprice}{Strike price, relative to a unit stock
  price}

  \item{dist.fn}{Distribution function for the risk neutral
  log-price process}

  \item{ess.dist.fn}{Esscher transformed (with h=1)
  distribution function for the risk neutral log-price
  process}

  \item{eval.time}{Evaluation time}

  \item{expiry.time}{Expiry time}

  \item{rate}{Continuously compounded interest rate (force
  of interest)}

  \item{...}{Parameters of the distribution function
  dist.fn}
}
\value{
European put option price
}
\description{
European put option pricing using Heston method
}
\author{
Francois Pelletier
}

