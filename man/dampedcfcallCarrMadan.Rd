\name{dampedcfcallCarrMadan}
\alias{dampedcfcallCarrMadan}
\title{Damped characteristic function of the call option log-price}
\usage{
dampedcfcallCarrMadan(u, char.fn, eval.time, expiry.time, rate, alpha, ...,
  moneyness = TRUE)
}
\arguments{
  \item{u}{Transform variate}

  \item{char.fn}{Characteristic function of the log-price
  process}

  \item{eval.time}{Evaluation time}

  \item{expiry.time}{Expiry time}

  \item{rate}{Continuously compounded interest rate (force
  of interest)}

  \item{alpha}{Damping parameter}

  \item{...}{Parameters of the characteristic function}

  \item{moneyness}{Boolean for moneyness of call option
  (TRUE if strike price is lower than stock price)}
}
\value{
Characteristic function value
}
\description{
Damped characteristic function of the call option log-price
}
\author{
Francois Pelletier
}

