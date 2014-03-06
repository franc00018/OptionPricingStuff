\name{putcallparity}
\alias{putcallparity}
\title{Put-Call Parity}
\usage{
putcallparity(optionvalue, strikeprice, eval.time, expiry.time, rate,
  toPut = TRUE)
}
\arguments{
  \item{optionvalue}{Option price}

  \item{strikeprice}{Strike price}

  \item{eval.time}{Evaluation time}

  \item{expiry.time}{Expiry time}

  \item{rate}{Continuously compounded interest rate (force
  of interest)}

  \item{toPut}{Boolean, Call to Put or Put to Call ?}
}
\value{
Option price
}
\description{
Put-Call Parity
}
\author{
François Pelletier
}

