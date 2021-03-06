% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/balance.R
\name{getBalance}
\alias{getBalance}
\title{Retrieve an account's balance}
\usage{
getBalance(mtoken = getMonzoToken(), accountId = NULL)
}
\arguments{
\item{mtoken}{The Monzo API token}

\item{accountId}{The id of the account you're requesting transactions from}
}
\description{
Retrieve balance, defaulting to the first valid account if no account ID is supplied
}
\keyword{balance}
