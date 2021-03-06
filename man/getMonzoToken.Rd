% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/authenticate.R
\name{getMonzoToken}
\alias{getMonzoToken}
\title{Authenticating against the API and retrieving auth tokens}
\usage{
getMonzoToken(key = Sys.getenv("MONZOR_CLIENTID"),
  secret = Sys.getenv("MONZOR_SECRET"))
}
\arguments{
\item{key}{The client id of your Monzo application}

\item{secret}{The secret for your Monzo application}
}
\description{
This function allows you to authenticate against the R API
}
\keyword{auth}
