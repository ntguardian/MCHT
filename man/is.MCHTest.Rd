% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MonteCarloTest.R
\name{is.MCHTest}
\alias{is.MCHTest}
\title{Is an Object of Type MCHTest?}
\usage{
is.MCHTest(x)
}
\arguments{
\item{x}{An R object}
}
\value{
\code{TRUE} if \code{x} is an \code{MCHTest}-class objet,
        \code{FALSE} otherwise
}
\description{
Checks whether its argument is an \code{\link{MCHTest}}-class object.
}
\examples{
f <- MCHTest(mean, mean, seed = 100)
is.MCHTest(1)
is.MCHTest(f)
}
