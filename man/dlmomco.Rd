\name{dlmomco}
\alias{dlmomco}
\title{Probability Density Function of the Distributions}
\description{
This function acts as an alternative front end to \code{\link{par2pdf}}.
The nomenclature of the \code{dlmomco} function is to mimic that of
built-in \pkg{R} functions.
}
\usage{
dlmomco(x,para)
}
\arguments{
  \item{x}{A real value.}
  \item{para}{The parameters from \code{\link{lmom2par}} or similar.}
}
\value{
  Probability density for \code{x}.
}
\author{ W.H. Asquith}
\seealso{\code{\link{plmomco}}, \code{\link{rlmomco}}, \code{\link{qlmomco}}}
\examples{
para <- vec2par(c(0,1),type='nor') # standard normal parameters
nonexceed <- dlmomco(1,para) # percentile of one standard deviation
}
\keyword{probability density function}

