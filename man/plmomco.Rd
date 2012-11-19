\name{plmomco}
\alias{plmomco}
\title{Cumulative Distribution Function of the Distributions}
\description{
This function acts as an alternative front end to \code{\link{par2cdf}}.
The nomenclature of the \code{plmomco} function is to mimic that of 
built-in \pkg{R} functions.
}
\usage{
plmomco(x,para)
}
\arguments{
  \item{x}{A real value.}
  \item{para}{The parameters from \code{\link{lmom2par}} or similar.}
}
\value{
  Nonexceedance probability (\eqn{0 \le F \le 1}) for \code{x}.
}
\author{ W.H. Asquith}
\seealso{\code{\link{dlmomco}}, \code{\link{rlmomco}}, \code{\link{qlmomco}}}
\examples{
para <- vec2par(c(0,1),type='nor') # standard normal parameters
nonexceed <- plmomco(1,para) # percentile of one standard deviation
}
\keyword{distribution}
