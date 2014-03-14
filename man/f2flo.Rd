\name{f2flo}
\alias{f2flo}
\title{Conversion of Annual Nonexceedance Probability to Conditional Probability Nonexceedance Probabilities}
\description{
This function converts  the cumulative distribution function of \eqn{F(x)} to a conditional  cumulative distribution function \eqn{P(x)} based on the probability level of the left-hand threshold. It is recommended that this threshold (as expressed as a probability) be that value returned from \code{\link{x2xlo}} in attribute \code{pp}. The conversion is simple
\deqn{
P(x) <- (F(x) - \mathrm{pp})/(1-\mathrm{pp})\mathrm{,}
}
where the term \eqn{\mathrm{pp}} corresponds to the estimated probability or plotting position of the left-hand threshold.

This function is particularly useful for applications in which zero values in the data set require truncation so that logarithms of the data may be used. But also this function  contributes to the isolation of the right-hand tail of the distribution for analysis.
}
\usage{
f2flo(f, pp=NA)
}
\arguments{
  \item{f}{A vector of nonexceedance probabilities.}
  \item{pp}{The plotting position of the left-hand threshold and recommended to come from \code{\link{x2xlo}}.}
}
\value{
  A vector of conditional nonexceedance probabilities.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{x2xlo}}, \code{\link{flo2f}} }
\examples{
# See examples for x2xlo().
}
\keyword{utility (probability)}
