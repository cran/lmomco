\name{par2cdf2}
\alias{par2cdf2}
\title{Equivalent Cumulative Distribution Function of Two Distributions}
\description{
This function computes the nonexceedance probability of a given quantile from the weighted combination of two quantile functions. The distributions are specified by the two parameter object arguments. The left-tail parameter object is the distribution governing the left tail; the right-tail parameter object is the distribution governing the right tail. The quantile function algebra is

\deqn{ Q(F) = (1-F) \times Q_{lefttail}(F) + F \times Q_{righttail}(F)}

where \eqn{Q(F)} is the equivalent quantile for nonexceedance probability \eqn{F} computed by the tail weigthing. \eqn{Q_{lefttail}(F)} is the left-tail quantile function; \eqn{Q_{righttail}} is the right-tail quantile function. The \code{par2cdf2} function inverses the above equation for \eqn{F}. Parameter objects are discussed in \code{\link{vec2par}} and \code{\link{lmom2par}} functions. If the optional \code{weight} argument is provided, then the multiplication of \code{1-F} or \code{F} is replaced by \code{1-weight} or \code{weight}, respectively. If \code{weight=0}, then the quantiles for the right tail are returned, and if \code{weight=1}, then the quantiles for the left tail are returned.

A word of caution. The resulting weighted quantile function is not checked for monotonic increase with \eqn{F}. This is a required property of quantile functions.
}
\usage{
par2cdf2(x,leftpara,rightpara,weight=NULL,...)
}
\arguments{
  \item{x}{A real value.}
  \item{leftpara}{The left-tail parameters from \code{\link{lmom2par}} or similar.}
  \item{rightpara}{The right-tail parameters from \code{\link{lmom2par}} or similar.}
  \item{weight}{An optional weighting argument to use in lieu of \code{F}.}
  \item{...}{The additional arguments are passed to the cumulative distribution function such as \code{paracheck=FALSE} for the generalized Lambda distribution (\code{\link{cdfgld}}).}
}
\value{
  Nonexceedance probability (\eqn{0 \le F \le 1}) for \code{x} from the two distributions.
}
\author{ W.H. Asquith}
\seealso{\code{\link{par2cdf}}, \code{\link{lmom2par}}, and \code{\link{par2qua2}}
}
\examples{
# Contrived example
lmr       <- lmom.ub(rnorm(20))
leftpara  <- parnor(lmr)
rightpara <- pargev(lmr)
combined.median <- par2qua2(0.5,leftpara,rightpara)
combined.nonexceed <- par2cdf2(combined.median,leftpara,rightpara)
}
\keyword{distribution (mixture)}
