\name{par2cdf2}
\alias{par2cdf2}
\title{Equivalent Cumulative Distribution Function of Two Distributions}
\description{
This function computes the nonexceedance probability of a given quantile from a linear weighted combination of two quantile functions but accomplishes this from the perspective of cumulative distribitution functions (see \code{\link{par2qua2}}).  However, for the current implementation simply \code{uniroot}'ing of a internally declared function and \code{\link{par2qua2}} is made.

The distributions are specified by the two parameter object arguments in usual \pkg{lmomco} style. The left-tail parameter object is the distribution obviously governing the left tail; the right-tail parameter object is of course governs the right tail. The quantile function algebra is
\deqn{ Q(F) = (1-F) \times Q_{\leftarrow}(F) + F \times Q_{\rightarrow}(F)\mbox{,}}
where \eqn{Q(F)} is the equivalent quantile for nonexceedance probability \eqn{F} computed by the tail weigthing. \eqn{Q_{\leftarrow}(F)} is the left-tail quantile function; \eqn{Q_{\rightarrow}(F)} is the right-tail quantile function. The \code{\link{par2cdf2}} function inverses the above equation for \eqn{F}. Parameter objects are discussed in \code{\link{lmom2par}} and \code{\link{vec2par}}. If the optional \code{weight} argument is provided, then the multiplication of \code{1-F} or \code{F} is replaced by \code{1-weight} or \code{weight}, respectively. If \code{weight=0}, then the quantiles for the right tail are returned, and if \code{weight=1}, then the quantiles for the left tail are returned.  The biggest feature on this function is that a vector of real values is used as the primary argument and not a vector of \eqn{F}.

A word of caution. The resulting weighted quantile function is not checked for monotonic increase with \eqn{F}. This is a required property of quantile functions.
}
\usage{
par2cdf2(x, leftpara, rightpara, weight=NULL, ...)
}
\arguments{
  \item{x}{A real value vector.}
  \item{leftpara}{The left-tail parameters from \code{\link{lmom2par}} or \code{\link{vec2par}}.}
  \item{rightpara}{The right-tail parameters from \code{\link{lmom2par}} or \code{\link{vec2par}}.}
  \item{weight}{An optional weighting argument to use in lieu of the \code{F}. Consult the documentation for \code{\link{par2qua2}} for the implementation details when \code{weight} is \code{NULL}.}
  \item{...}{The additional arguments are passed to the cumulative distribution function such as \code{paracheck=FALSE} for the Generalized Lambda distribution (\code{\link{cdfgld}}).}
}
\value{
  Nonexceedance probabilities (\eqn{0 \le F \le 1}) for \code{x} from the two distributions.
}
\author{ W.H. Asquith}
\seealso{\code{\link{par2cdf}}, \code{\link{lmom2par}}, \code{\link{par2qua2}}
}
\examples{
# Contrived example
lmr       <- lmoms(rnorm(20))
leftpara  <- parnor(lmr); rightpara <- pargev(lmr)
combined.median <- par2qua2(0.5,leftpara,rightpara)
combined.nonexceed <- par2cdf2(combined.median,leftpara,rightpara)
}
\keyword{distribution (mixture)}
