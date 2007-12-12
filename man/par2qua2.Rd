\name{par2qua2}
\alias{par2qua2}
\title{ Equivalent Quantile Function of Two Distributions}
\description{
This function combines two distributions into one by weighting of the two quantile functions by the nonexceedance probability. The distributions are specified by the parameter arguments. The left-tail parameter object is the distribution governing the left tail; the right-tail parameter object is the distribution governing the right tail. The quantile function algebra is

\deqn{ Q(F) = (1-F) \times Q_{lefttail}(F) + F \times Q_{righttail}(F)} 

where \eqn{Q(F)} is the equivalent quantile for nonexceedance probability \eqn{F} computed by the tail weigthing. \eqn{Q_{lefttail}(F)} is the left-tail quantile function; \eqn{Q_{righttail}} is the right-tail quantile function.  Parameter objects are discussed in \code{\link{vec2par}} and \code{\link{lmom2par}} functions. If the optional \code{weight} argument is provided, then the multiplication of \code{1-F} or \code{F} is replaced by \code{1-weight} or \code{weight}, respectively. If \code{weight=0}, then the quantiles for the right tail are returned, and if \code{weight=1}, then the quantiles for the left tail are returned.

A word of caution. The resulting weighted quantile function is not checked for monotonic increase with \eqn{F}. This is a required property of quantile functions.
}
\usage{
par2qua2(f,leftpara,rightpara,weight=NULL,...)
}
\arguments{
  \item{f}{Nonexceedance probability (\eqn{0 \le F \le 1}).}
  \item{leftpara}{The left-tail parameters from \code{\link{lmom2par}} or similar.}
  \item{rightpara}{The right-tail parameters from \code{\link{lmom2par}} or similar.}
  \item{weight}{An optional weighting argument to use in lieu of \code{F}.}
  \item{...}{The additional arguments are passed to the quantile function such as \code{paracheck = FALSE} for the generalized Lambda distribution (\code{\link{quagld}}).}
}
\value{
  Quantile value for \eqn{F} from the two distributions.
}
\author{ W.H. Asquith}
\seealso{\code{\link{par2qua}}, \code{\link{lmom2par}}, and \code{\link{par2cdf2}}
}
\examples{
# Contrived example
lmr       <- lmom.ub(rnorm(20))
leftpara  <- parnor(lmr)
rightpara <- pargev(lmr)
combined.median <- par2qua2(0.5,leftpara,rightpara)

# Bigger example--using Kappa fit to whole sample
# for the right tail and Normal fit to whole sample
# for the left tail
D   <- c(123,523,345,356,2134,345,2365,235,12,235,61)
LM  <- lmom.ub(D)
KAP <- parkap(LM)
NOR <- parnor(LM)
PP  <- pp(D)
plot(PP,sort(D),ylim=c(-500,2300))
lines(PP,par2qua(PP,KAP),col=2)
lines(PP,par2qua(PP,NOR),col=3)
lines(PP,par2qua2(PP,NOR,KAP))
}
\keyword{distribution}
