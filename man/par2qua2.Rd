\name{par2qua2}
\alias{par2qua2}
\title{ Equivalent Quantile Function of Two Distributions}
\description{
This function computes the nonexceedance probability of a given quantile from a linear weighted combination of two quantile functions.

The distributions are specified by the two parameter object arguments in usual \pkg{lmomco} style. The left-tail parameter object is the distribution obviously governing the left tail; the right-tail parameter object is of course governs the right tail. The quantile function algebra is
\deqn{ Q(F) = (1-F^\star) \times {\triangleleft}Q(F) + F^\star \times Q(F){\triangleright}\mbox{,}}
where \eqn{Q(F)} is the mixed quantile for nonexceedance probability \eqn{F}. \eqn{{\triangleleft}Q(F)} is the left-tail quantile function; \eqn{Q(F){\triangleright}} is the right-tail quantile function. The \code{\link{par2cdf2}} function inverses the above equation for \eqn{F}. Parameter objects are discussed in \code{\link{lmom2par}} and \code{\link{vec2par}}.

If the optional \code{weight} argument is provided, then \eqn{F^\star} is replaced by that weight. If \code{weight = 0}, then only the quantiles for the right tail (\eqn{Q(F){\triangleright}}) are returned, and if \code{weight = 1}, then only the quantiles for the left tail (\eqn{{\triangleleft}Q(F)}) are returned. If \code{weight = NULL}, then \eqn{F^\star = F =} \code{f} and the weight between the two quantile functions thus continuously varies from left to right. This is a probability proration from one to the other. A word of caution. The resulting weighted quantile function is not checked for monotonic increase with \eqn{F}. This is a required property of quantile functions.
}
\usage{
par2qua2(f, leftpara, rightpara, weight=NULL, ...)
}
\arguments{
  \item{f}{Nonexceedance probability (\eqn{0 \le F \le 1}).}
  \item{leftpara}{The left-tail parameters from \code{\link{lmom2par}} or \code{\link{vec2par}}.}
  \item{rightpara}{The right-tail parameters from \code{\link{lmom2par}} or similar.}
  \item{weight}{An optional weighting argument to use in lieu of \code{F}.}
  \item{...}{The additional arguments are passed to the quantile function such as \cr \code{paracheck = FALSE} for the generalized Lambda distribution (\code{\link{quagld}}).}
}
\value{
  The weighted quantile value for \eqn{F} from the two distributions.
}
\author{ W.H. Asquith}
\seealso{\code{\link{par2qua}}, \code{\link{par2cdf2}}
}
\examples{
# Contrived example
lmr       <- lmoms(rnorm(20))
leftpara  <- parnor(lmr)
rightpara <- pargev(lmr)
combined.median <- par2qua2(0.5,leftpara,rightpara)

# Bigger example--using Kappa fit to whole sample
# for the right tail and Normal fit to whole sample
# for the left tail
D   <- c(123,523,345,356,2134,345,2365,235,12,235,61)
LM  <- lmoms(D)
KAP <- parkap(LM)
NOR <- parnor(LM)
PP  <- pp(D)
plot(PP,sort(D),ylim=c(-500,2300))
lines(PP,par2qua(PP,KAP),col=2)
lines(PP,par2qua(PP,NOR),col=3)
lines(PP,par2qua2(PP,NOR,KAP))
}
\keyword{quantile mixture function}
\keyword{distribution (mixture)}
