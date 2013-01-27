\name{quarice}
\alias{quarice}
\title{Quantile Function of the Rice Distribution}
\description{
This function computes the quantiles of the Rice distribution given
parameters (\eqn{\nu} and \eqn{\alpha}) of the distribution computed
by \code{\link{parrice}}. The quantile function of the distribution is complex and numerical rooting of the cumulative distribution function is used.
\deqn{
x(F,\nu,\alpha)
}
where \eqn{x(F)} is the quantile for nonexceedance probability \eqn{F},
\eqn{\nu} is a parameter, and \eqn{\nu/\alpha} is a form of signal-to-noise ratio \eqn{\mathrm{SNR}}. If \eqn{\nu=0}, then the Rayleigh distribution results and \code{\link{quaray}} is used.
}
\usage{
quarice(f, para, xmax=NULL, paracheck=TRUE)
}
\arguments{
  \item{f}{Nonexceedance probability (\eqn{0 \le F \le 1}).}
  \item{para}{The parameters from \code{\link{parrice}} or similar.}
  \item{xmax}{The maximum x value used for integeration.}
  \item{paracheck}{A logical controlling whether the parameters and checked for validity. Overriding of this check might be extremely important and needed for use of the distribution quantile function in the context of TL-moments with nonzero trimming.}
}
\value{
  Quantile value for nonexceedance probability \eqn{F}.
}
\author{ W.H. Asquith}
\seealso{\code{\link{cdfrice}}, \code{\link{parrice}}}
\examples{
lmr <- vec2lmom(c(125,0.20), lscale=FALSE)
quarice(0.75,parrice(lmr))


# The quantile function of the Rice as implemented in lmomco
# is slow because of rooting the CDF, which is created by
# integration of the PDF. Rician random variates are easily created.
# Thus, in speed applications the rlmomco() with a Rice parameter
# object could be bypassed by the following function, rrice().
\dontrun{
"rrice" = function(n, nu, alpha) { # from the VGAM package
    theta = 1 # any number
    X = rnorm(n, mean=nu * cos(theta), sd=alpha)
    Y = rnorm(n, mean=nu * sin(theta), sd=alpha)
    return(sqrt(X^2 + Y^2))
}
n <- 500; # suggest making it about 10,000
nu <- 100; alpha <- 10
lmoms(rrice(n, nu, alpha))
lmoms(rlmomco(n, vec2par(c(nu,alpha), type='rice')))
}


}
\keyword{distribution}
\keyword{quantile function}
