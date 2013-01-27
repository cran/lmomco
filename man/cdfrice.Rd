\name{cdfrice}
\alias{cdfrice}
\title{Cumulative Distribution Function of the Rice Distribution}
\description{
This function computes the cumulative probability or nonexceedance probability
of the Rice distribution given parameters (\eqn{\nu} and \eqn{\mathrm{SNR}}) of the distribution computed
by \code{\link{parrice}}. The cumulative distribution function of the distribution is complex and numerical integration of the probability density function is used.
\deqn{
F(x) = 1 - Q\biggl(\frac{\nu}{\alpha}, \frac{x}{\alpha}\biggr)
}
where \eqn{F(x)} is the nonexceedance probability for quantile \eqn{x}, \eqn{Q(a,b)} is the Marcum Q-function, and \eqn{\nu/\alpha} is a form of signal-to-noise ratio \eqn{\mathrm{SNR}}. If \eqn{\nu=0}, then the Rayleigh distribution results and \code{\link{pdfray}} is used. The Marcum Q-function is difficult to work with and the \pkg{lmomco} uses the \code{integrate} function on \code{\link{pdfrice}}.
}
\usage{
cdfrice(x, para)
}
\arguments{
  \item{x}{A real value.}
  \item{para}{The parameters from \code{\link{parrice}} or similar.}
}
\value{
  Nonexceedance probability (\eqn{F}) for \eqn{x}.
}
\author{ W.H. Asquith}
\seealso{\code{\link{pdfrice}}, \code{\link{quarice}}, \code{\link{parrice}}}
\examples{
lmr <- vec2lmom(c(45,0.27), lscale=FALSE)
cdfrice(35,parrice(lmr))
}
\keyword{distribution}
\keyword{cumulative distribution function}
