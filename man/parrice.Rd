\name{parrice}
\alias{parrice}
\title{Estimate the Parameters of the Rice Distribution}
\description{
This function estimates the parameters (\eqn{\nu} and \eqn{\alpha}) of the Rice distribution giventhe L-moments of the data in an L-moment object such as that returned by\code{lmom.ub}. The relations between distribution parameters and L-moments are complex and tabular lookup is made using a relation between \eqn{\tau} and a form of signal-to-noise ratio \eqn{\mathrm{SNR}} defined as \eqn{\nu/\alpha} and a relation between \eqn{\tau} and precomputed Laguerre polynomial, which in turn requires modified Bessel functions of the first kind.
The Laguerre polynomial is
\deqn{
L_{1/2}(x) = \mathrm{e}^{x/2}\times[(1-x)I_0(-x/2) - xI_1(-x/2)]
}
where the modified Bessel function of the first kind is \eqn{I_k(x)}.

The \eqn{\lambda_1} (mean) is most straightforward
\deqn{
\lambda_1 = \alpha \times \sqrt{\pi/2} L_{1/2}(-\nu^2/[2\alpha^2])
}
for which the terms to the right of the multiplication symbol are uniquely a function of \eqn{\tau} and precomputed for tabular lookup and interpolation from \file{sysdata.rdb} (\env{.lmomcohash$RiceTable}). Parameter estimation also relies directly on tabular lookup and interpolation to convert \eqn{\tau} to \eqn{\mathrm{SNR}}. The file \file{SysDataBuilder.R} provides additional technical details.
}
\usage{
parrice(lmom, checklmom=TRUE)
}
\arguments{
  \item{lmom}{A L-moment object created by \code{\link{lmom.ub}}
or \code{\link{pwm2lmom}}.}
  \item{checklmom}{Should the \code{lmom} be checked for validity using the \code{are.lmom.valid} function. Normally this should be left as the default and it is very unlikely that the L-moments will not be viable (particularly in the \eqn{\tau_4} and \eqn{\tau_3} inequality). However, for some circumstances or large simulation exercises then one might want to bypass this check. However, the end point of the Rice distribution for high \eqn{\nu/\alpha} is not determined here, so it is recommended to leave \code{checklmom} turned on.}
}
\value{
  An R \code{list} is returned.

  \item{type}{The type of distribution: \code{rice}.}
  \item{para}{The parameters of the distribution.}
  \item{source}{The source of the parameters: \dQuote{parrice}.}
}
\author{ W.H. Asquith}
\seealso{\code{\link{lmom.ub}}, \code{\link{lmomrice}}, \code{\link{cdfrice}}, \code{\link{quarice}}
}
\examples{

parrice(lmomrice(vec2par(c(100,11), type="rice")))
parrice(lmomrice(vec2par(c(10,50), type="rice")))

\dontrun{ # Beyond limits of the Rice
  parrice(lmomrice(vec2par(c(100,0.1), type="rice")))
}

plotlmrdia(lmrdia(), xlim=c(0,0.2), ylim=c(-0.1,0.22),
           autolegend=TRUE, xleg=0.05, yleg=0.05)
lines(.lmomcohash$RiceTable$TAU3, .lmomcohash$RiceTable$TAU4,
      lwd=5, col=8)
legend(0.1,0, "RICE DISTRIBUTION", lwd=5, col=8, bty="n")
text(0.14,-0.04, "Normal distribution limit on left end point")
text(0.14,-0.055, "Rayleigh distribution limit on right end point")
}
\keyword{distribution}
