\name{quakmu}
\alias{quakmu}
\title{Quantile Function of the Kappa-Mu Distribution}
\description{
This function computes the quantiles of the Kappa-Mu (\eqn{\kappa:\mu}) distribution given parameters (\eqn{\kappa} and \eqn{\alpha}) of the distribution computed
by \code{\link{parkmu}}. The quantile function of the distribution is complex and numerical rooting of the cumulative distribution function is used. The quantile function is
\deqn{
x(F,\eta,\mu) = \ldots
}
where \eqn{x(F)} is the quantile for nonexceedance probability \eqn{F}.
}
\usage{
quakmu(f, para, paracheck=TRUE, marcumQ=TRUE, ...)
}
\arguments{
  \item{f}{Nonexceedance probability (\eqn{0 \le F \le 1}).}
  \item{para}{The parameters from \code{\link{parkmu}} or similar.}
  \item{paracheck}{A logical controlling whether the parameters and checked for validity.}
  \item{marcumQ}{A logical controlling whether the Marcum Q function is used for the cumulative distribution function instead of numerical integration of \code{\link{pdfkmu}}.}
  \item{...}{Additional arguments to pass to the other probability functions used for the rooting process.}
}
\value{
  Quantile value for nonexceedance probability \eqn{F}.
}
\references{
Yacoub, M.D., 2007, The kappa-mu distribution and the eta-mu distribution: IEEE Antennas and Propagation Magazine, v. 49, no. 1, pp. 68--81
}
\author{ W.H. Asquith}
\seealso{\code{\link{cdfkmu}}, \code{\link{parkmu}}}
\examples{

quakmu(0.75,vec2par(c(0.9, 1.5), type="kmu"))

}
\keyword{distribution}
