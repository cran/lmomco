\name{quaemu}
\alias{quaemu}
\title{Quantile Function of the Eta-Mu Distribution}
\description{
This function computes the quantiles of the Eta-Mu (\eqn{\eta:\mu}) distribution given \eqn{\eta} and \eqn{\mu}) of the distribution computed
by \code{\link{paremu}}. The quantile function of the distribution is complex and numerical rooting of the cumulative distribution function is used. The quantile function is
\deqn{
x(F,\eta,\mu) = \ldots
}
where \eqn{x(F)} is the quantile for nonexceedance probability \eqn{F}.
}
\usage{
quaemu(f, para, paracheck=TRUE, yacoubsintegral=TRUE, eps=1e-7)
}
\arguments{
  \item{f}{Nonexceedance probability (\eqn{0 \le F \le 1}).}
  \item{para}{The parameters from \code{\link{paremu}} or similar.}
  \item{paracheck}{A logical controlling whether the parameters and checked for validity.}
  \item{yacoubsintegral}{A logical controlling whether the integral by Yacoub (2007) is used for the cumulative distribution function instead of numerical integration of \code{\link{pdfemu}}.}
  \item{eps}{A close-enough error term for the recursion process.}
}
\value{
  Quantile value for nonexceedance probability \eqn{F}.
}
\references{
Yacoub, M.D., 2007, The kappa-mu distribution and the eta-mu distribution: IEEE Antennas and Propagation Magazine, v. 49, no. 1, pp. 68--81
}
\author{ W.H. Asquith}
\seealso{\code{\link{cdfemu}}, \code{\link{paremu}}}
\examples{

quaemu(0.75,vec2par(c(0.9, 1.5), type="emu"))

}
\keyword{distribution}
\keyword{quantile function}
