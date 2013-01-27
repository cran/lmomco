\name{lmomwei}
\alias{lmomwei}
\title{L-moments of the Weibull Distribution}
\description{
This function estimates the L-moments of the Weibull distribution given the parameters
(\eqn{\zeta}, \eqn{\beta}, and \eqn{\delta}) from \code{\link{parwei}}. The Weibull distribution is a reverse Generalized Extreme Value distribution.  As result, the Generalized Extreme Value algorithms (\code{\link{lmomgev}}) are used for computation of the L-moments of the Weibull in this package (see \code{\link{parwei}}).

}
\usage{
lmomwei(para)
}
\arguments{
  \item{para}{The parameters of the distribution.}
}
\value{
  An R \code{list} is returned.

  \item{L1}{Arithmetic mean.}
  \item{L2}{L-scale---analogous to standard deviation.}
  \item{LCV}{coefficient of L-variation---analogous to coefficient of variation.}
  \item{TAU3}{The third L-moment ratio or L-skew---analogous to skew.}
  \item{TAU4}{The fourth L-moment ratio or L-kurtosis---analogous to kurtosis.}
  \item{TAU5}{The fifth L-moment ratio.}
  \item{L3}{The third L-moment.}
  \item{L4}{The fourth L-moment.}
  \item{L5}{The fifth L-moment.}
  \item{source}{An attribute identifying the computational
source of the L-moments: \dQuote{lmomwei}.}
}
\references{
Hosking, J.R.M. and Wallis, J.R., 1997, Regional frequency analysis---An
approach based on L-moments: Cambridge University Press.
}
\author{ W.H. Asquith}
\seealso{\code{\link{parwei}}, \code{\link{quawei}}, \code{\link{cdfwei}} }
\examples{
lmr <- lmom.ub(c(123,34,4,654,37,78))
lmr
lmomwei(parwei(lmr))
}
\keyword{L-moment (distribution)}
