\name{lmomgev}
\alias{lmomgev}
\title{L-moments of the Generalized Extreme Value Distribution}
\description{
This function estimates the L-moments of the Generalized Extreme Value distribution given the parameters
(\eqn{\xi}, \eqn{\alpha}, and \eqn{\kappa}) from \code{\link{pargev}}.
The L-moments in terms of the parameters are

\deqn{\lambda_1 = \xi + \frac{\alpha}{\kappa}(1-\Gamma(1+\kappa))}
\deqn{\lambda_2 = \frac{\alpha}{\kappa}(1-2^{-\kappa})\Gamma(1+\kappa)}
\deqn{\tau_3 = \frac{2(1-3^{-\kappa})}{1-2^{-\kappa}} - 3}
\deqn{\tau_4 = \frac{5(1-4^{-\kappa})-10(1-3^{-\kappa})+6(1-2^{-\kappa})}{1-2^{-\kappa}}}
}
\usage{
lmomgev(para)
}
\arguments{
  \item{para}{The parameters of the distribution.}
}
\value{
  An R \code{list} is returned.

  \item{L1}{Arithmetic mean.}
  \item{L2}{L-scale--analogous to standard deviation.}
  \item{LCV}{coefficient of L-variation--analogous to coefficient of variation.}
  \item{TAU3}{The third L-moment ratio or L-skew--analogous to skew.}
  \item{TAU4}{The fourth L-moment ratio or L-kurtosis--analogous to kurtosis.}
  \item{TAU5}{The fifth L-moment ratio.}
  \item{L3}{The third L-moment.}
  \item{L4}{The fourth L-moment.}
  \item{L5}{The fifth L-moment.}
  \item{source}{An attribute identifying the computational 
source of the L-moments: \dQuote{lmomgev--ordinary}.}
}
\references{
Hosking, J.R.M., 1990, L-moments--Analysis and estimation of
distributions using linear combinations of order statistics: Journal
of the Royal Statistical Society, Series B, vol. 52, p. 105-124.

Hosking, J.R.M., 1996, FORTRAN routines for use with the method of L-moments:
Version 3, IBM Research Report RC20525, T.J. Watson Research Center,
Yorktown Heights, New York.

Hosking, J.R.M. and Wallis, J.R., 1997, Regional frequency analysis--An
approach based on L-moments: Cambridge University Press.
}
\author{ W.H. Asquith}
\seealso{\code{\link{pargev}}, \code{\link{quagev}}, \code{\link{cdfgev}} }
\examples{
lmr <- lmom.ub(c(123,34,4,654,37,78))
lmr
lmomgev(pargev(lmr))
}
\keyword{distribution}
