\name{lmomnor}
\alias{lmomnor}
\title{L-moments of the Normal Distribution}
\description{
This function estimates the L-moments of the Normal distribution given
the parameters (\eqn{\mu} and \eqn{\sigma}) from \code{\link{parnor}}.
The L-moments in terms of the parameters are

\deqn{\lambda_1 = \mu}
\deqn{\lambda_2 = \sqrt{\pi} \sigma}
\deqn{\tau_3 = 0}
\deqn{\tau_4 = 0.122602}
\deqn{\tau_5 = 0}
}
\usage{
lmomnor(para)
}
\arguments{
  \item{para}{The parameters of the distribution.}
}
\value{
  An R \code{list} is returned.

  \item{L1}{Arithmetic mean.}
  \item{L2}{L-scale--analogous to standard deviation.}
  \item{LCV}{coefficient of L-variation--analogous to coe. of variation.}
  \item{TAU3}{The third L-moment ratio or L-skew--analogous to skew.}
  \item{TAU4}{The fourth L-moment ratio or L-kurtosis--analogous to kurtosis.}
  \item{TAU5}{The fifth L-moment ratio.}
  \item{L3}{The third L-moment.}
  \item{L4}{The fourth L-moment.}
  \item{L5}{The fifth L-moment.}
  \item{source}{An attribute identifying the computational 
source of the L-moments: \dQuote{lmomnor--ordinary}.}
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
\seealso{\code{\link{parnor}}, \code{\link{quanor}}, \code{\link{cdfnor}} }
\examples{
lmr <- lmom.ub(c(123,34,4,654,37,78))
lmr
lmomnor(parnor(lmr))
}
\keyword{distribution}
