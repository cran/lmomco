\name{quagpa}
\alias{quagpa}
\title{Quantile Function of the Generalized Pareto Distribution}
\description{
This function computes the quantiles of the Generalized Pareto distribution
given parameters (\eqn{\xi}, \eqn{\alpha}, and \eqn{\kappa}) of the distribution computed
by \code{\link{pargpa}}. The quantile function of the distribution is

\deqn{x(F) = \xi + \frac{\alpha}{\kappa} \left( 1-(1-F)^\kappa \right)
   \mbox{ for } \kappa \ne 0 \mbox{ and }}

\deqn{x(F) = \xi - \alpha\log(1-F) \mbox{ for } \kappa = 0 \mbox{,}}

where \eqn{x(F)} is the quantile for nonexceedance probability \eqn{F},
\eqn{\xi} is a location parameter, \eqn{\alpha} is a scale parameter, and
\eqn{\kappa} is a shape parameter.
}
\usage{
quagpa(f, para, paracheck=TRUE)
}
\arguments{
  \item{f}{Nonexceedance probability (\eqn{0 \le F \le 1}).}
  \item{para}{The parameters from \code{\link{pargpa}} or similar.}
  \item{paracheck}{A logical controlling whether the parameters and checked for validity. Overriding of this check might be extremely important and needed for use of the distribution quantile function in the context of TL-moments with nonzero trimming.}
}
\value{
  Quantile value for nonexceedance probability \eqn{F}.
}
\references{
Hosking, J.R.M., 1990, L-moments---Analysis and estimation of
distributions using linear combinations of order statistics: Journal
of the Royal Statistical Society, Series B, vol. 52, p. 105--124.

Hosking, J.R.M., 1996, FORTRAN routines for use with the method of L-moments:
Version 3, IBM Research Report RC20525, T.J. Watson Research Center,
Yorktown Heights, New York.

Hosking, J.R.M. and Wallis, J.R., 1997, Regional frequency analysis---An
approach based on L-moments: Cambridge University Press.
}
\author{ W.H. Asquith}
\seealso{\code{\link{cdfgpa}}, \code{\link{pargpa}}}
\examples{
  lmr <- lmom.ub(c(123,34,4,654,37,78))
  quagpa(0.5,pargpa(lmr))
}
\keyword{distribution}
\keyword{quantile function}
