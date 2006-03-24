\name{cdfpe3}
\alias{cdfpe3}
\title{Cumulative Distribution Function of the Pearson Type III Distribution}
\description{
This function computes the cumulative probability or nonexceedance probability
of the Pearson Type III distribution given parameters (\eqn{\xi}, \eqn{\alpha},
and \eqn{\gamma}) of the distribution computed
by \code{\link{parpe3}}. The cumulative distribution function of the
distribution is

\deqn{F(x) = \frac{G\left(\alpha,\frac{x - \xi}{\gamma}\right)}{\Gamma(\alpha)} \mbox{,}}

where \eqn{F(x)} is the nonexceedance probability for quantile \eqn{x},
\eqn{G} is the incomplete gamma function, \eqn{\Gamma} is the gamma function,
\eqn{\xi} is a location parameter, \eqn{\alpha} is a scale parameter, and
\eqn{\gamma} is a shape parameter.
}
\usage{
cdfpe3(x, para)
}
\arguments{
  \item{x}{A real value.}
  \item{para}{The parameters from \code{\link{parpe3}} or similar.}
}

\value{
  Nonexceedance probability (\eqn{F}) for \eqn{x}.
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
\seealso{\code{\link{quape3}}, \code{\link{parpe3}}}
\examples{
  lmr <- lmom.ub(c(123,34,4,654,37,78))
  cdfpe3(50,parpe3(lmr))
}
\keyword{distribution}
