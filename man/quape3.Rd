\name{quape3}
\alias{quape3}
\title{Quantile Function of the Pearson Type III Distribution}
\description{
This function computes the quantiles of the Pearson Type III distribution given
parameters (\eqn{\xi}, \eqn{\alpha}, and \eqn{\gamma}) of the distribution computed by
\code{\link{parpe3}}. The
quantile function of the distribution has no explicit form (see
\code{\link{cdfpe3}}). The parameters have the following interpretations: \eqn{\xi} is a location parameter, \eqn{\alpha} is a scale parameter, and
\eqn{\gamma} is a shape parameter.
}
\usage{
quape3(f, para)
}
\arguments{
  \item{f}{Nonexceedance probability (\eqn{0 \le F \le 1}).}
  \item{para}{The parameters from \code{\link{parpe3}} or similar.}
}
\value{
  Quantile value for nonexceedance probability \eqn{F}.
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
\seealso{\code{\link{cdfpe3}}, \code{\link{parpe3}}}
\examples{
  lmr <- lmom.ub(c(123,34,4,654,37,78))
  quape3(0.5,parpe3(lmr))
}
\keyword{distribution}
