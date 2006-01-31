\name{quanor}
\alias{quanor}
\title{Quantile Function of the Normal Distribution}
\description{
This function computes the quantiles of the Normal distribution given
parameters (\eqn{mean} and \eqn{sd}) of the distribution computed by
\code{\link{parnor}}.
The quantile function of the distribution has no explicit form
(see \code{cdfnor} and \code{qnorm}). The parameters have the following 
interpretations: \eqn{mean} is the arithmetic
mean and \eqn{sd} is the standard deviation.
}
\usage{
quanor(f, para)
}
\arguments{
  \item{f}{Nonexceedance probability (\eqn{0 \le F \le 1}).}
  \item{para}{The parameters from \code{\link{parnor}} or similar.}
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
\seealso{\code{\link{cdfnor}}, \code{\link{parnor}}, \code{\link{quagno}}}
\examples{
  lmr <- lmom.ub(c(123,34,4,654,37,78))
  quanor(0.5,parnor(lmr))
}
\keyword{distribution}
