\name{qualn3}
\alias{qualn3}
\title{Quantile Function of the 3-Parameter Log-Normal Distribution}
\description{
This function computes the quantiles of the Log-Normal3 distribution
given parameters (\eqn{zeta}, \eqn{\mu}, and \eqn{\sigma}) of the distribution computed by
\code{\link{parln3}}.  The quantile function of the distribution has no explicit form.
The parameters have the following interpretations: \eqn{\zeta} is a lower bounds parameter, \eqn{\mu} is a location parameter, and
\eqn{\sigma} is a scale parameter.
}
\usage{
qualn3(f, para, paracheck=TRUE)
}
\arguments{
  \item{f}{Nonexceedance probability (\eqn{0 \le F \le 1}).}
  \item{para}{The parameters from \code{\link{parln3}} or similar.}
  \item{paracheck}{A logical controlling whether the parameters and checked for validity. Overriding of this check might be extremely important and needed for use of the distribution quantile function in the context of TL-moments with nonzero trimming.}
}
\value{
  Quantile value for nonexceedance probability \eqn{F}.
}
\references{
NEED
}
\author{ W.H. Asquith}
\seealso{\code{\link{cdfln3}}, \code{\link{parln3}}}
\examples{
  lmr <- lmom.ub(c(123,34,4,654,37,78))
  qualn3(0.5,parln3(lmr))
}
\keyword{distribution}
