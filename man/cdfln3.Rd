\name{cdfln3}
\alias{cdfln3}
\title{Cumulative Distribution Function of the 3-Parameter Log-Normal Distribution}
\description{
This function computes the cumulative probability or nonexceedance probability
of the Log-Normal3 distribution given parameters (\eqn{\zeta}, \eqn{\mu},
and \eqn{\sigma}) of the distribution computed by \code{\link{parln3}}.
The cumulative distribution function of the distribution is

\deqn{F(x) = \Phi(y) \mbox{,} }
where \eqn{\Phi} is the cumulative ditribution function of the
standard normal distribution and \eqn{y} is
}
\usage{
cdfln3(x, para)
}
\arguments{
  \item{x}{A real value.}
  \item{para}{The parameters from \code{\link{parln3}} or similar.}
}

\value{
  Nonexceedance probability (\eqn{F}) for \eqn{x}.
}
\references{
NEED
}
\author{ W.H. Asquith}
\seealso{\code{\link{qualn3}}, \code{\link{parln3}}}
\examples{
  lmr <- lmom.ub(c(123,34,4,654,37,78))
  cdfln3(50,parln3(lmr))
}
\keyword{distribution}
