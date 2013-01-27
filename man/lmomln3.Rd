\name{lmomln3}
\alias{lmomln3}
\title{L-moments of the 3-Parameter Log-Normal Distribution}
\description{
This function estimates the L-moments of the Log-Normal3 distribution given the parameters
(\eqn{\xi}, \eqn{\alpha}, and \eqn{\kappa}) from \code{\link{parln3}}.
The L-moments in terms of the parameters are

\deqn{\lambda_1 = \xi + \frac{\alpha}{\kappa}(1-e^{\kappa^2/2}) \mbox{ and}}
\deqn{\lambda_2 = \frac{\alpha}{\kappa}(e^{\kappa^2/2})(1-2\Phi(-\kappa/\sqrt{2})) \mbox{,}}

where \eqn{\Phi} is the cumulative distribution of the standard normal distribution.
There are no simple expressions for \eqn{\tau_3}, \eqn{\tau_4}, and \eqn{\tau_5}. Log transformation of the data prior to fitting of the
Generalized Normal distribution is not required.
}
\usage{
lmomln3(para)
}
\arguments{
  \item{para}{The parameters of the distribution.}
}
\value{
  An R \code{list} is returned.

  \item{L1}{Arithmetic mean.}
  \item{L2}{L-scale---analogous to standard deviation.}
  \item{LCV}{coefficient of L-variation---analogous to coe. of variation.}
  \item{TAU3}{The third L-moment ratio or L-skew---analogous to skew.}
  \item{TAU4}{The fourth L-moment ratio or L-kurtosis---analogous to kurtosis.}
  \item{TAU5}{The fifth L-moment ratio.}
  \item{L3}{The third L-moment.}
  \item{L4}{The fourth L-moment.}
  \item{L5}{The fifth L-moment.}
  \item{source}{An attribute identifying the computational
source of the L-moments: \dQuote{lmomln3}.}
}
\references{
NEED
}
\author{ W.H. Asquith}
\seealso{\code{\link{parln3}}, \code{\link{qualn3}}, \code{\link{cdfln3}} }
\examples{
lmr <- lmom.ub(c(123,34,4,654,37,78))
lmr
lmomln3(parln3(lmr))
}
\keyword{L-moment (distribution)}
