\name{lmomray}
\alias{lmomray}
\title{L-moments of the Rayleigh Distribution}
\description{
This function estimates the L-moments of the Rayleigh distribution given
the parameters (\eqn{\xi} and \eqn{\alpha}) from \code{\link{parray}}.
The L-moments in terms of the parameters are

\deqn{\lambda_1 = \xi + \alpha\sqrt{\pi/2} \mbox{,}}
\deqn{\lambda_2 = \frac{1}{2} \alpha(\sqrt{2} - 1)\sqrt{\pi}\mbox{,}}
\deqn{\tau_3 = \frac{1 - 3/\sqrt{2} + 2/\sqrt{3}}{1 - 1/\sqrt{2}} = 0.1140 \mbox{, and}}
\deqn{\tau_4 = \frac{1 - 6/\sqrt{2} +  10/\sqrt{3} - 5\sqrt{4}}{1 - 1/\sqrt{2}} = 0.1054 \mbox{.}}

}
\usage{
lmomray(para)
}
\arguments{
  \item{para}{The parameters of the distribution.}
}
\value{
  An R \code{list} is returned.

  \item{L1}{Arithmetic mean.}
  \item{L2}{L-scale---analogous to standard deviation.}
  \item{LCV}{coefficient of L-variation---analogous to coe. of variation.}
  \item{TAU3}{The third L-moment ratio or L-skew--analogous to skew.}
  \item{TAU4}{The fourth L-moment ratio or L-kurtosis---analogous to kurtosis.}
  \item{TAU5}{The fifth L-moment ratio.}
  \item{L3}{The third L-moment.}
  \item{L4}{The fourth L-moment.}
  \item{L5}{The fifth L-moment.}
  \item{source}{An attribute identifying the computational
source of the L-moments: \dQuote{lmomray}.}
}
\references{
Hosking, J.R.M., 1986, The theory of probability weighted moments:
Research Report RC12210, IBM Research Division, Yorkton Heights, N.Y.
}
\author{ W.H. Asquith}
\seealso{\code{\link{parray}}, \code{\link{quaray}}, \code{\link{cdfray}} }
\examples{
lmr <- lmom.ub(c(123,34,4,654,37,78))
lmr
lmomray(parray(lmr))
}
\keyword{distribution}
