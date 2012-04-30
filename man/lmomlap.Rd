\name{lmomlap}
\alias{lmomlap}
\title{L-moments of the Laplace Distribution}
\description{
This function estimates the L-moments of the Laplace distribution given the parameters
(\eqn{\xi} and \eqn{\alpha}) from \code{\link{parlap}}.
The L-moments in terms of the parameters are

\deqn{\lambda_1 = \xi \mbox{,}}
\deqn{\lambda_2 = \frac{3 \alpha}{4} \mbox{,}}
\deqn{\tau_3 = 0 \mbox{,}}
\deqn{\tau_4 = \frac{17}{22} \mbox{,}}
\deqn{\tau_5 = 0 \mbox{,}}
\deqn{\tau_6 = \frac{31}{360} \mbox{.}}

For \eqn{r} odd and \eqn{r \ge 3}, \eqn{\lambda_r = 0}, and for \eqn{r} even and \eqn{r \ge 4}, the L-moments using the hypergeometric function \eqn{{}_2F_1()} are

\deqn{\lambda_r = \frac{2\alpha}{r(r-1)}[1 - {}_2F_1(-r, r-1, 1, 1/2)]}

}
\usage{
lmomlap(para)
}
\arguments{
  \item{para}{The parameters of the distribution.}
}
\value{
  An R \code{list} is returned.

  \item{lambdas}{The L-moments}
  \item{ratios}{The L-moment ratios.}
  \item{source}{An attribute identifying the computational source of the L-moments: \dQuote{lmomlap}.}
}
\references{
Hosking, J.R.M., 1986, The theory of probability weighted moments: IBM Research Report RC12210, T.J. Watson Research Center,
Yorktown Heights, New York.
}
\author{ W.H. Asquith}
\seealso{\code{\link{parlap}}, \code{\link{qualap}}, \code{\link{cdflap}} }
\examples{
lmr <- lmom.ub(c(123,34,4,654,37,78))
lmr
lmomlap(parlap(lmr))
}
\keyword{distribution}
