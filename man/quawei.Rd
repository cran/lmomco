\name{quawei}
\alias{quawei}
\title{Quantile Function of the Weibull Distribution}
\description{
This function computes the quantiles of the Weibull
distribution given parameters (\eqn{\zeta}, \eqn{\beta}, and \eqn{\delta}) of the
distribution computed by \code{\link{parwei}}. The quantile function of the
distribution is

\deqn{x(F) = \beta[- \log(1-F)]^{1/\delta} - \zeta \mbox{,}}

where \eqn{x(F)} is the quantile for nonexceedance probability \eqn{F},
\eqn{\zeta} is a location parameter, \eqn{\beta} is a scale parameter, and
\eqn{\delta} is a shape parameter.

The Weibull distribution is a reverse Generalized Extreme Value distribution.  As result, the Generalized Extreme Value algorithms are used for implementation of the Weibull in this package. The relation between the Generalized Extreme Value distribution parameters (\eqn{\xi}, \eqn{\alpha}, and \eqn{\kappa}) is

\deqn{\kappa = 1/\delta \mbox{,}}

\deqn{\alpha = \beta/\delta \mbox{, and}}

\deqn{\xi = \zeta - \beta \mbox{.}}

These relations are taken from Hosking and Wallis (1997).

In R the quantile function of the Weibull distribution is \code{qweibull}. Given a Weibull parameter object \code{para}, the R syntax is \code{qweibull(f,para$para[3],scale=para$para[2]) - para$para[1]}. For the current implementation for this package, the reversed Generalized Extreme Value distribution is used \code{-quagev((1-f),para)}.
}
\usage{
quawei(f, para)
}
\arguments{
  \item{f}{Nonexceedance probability (\eqn{0 \le F \le 1}).}
  \item{para}{The parameters from \code{\link{parwei}} or similar.}
}
\value{
  Quantile value for nonexceedance probability \eqn{F}.
}
\references{
Hosking, J.R.M. and Wallis, J.R., 1997, Regional frequency analysis---An
approach based on L-moments: Cambridge University Press.
}
\author{ W.H. Asquith}
\seealso{\code{\link{cdfwei}}, \code{\link{parwei}}}
\examples{
  lmr <- lmom.ub(c(123,34,4,654,37,78))
  WEI <- parwei(lmr)
  Q1  <- quawei(0.5,WEI)
  Q2  <- qweibull(0.5,shape=WEI$para[3],scale=WEI$para[2])-WEI$para[1]
  if(Q1 == Q2) EQUAL <- TRUE
}
\keyword{distribution}
