\name{quakur}
\alias{quakur}
\title{Quantile Function of the Kumaraswamy Distribution}
\description{
This function computes the quantiles \eqn{0 < x < 1} of the Kumaraswamy distribution given
parameters (\eqn{\alpha} and \eqn{\beta}) of the distribution computed by
\code{\link{parkur}}.
The quantile function of the distribution is

\deqn{x(F) = (1 - (1-F)^{1/\beta})^{1/\alpha} \mbox{,}}

where \eqn{x(F)} is the quantile for nonexceedance probability \eqn{F},
\eqn{\alpha} is a shape parameter, and \eqn{\beta} is a shape parameter.
}
\usage{
quakur(f, para)
}
\arguments{
  \item{f}{Nonexceedance probability (\eqn{0 \le F \le 1}).}
  \item{para}{The parameters from \code{\link{parkur}} or similar.}
}
\value{
  Quantile value for nonexceedance probability \eqn{F}.
}
\references{
Jones, M.C., 2009, Kumaraswamy's distribution---A beta-type distribution with
some tractability advantages: Statistical Methodology, v.6, pp. 70--81.
}
\author{ W.H. Asquith}
\seealso{\code{\link{cdfkur}}, \code{\link{parkur}}}
\examples{
  lmr <- lmom.ub(c(0.25, 0.4, 0.6, 0.65, 0.67, 0.9))
  quakur(0.5,parkur(lmr))
}
\keyword{distribution}
