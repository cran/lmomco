\name{lmomrevgum}
\alias{lmomrevgum}
\title{L-moments of the Reverse Gumbel Distribution}
\description{
This function estimates the L-moments of the Reverse Gumbel distribution given
the parameters (\eqn{\xi} and \eqn{\alpha}) from \code{\link{parrevgum}}.
The type-B L-moments in terms of the parameters are

\deqn{\lambda^B_1 = \xi - (0.5722\dots) \alpha  - \alpha\lbrace\mathrm{Ei}(-\log(1-\zeta))\rbrace\mbox{,}}
\deqn{\lambda^B_2 = \alpha\lbrace\log(2) + \mathrm{Ei}(-2\log(1-\zeta)) - \mathrm{Ei}(-\log(1-\zeta))\rbrace\mbox{,}}
\deqn{\tau_3 =  \mbox{,}}
\deqn{\tau_4 =  \mbox{, and}}
\deqn{\tau_5 = \mbox{.}}

where \eqn{\zeta} is the right-tail censoring fraction of the sample or the nonexceedance probability of the right-tail censoring threshold, and \eqn{\mathrm{Ei}(x)} is the exponential integral defined as

\deqn{ \mathrm{Ei}(X) = \int_X^{\infty} x^{-1}e^{-x}\mathrm{d}x \mbox{,}}

where \eqn{\mathrm{Ei}(-\log(1-\zeta)) \rightarrow 0} as \eqn{\zeta \rightarrow 1} and \eqn{\mathrm{Ei}(-\log(1-\zeta))} can not be evaluated as \eqn{\zeta \rightarrow 0}.
}
\usage{
lmomrevgum(para)
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
  \item{zeta}{Number of samples observed (noncensored) divided by the total number of samples.}
  \item{source}{An attribute identifying the computational
source of the L-moments: \dQuote{lmomrevgum}.}
}
\references{
Hosking, J.R.M., 1990, L-moments---Analysis and estimation of
distributions using linear combinations of order statistics: Journal
of the Royal Statistical Society, Series B, vol. 52, p. 105--124.

Hosking, J.R.M., 1995, The use of L-moments in the analysis of censored data,
in Recent Advances in Life-Testing and Reliability, edited by N. Balakrishnan,
chapter 29, CRC Press, Boca Raton, Fla., pp. 546--560.
}
\author{ W.H. Asquith}
\seealso{\code{\link{parrevgum}}, \code{\link{quarevgum}}, \code{\link{cdfrevgum}} }
\examples{
lmr <- lmom.ub(c(123,34,4,654,37,78))
rev.para <- lmom2par(lmr,type='revgum')
lmomrevgum(rev.para)
}
\keyword{L-moment (distribution)}
