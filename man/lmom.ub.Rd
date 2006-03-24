\name{lmom.ub}
\alias{lmom.ub}
\title{ Unbiased Sample L-moments by Direct Sample Estimators }
\description{
Unbiased sample L-moments are computed for a vector using the direct sample estimation
method as opposed to the use of sample probability-weighted moments. The L-moments are the ordinary L-moments and not the trimmed L-moments (see 
\code{\link{TLmoms}}). The mean, 
L-scale, coefficient of L-variation (\eqn{\tau}, LCV, L-scale/mean),
L-skew (\eqn{\tau_3}, TAU3, L3/L2), 
L-kurtosis (\eqn{\tau_4}, TAU4, L4/L2), and \eqn{\tau_5} (TAU5, L4/L2) are computed.
In conventional nomenclature, the L-moments are

\deqn{ \hat{\lambda}_1 = \mbox{L1} = \mbox{mean, }}
\deqn{ \hat{\lambda}_2 = \mbox{L2} = \mbox{L-scale, }}
\deqn{ \hat{\lambda}_3 = \mbox{L3} = \mbox{third L-moment, }}
\deqn{ \hat{\lambda}_4 = \mbox{L4} = \mbox{fourth L-moment, }}
\deqn{ \hat{\lambda}_5 = \mbox{L5} = \mbox{fifth L-moment, }}
\deqn{ \hat{\tau} = \mbox{LCV} = \lambda_2/\lambda_1 = \mbox{coefficient of L-variation, }}
\deqn{ \hat{\tau}_3 = \mbox{TAU3} = \lambda_3/\lambda_2 = \mbox{L-skew, }}
\deqn{ \hat{\tau}_4 = \mbox{TAU4} = \lambda_4/\lambda_2 = \mbox{L-kurtosis, and}}
\deqn{ \hat{\tau}_5 = \mbox{TAU5} = \lambda_5/\lambda_2 = \mbox{not named.}}
}
\usage{
lmom.ub(x)
}
\arguments{
  \item{x}{A vector of data values.}
}
\details{
The L-moment ratios (\eqn{\tau}, \eqn{\tau_3}, \eqn{\tau_4}, and \eqn{\tau_5})
are the primary higher L-moments for application, such as for distribution parameter estimation.
However, the actual L-moments (\eqn{\lambda_3}, \eqn{\lambda_4}, and \eqn{\lambda_5}) are also
reported. This implementation of L-moment calculation requires a minimum of five data points.
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
  \item{source}{An attribute identifying the computational source of the L-moments: \dQuote{lmom.ub--ordinary}.}
}
\source{
The Perl code base of W.H. Asquith
}
\references{
Hosking, J.R.M., 1990, L-moments---Analysis and estimation of
distributions using linear combinations of order statistics: Journal
of the Royal Statistical Society, Series B, vol. 52, p. 105--124.

Hosking, J.R.M., 1996, FORTRAN routines for use with the method of L-moments:
Version 3, IBM Research Report RC20525, T.J. Watson Research Center,
Yorktown Heights, New York.

Hosking, J.R.M. and Wallis, J.R., 1997, Regional frequency analysis---An
approach based on L-moments: Cambridge University Press.

Wang, Q.J., 1996b, Direct sample estimators of L-moments: Water Resources
Research, vol. 32, no. 12., pp. 3617--3619.
}
\author{ W.H. Asquith}

\seealso{\code{\link{lmom2pwm}}, \code{\link{pwm.ub}}, \code{\link{pwm2lmom}}}
\examples{
lmr <- lmom.ub(c(123,34,4,654,37,78))

lmom.ub(rnorm(100))
}
\keyword{univar}
\keyword{distribution}
