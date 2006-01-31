\name{pwm2lmom}
\alias{pwm2lmom}
\title{Probability-Weighted Moments to L-moments}
\description{
Converts the Probability-Weighted Moments (PWM) to the L-moments given the
PWM. The conversion is linear so procedures based on PWMs and identical to 
those based on L-moments.

\deqn{\lambda_1 = \beta_0}

\deqn{\lambda_2 = 2\beta_1 - \beta_0}

\deqn{\lambda_3 = 6\beta_2 - 6\beta_1 + \beta_0}

\deqn{\lambda_4 = 20\beta_3 - 30\beta_2 + 12\beta_1 - \beta_0}

\deqn{\lambda_5 = 70\beta_4 - 140\beta_3 + 90\beta_2 - 20\beta_1 + \beta_0}

\deqn{\tau = \lambda_2/\lambda_1}

\deqn{\tau_3 = \lambda_3/\lambda_2}

\deqn{\tau_4 = \lambda_4/\lambda_2}

\deqn{\tau_5 = \lambda_5/\lambda_2}
}
\usage{
pwm2lmom(pwm)
}
\arguments{
  \item{pwm}{ A PWM object created by \code{\link{pwm.ub}} or similar. }
}
\details{
The Probability Weighted Moments (PWMs) are linear combinations of the L-moments and
therefore contain the same statistical information of the data as the L-moments.
However, the PWMs are harder to interpret as measures of probability distributions.
}
\value{
  An R \code{list} is returned.

  \item{L1}{Arithmetic mean}
  \item{L2}{L-scale--analogous to standard deviation}
  \item{LCV}{coefficient of L-variation--analogous to coe. of variation}
  \item{TAU3}{The third L-moment ratio or L-skew--analogous to skew}
  \item{TAU4}{The fourth L-moment ratio or L-kurtosis--analogous to kurtosis}
  \item{TAU5}{The fifth L-moment ratio}
  \item{L3}{The third L-moment}
  \item{L4}{The fourth L-moment}
  \item{L5}{The fifth L-moment}
}
\references{
Greenwood, J.A., Landwehr, J.M., Matalas, N.C., and Wallis, J.R., 1979,
Probability weighted moments--Definition and relation to parameters of
several distributions expressable in inverse form: Water Resources Research,
vol. 15, p. 1,049-1,054.

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
\seealso{\code{\link{lmom.ub}}, \code{\link{pwm.ub}}, \code{\link{lmom2pwm}}}
\examples{
lmom <- pwm2lmom(pwm.ub(c(123,34,4,654,37,78)))

pwm2lmom(pwm.ub(rnorm(100)))
}
\keyword{univar}
\keyword{distribution}
