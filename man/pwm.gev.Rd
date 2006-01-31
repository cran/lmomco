\name{pwm.gev}
\alias{pwm.gev}
\title{ Generalized Extreme Value Plotting Position Probability-Weighted Moments }
\description{
Generalized Extreme Value plotting position Probability-Weighted Moments
(PWMs) are computed from a sample.  The first five \eqn{\beta_r}'s are computed.
The plotting position formula for the Generalized Extreme Value distribution is

\deqn{p_i = \frac{i-0.35}{n}}

where \eqn{pp_i} is the nonexceedance probability \eqn{F} of the \eqn{i}th ascending
data values.  The parameters \eqn{A} and \eqn{B} together specify the plotting
position type, and \eqn{n} is the sample size. The PWMs are computed by

\deqn{\beta_r = n^{-1}\sum_{i=1}^{n}p_i^r \times X_{j:n}}

where \eqn{X_{j:n}} is the \eqn{j}th order statistic
\eqn{X_{1:n} \le X_{2:n} \le X_{j:n} \dots \le X_{n:n}} of random variable X, and
\eqn{r} is \eqn{0, 1, 2, \dots}.

Finally, \code{pwm.gev} dispatches to \code{pwm.pp(data,A=-0.35,B=0)} and does not 
have its own logic.
}
\usage{
pwm.gev(x)
}
\arguments{
  \item{x}{A vector of data values.}
}
\value{
  An R \code{list} is returned.

  \item{BETA0}{The first PWM--equal to the arithmetic mean.}
  \item{BETA1}{The second PWM.}
  \item{BETA2}{The third PWM.}
  \item{BETA3}{The fourth PWM.}
  \item{BETA4}{The fifth PWM.}
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
\seealso{ \code{\link{pwm.ub}}, \code{\link{pwm.pp}}, \code{\link{pwm2lmom}} }
\examples{
pwm <- pwm.gev(rnorm(20))
}
\keyword{univar}
\keyword{distribution}
