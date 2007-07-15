\name{pwm.ub}
\alias{pwm.ub}
\title{ Unbiased Sample Probability-Weighted Moments }
\description{
Unbiased sample Probability-Weighted Moments (PWMs) are computed from a sample.
The first five \eqn{\beta_r}'s are computed by default.

\deqn{\beta_r = n^{-1}\sum^n_{j=1} (j-1 \choose r)x_{j:n}}

}
\usage{
pwm.ub(x,nmom=5,sort=TRUE)
}
\arguments{
  \item{x}{A vector of data values.}
  \item{nmom}{Number of PWMs to return.}
  \item{sort}{Does the data need sorting? The computations require sorted data. This option is provided to optimize processing speed if presorted data already exists.}
}
\value{
  An R \code{list} is returned.

  \item{betas}{The PWMs. Note that convention is the have a \eqn{\beta_0}, but this is placed in the first index \code{i=1} of the \code{betas} vector.}
  \item{source}{Source of the PWMs: \dQuote{pwm.ub}}
}
\references{
Greenwood, J.A., Landwehr, J.M., Matalas, N.C., and Wallis, J.R., 1979,
Probability weighted moments---Definition and relation to parameters of
several distributions expressable in inverse form: Water Resources Research,
vol. 15, p. 1,049--1,054.

Hosking, J.R.M., 1990, L-moments---Analysis and estimation of
distributions using linear combinations of order statistics: Journal
of the Royal Statistical Society, Series B, vol. 52, p. 105--124.

Hosking, J.R.M. and Wallis, J.R., 1997, Regional frequency analysis---An
approach based on L-moments: Cambridge University Press.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{pwm.pp}}, \code{\link{pwm.gev}}, \code{\link{pwm2lmom}} }
\examples{
pwm <- pwm.ub(rnorm(20))
}
\keyword{univar}
\keyword{distribution}
