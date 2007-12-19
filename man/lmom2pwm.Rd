\name{lmom2pwm}
\alias{lmom2pwm}
\title{L-moments to Probability-Weighted Moments}
\description{
Converts the L-moments to the Probability-Weighted Moments (PWMs)
given the L-moments. The conversion is linear so procedures based on 
L-moments are identical to those based on PWMs. The relation between L-moments
and PWMs is shown with \code{\link{pwm2lmom}}.
}
\usage{
lmom2pwm(lmom)
}
\arguments{
  \item{lmom}{ An L-moment object created by \code{\link{lmom.ub}} or similar. The function also supports \code{lmom} as a vector of L-moments (\eqn{\lambda_1}, \eqn{\lambda_2}, \eqn{\tau_3}, \eqn{\tau_4}, and \eqn{\tau_5}).}
}
\details{
PWMs are linear combinations of the L-moments and
therefore contain the same statistical information of the data as the L-moments.
However, the PWMs are harder to interpret as measures of probability distributions.
The PWMs are included here for theoretical completeness and are not intended for
use with the majority of the other functions implementing the various probability
distributions. The relation between L-moments (\eqn{\lambda_r}) and PWMs (\eqn{\beta_{r-1}}) for \eqn{1 \le r \le 5} order is

 \deqn{\lambda_1 = \beta_0 \mbox{,}}
 
 \deqn{\lambda_2 = 2\beta_1 - \beta_0 \mbox{,}}
 
 \deqn{\lambda_3 = 6\beta_2 - 6\beta_1 + \beta_0 \mbox{,}}
 
 \deqn{\lambda_4 = 20\beta_3 - 30\beta_2 + 12\beta_1 - \beta_0\mbox{, and}}

 \deqn{\lambda_5 = 70\beta_4  - 140\beta_3 + 90\beta_2 - 20\beta_1 + \beta_0\mbox{.}}

The linearity between L-moments and PWMs means that procedures based on one are equivalent to the other. This function only accomodates the first five L-moments and PWMs. Therefore, at least five L-moments are required in the passed argument.
}
\value{
  An R \code{list} is returned.

  \item{betas}{The PWMs. Note that convention is the have a \eqn{\beta_0}, but this is placed in the first index \code{i=1} of the \code{betas} vector.}
  \item{source}{Source of the PWMs: \dQuote{pwm}}
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
\author{ W.H. Asquith }
\seealso{\code{\link{lmom.ub}}, \code{\link{pwm.ub}}, \code{\link{pwm2lmom}}}
\examples{
pwm <- lmom2pwm(lmom.ub(c(123,34,4,654,37,78)))

lmom2pwm(lmom.ub(rnorm(100)))

lmom2pwm(lmoms(rnorm(100)))


lmomvec1 <- c(1000,1300,0.4,0.3,0.2,0.1)
pwmvec   <- lmom2pwm(lmomvec1)
print(pwmvec)
#$betas
#[1] 1000.0000 1150.0000 1070.0000  984.5000  911.2857
#
#$source
#[1] "lmom2pwm"


lmomvec2 <- pwm2lmom(pwmvec)
print(lmomvec2)
#$lambdas
#[1] 1000 1300  520  390  260
#
#$ratios
#[1]  NA 1.3 0.4 0.3 0.2
#
#$source
#[1] "pwm2lmom"


pwm2lmom(lmom2pwm(list(L1=25,L2=20,TAU3=.45,TAU4=0.2,TAU5=0.1)))
}
\keyword{univar}
\keyword{distribution}
