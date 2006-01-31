\name{vec2pwm}
\alias{vec2pwm}
\title{Convert a Vector of Probability-Weighted Moments to a Probability-Weighted Moments Object}
\description{
This function converts a vector of Probability-Weighted Moments (PWM) to a PWM
object of this package. The object is an R \code{list}. This function is intended
to facilitate the use of PWM that the user might have from other sources. The
first five PWMs are supported (\eqn{\beta_0}, \eqn{\beta_1}, \eqn{\beta_2}, 
\eqn{\beta_3}, \eqn{\beta_4}).
}
\usage{
vec2pwm(vec)
}
\arguments{
  \item{vec}{A vector of PWM values in (\eqn{\beta_0}, \eqn{\beta_1}, \eqn{\beta_2}, 
\eqn{\beta_3}, \eqn{\beta_4}) order.}
}
\value{
  An R \code{list} is returned.

  \item{BETA0}{The first PWM--equal to the arithmetic mean.}
  \item{BETA1}{The second PWM.}
  \item{BETA2}{The third PWM.}
  \item{BETA3}{The fourth PWM.}
  \item{BETA4}{The fifth PWM.}
}
\author{ W.H. Asquith}
\seealso{\code{\link{vec2lmom}}}
\examples{
pwm <- vec2pwm(c(12,123,12,12,54))
}
\keyword{distribution}
