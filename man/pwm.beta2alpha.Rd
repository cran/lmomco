\name{pwm.beta2alpha}
\alias{pwm.beta2alpha}
\alias{pwm.alpha2beta}
\title{ Conversion of Beta to Alpha Probability-Weighted Moments (PWMs) or Alpha to Beta PWMs }
\description{
Conversion of Beta to Alpha Probability-Weighted Moments (PWMs) by \code{pwm.beta2alpha()} or Alpha to Beta PWMs by \code{pwm.alpha2beta()}. The relations between the \eqn{\alpha} and \eqn{\beta} PWMs are

\deqn{\alpha_r = \sum^r_{k=0} (-1)^k {r \choose k} \beta_k}

and

\deqn{\beta_r = \sum^r_{k=0} (-1)^k {r \choose k} \alpha_k}

Lastly, note that the \eqn{\beta} are almost exclusively used in the literature. Since each is a linear combination of the other, they are equivalent in meaning but not numerically.
}
\usage{
pwm.beta2alpha(pwm)

pwm.alpha2beta(pwm)
}
\arguments{
  \item{pwm}{A vector of beta probability-weighted moments.}
}
\value{
  A vector of the \eqn{\alpha_r}. Note that convention is the have a \eqn{\alpha_0}, but this is placed in the first index \code{i=1} vector. Alternatively, a vector of the \eqn{\beta_r}.
}
\references{
Hosking, J.R.M., 1986, The theory of probability weighted moments: Research Report RC12210, IBM Research Division, Yorktown Heights, New York, reissued with corrections April 1989, 160 p.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{pwm}}, \code{\link{pwm2lmom}} }
\examples{
X <- rnorm(100)
pwm(X)$betas
pwm.beta2alpha(pwm(X)$betas)
pwm.alpha2beta(pwm.beta2alpha(pwm(X)$betas))
}
\keyword{univar}
