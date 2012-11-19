\name{parlap}
\alias{parlap}
\title{Estimate the Parameters of the Laplace Distribution}
\description{
This function estimates the parameters of the Generalized Logistic distribution given
the L-moments of the data in an L-moment object such as that returned by
\code{lmom.ub}. The relation between distribution parameters and sample L-moments using the best available combination of the lower L-moments are
\deqn{\xi = \lambda_1 - 50/31\times\lambda_3 \mbox{, and}}
\deqn{\alpha = 1.4741\lambda_2 - 0.5960\lambda_4 \mbox{.}}

Although by simplist definition, one would have \eqn{\xi = \lambda_1} and \eqn{\alpha = \frac{4}{3}\lambda_2}.
}
\usage{
parlap(lmom,checklmom=TRUE)
}
\arguments{
  \item{lmom}{A L-moment object created by \code{\link{lmom.ub}}
or \code{\link{pwm2lmom}}.}
  \item{checklmom}{Should the \code{lmom} be checked for validity using the \code{are.lmom.valid} function. Normally this should be left as the default and it is very unlikely that the L-moments will not be viable (particularly in the \eqn{\tau_4} and \eqn{\tau_3} inequality). However, for some circumstances or large simulation exercises then one might want to bypass this check.}
}

\value{
  An R \code{list} is returned.

  \item{type}{The type of distribution: \code{lap}.}
  \item{para}{The parameters of the distribution.}
  \item{source}{The source of the parameters: \dQuote{parlap}.}
}
\references{
Hosking, J.R.M., 1986, The theory of probability weighted moments: IBM Research Report RC12210, T.J. Watson Research Center,
Yorktown Heights, New York.
}
\author{ W.H. Asquith}
\seealso{\code{\link{lmom.ub}}, \code{\link{lmomlap}},
\code{\link{cdflap}}, \code{\link{qualap}}
}
\examples{
lmr <- lmom.ub(rnorm(20))
parlap(lmr)
}
\keyword{distribution}
