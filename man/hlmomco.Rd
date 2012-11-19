\name{hmomco}
\alias{hlmomco}
\title{Hazard Functions of the Distributions}
\description{
This function acts as a front end to \code{\link{dlmomco}} and \code{\link{plmomco}} to compute the hazard function \eqn{h(x)} or conditional failure rate. The function is defined by

\deqn{h(x) = \frac{f(x)}{1 - F(x)}\mbox{,}}

where \eqn{f(x)} is a probability density function and \eqn{F(x)} is the cumulative distribution function.

To help with intuitive understanding of what \eqn{h(x)} means (Ugarte and others, 2008), let \eqn{dx} represent a small unit of measurement. Then the quantity \eqn{h(x)dx} can be conceptualized as the approximate probability that random variable \eqn{X} takes on a value in the interval \eqn{[x, x+dx]}.

Ugarte and others (2008) continue by stating that \eqn{h(x)} represents the instantaneous rate of death or failure at time \eqn{x}, given the survival to time \eqn{x} has occurred. Emphasis is needed that \eqn{h(x)} is a rate of probability change and not a probability itself.
}
\usage{
hlmomco(x,para)
}
\arguments{
  \item{x}{A real value.}
  \item{para}{The parameters from \code{\link{lmom2par}} or similar.}
}
\value{
  Hazard rate for \code{x}.
}
\references{
Ugarte, M.D., Militino, A.F., and Arnholt, A.T., 2008, Probability and statistics with R: Boca Raton, FL., CRC Press, 700 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{plmomco}}, \code{\link{dlmomco}}}
\examples{
my.lambda <- 100
para <- vec2par(c(0,my.lambda), type="exp")

x <- seq(40:60)
hlmomco(x,para) # returns vector of 0.01 
# because the exponential distribution has a constant 
# failure rate equal to 1/scale or 1/100 as in this example.
}
\keyword{distribution}
