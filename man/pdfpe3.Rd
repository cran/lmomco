\name{pdfpe3}
\alias{pdfpe3}
\title{Probability Density Function of the Pearson Type III Distribution}
\description{
This function computes the probability density of the Pearson Type III distribution given parameters (\eqn{\mu}, \eqn{\sigma}, and \eqn{\gamma})  computed by \code{\link{parpe3}}. These parameters are equal to the product moments: mean, standard deviation, and skew (see \code{\link{pmoms}}). The probability density function for \eqn{\gamma \ne 0} is
\deqn{f(x) = \frac{Y^{\alpha -1} \exp({-Y/\beta})}
                  {\beta^\alpha\, \Gamma(\alpha)} \mbox{,}}
where \eqn{f(x)} is the probability density for quantile \eqn{x}, \eqn{G} is defined below and is related to the incomplete gamma function in  \R  as \code{pgamma}, \eqn{\Gamma} is the complete gamma function in \R as \code{gamma},
\eqn{\xi} is a location parameter, \eqn{\beta} is a scale parameter,
\eqn{\alpha} is a shape parameter, and \eqn{Y = x - \xi} if \eqn{\gamma > 0} and \eqn{Y = \xi - x} if \eqn{\gamma < 0} These three \dQuote{new} parameters are related to the product moments (\eqn{\mu}, mean; \eqn{\sigma}, standard deviation; \eqn{\gamma}, skew) by
\deqn{\alpha = 4/\gamma^2 \mbox{,}}
\deqn{\beta  = \frac{1}{2}\sigma |\gamma| \mbox{,\ and}}
\deqn{\xi = \mu - 2\sigma/\gamma \mbox{.}}

The function \eqn{G(\alpha,x)} is
\deqn{G(\alpha,x) = \int_0^x t^{(a-1)} \exp(-t)\, \mathrm{d}t \mbox{.}}

If \eqn{\gamma = 0}, the distribution is symmetrical and simply is the probability density normal distribution with mean and standard deviation of \eqn{\mu} and \eqn{\sigma}, respectively. Internally, the \eqn{\gamma = 0} condition is implemented by \R function \code{pnorm}.
}
\usage{
pdfpe3(x, para)
}
\arguments{
  \item{x}{A real value vector.}
  \item{para}{The parameters from \code{\link{parpe3}} or \code{\link{vec2par}}.}
}
\value{
  Probability density (\eqn{f}) for \eqn{x}.
}
\references{
Hosking, J.R.M., 1990, L-moments---Analysis and estimation of
distributions using linear combinations of order statistics: Journal
of the Royal Statistical Society, Series B, v. 52, pp. 105--124.

Hosking, J.R.M., 1996, FORTRAN routines for use with the method of L-moments:
Version 3, IBM Research Report RC20525, T.J. Watson Research Center,
Yorktown Heights, New York.

Hosking, J.R.M., and Wallis, J.R., 1997, Regional frequency analysis---An
approach based on L-moments: Cambridge University Press.
}
\author{ W.H. Asquith}
\seealso{\code{\link{cdfpe3}}, \code{\link{quape3}}, \code{\link{lmompe3}}, \code{\link{parpe3}}}
\examples{
  lmr <- lmoms(c(123,34,4,654,37,78))
  pe3 <- parpe3(lmr)
  x <- quape3(0.5,pe3)
  pdfpe3(x,pe3)
}
\keyword{distribution}
\keyword{probability density function}
\keyword{Distribution: Pearson Type III}
