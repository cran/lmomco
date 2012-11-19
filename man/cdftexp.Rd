\name{cdftexp}
\alias{cdftexp}
\title{Cumulative Distribution Function of the Truncated Exponential Distribution}
\description{
This function computes the cumulative probability or nonexceedance probability
of the Truncated Exponential distribution given parameters (\eqn{\xi} and \eqn{\alpha}) of the
distribution computed by \code{\link{partexp}}. The cumulative distribution
function of the distribution is

\deqn{F(x) = \frac{1-\mathrm{exp}(-t/\alpha)}{1-\mathrm{exp}(-\xi/\alpha)}\mbox{,}}

where \eqn{F(x)} is the nonexceedance probability for the quantile \eqn{x},
\eqn{\xi} is a location parameter, \eqn{\alpha} is a scale parameter, and \eqn{0 \le x \le \xi}.  The distribution has \eqn{0 < \tau_2 <= 1/2}, \eqn{\xi > 0}, and \eqn{1/\alpha \ne 0}.
}
\usage{
cdftexp(x, para)
}
\arguments{
  \item{x}{A real value.}
  \item{para}{The parameters from \code{\link{partexp}} or similar.}
}

\value{
  Nonexceedance probability (\eqn{F}) for \eqn{x}.
}
\references{
Vogel, R.M., Hosking, J.R.M., Elphick, C.S., Roberts, D.L., and Reed, J.M., 2008, Goodness of fit of probability distributions for sightings as species approach extinction: Bulletin of Mathematial Biology, v. 71, no. 3, pp. 701--719.
}
\author{ W.H. Asquith}
\seealso{\code{\link{pdftexp}}, \code{\link{quatexp}}, \code{\link{partexp}}}
\examples{
  lmr <- vec2lmom(c(40,0.38), lscale=FALSE)
  cdftexp(50,partexp(lmr))

  # Vogel and others (2008) example sighting times for the bird
  # Eskimo Curlew, inspection shows that these are fairly uniform.
  # There is a sighting about every year to two.
  T <- c(1946, 1947, 1948, 1950, 1955, 1956, 1959, 1960, 1961,
         1962, 1963, 1964, 1968, 1970, 1972, 1973, 1974, 1976,
         1977, 1980, 1981, 1982, 1982, 1983, 1985)
  R <- 1945 # beginning of record
  S <- T - R
  PARcurlew <- partexp(lmoms(S))
  Xmax <- quatexp(1, PARcurlew)
  X <- seq(0,Xmax, by=1)
  plot(X, cdftexp(X,PARcurlew), type="l")

  # Plot looks not curved enough to show the texp?
  # Try S <- S^2 and rerunning the last four lines.
}
\keyword{distribution}
