\encoding{utf8}
\name{dat2bernqua}
\alias{dat2bernqua}
\title{ Observed Data to Quantiles through the Bernstein Polynomial }
\description{
The empirical quantile function can be supposedly \dQuote{smoothed} (\enc{Hernández-Maldonado}{Hernandez-Maldonado} and others, 2012, p. 114) through the Berstein polynomial (\enc{Muñoz-Pérez}{Munoz-Perez} and \enc{Fernández-Palacín}{Fernandez-Palacin}, 1987) for the sample order statistics \eqn{x_{k:n}} for a sample of size \eqn{n} is

\deqn{\tilde{X}_n(F) = \frac{1}{2}\sum_{k=0}^n (x_{k:n} + x_{(k+1):n}) {n \choose k} F^k (1-F)^{n-k}\mbox{,}}

where \eqn{F} is nonexceedance probability, and \eqn{(n\:k)} are the binomial coefficients from the \code{choose()} function of \R, and special situation for \eqn{k=0} and \eqn{k=n} are described within the Note section.
}
\usage{
dat2bernqua(f, x, natmin=NULL, natmax=NULL)
}
\arguments{
  \item{f}{A vector of nonexceedance probabilities \eqn{F};}
  \item{x}{A vector of data values;}
  \item{natmin}{For \eqn{k = 0}, either the natural minimum is used if provided as non \code{NULL} or the observed minimum of the data; If the minimum of the data is less than the \code{natmin}, a warning is triggered and \code{natmin} is set to \code{NULL}.}
  \item{natmax}{For \eqn{k = n}, either the natural maximum is used if provided as non \code{NULL} or the observed maximum of the data. If the maximum of the data is greater than the \code{natmax}, a warning is triggered and \code{natmax} is set to \code{NULL}.}
}
\value{
  An R \code{vector} is returned.
}
\note{
\enc{Muñoz-Pérez}{Munoz-Perez} and \enc{Fernández-Palacín}{Fernandez-Palacin} (1987, p. 391) describe what to do with the condition of \eqn{k = 0} but seemingly do not comment on the condition of \eqn{k = n}. There is no 0th order statistic nor is there a \eqn{k > n} order statistic. \enc{Muñoz-Pérez}{Munoz-Perez} and \enc{Fernández-Palacín}{Fernandez-Palacin} (1987) bring up the notion of a natural minimum for the data (for example, data that must be positive, \code{natmin = 0} could be set). Logic dictates that a similar argument must be made for the maximum to keep a critical error from occurring if one tries to access \code{x[n+1]}.
}
\references{
\enc{Hernández-Maldonado}{Hernandez-Maldonado}, V., \enc{Díaz-Viera}{Diaz-Viera}, M., and Erdely, A., 2012, A joint stochastic simulation method using the Berstein copula as a flexible tool for modeling nonlinear dependence structures between petrophysical properties: Journal of Petroleum Science and Engineering, v. 90--91, pp. 112--123.


\enc{Muñoz-Pérez}{Munoz-Perez}, J., and \enc{Fernández-Palacín}{Fernandez-Palacin}, A., 1987, Estimating the quantile function by Bernstein polynomials: Computational Statistics and Data Analysis, v. 5, pp. 391--397.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{nonexceeds}} }
\examples{
#
X <- rnorm(50); F <- nonexceeds()
Q <- dat2bernqua(F, X)        # the Bernstein estimates
Z <- quantile(X, probs=F) # more or less default method using R itself
plot(F, Q, type="b")
points(F, Z, col=2, pch=16)

}
\keyword{univar}
\keyword{distribution}
