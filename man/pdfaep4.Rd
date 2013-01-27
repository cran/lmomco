\name{pdfaep4}
\alias{pdfaep4}
\title{Probability Density Function of the 4-p Asymmetric Exponential Power Distribution}
\description{
This function computes the probability density
of the 4-parameter Asymmetric Exponential Power distribution given parameters (\eqn{\xi},
\eqn{\alpha}, \eqn{\kappa}, and \eqn{h}) of the distribution computed by \code{\link{paraep4}}. The
probability density function of the distribution is

\deqn{f(x) = \frac{\kappa\,h}{\alpha(1+\kappa^2)\,\Gamma(1/h)}
    \exp[ -(\kappa^{ \mathrm{sign}(x-\xi)}\,(\,|x-\xi|/\alpha)\,)^h ]  }

where \eqn{f(x)} is the probability density for quantile \eqn{x},
\eqn{\xi} is a location parameter, \eqn{\alpha} is a scale parameter,
\eqn{\kappa} is a shape parameter, and \eqn{h} is another shape parameter. The range of the distribution is \eqn{-\infty < x < \infty}.
}
\usage{
pdfaep4(x, para, paracheck=TRUE)
}
\arguments{
  \item{x}{A real value.}
  \item{para}{The parameters from \code{\link{paraep4}} or similar.}
  \item{paracheck}{A logical controlling whether the parameters and checked for validity.}
}

\value{
  Probability density (\eqn{f}) for \eqn{x}.
}
\references{
Ayebo, A., and Kozubowski, T.J., 2003, An asymmetric generalization of Gaussian and Laplace laws: Journal of Probability and Statistical Science, v. 1, no. 2, pp. 187-210.

Delicado, P., and Goria, M.N., 2008, A small sample comparison of maximum likelihood,
moments and L-moments methods for the asymmetric exponential power distribution:
Computational Statistics and Data Analysis, v. 52, no. 3, pp. 1661-1673.

Asquith, W.H., 2013, Parameter Estimation for the 4-Parameter Asymmetric Exponential Power Distribution by the Method of L-moments using R, Computational Statistics and Data Analysis, http://dx.doi.org/10.1016/j.csda.2012.12.013
}
\author{ W.H. Asquith}
\seealso{\code{\link{cdfaep4}}, \code{\link{quaaep4}}, \code{\link{paraep4}}}
\examples{
aep4 <- vec2par(c(1000,15000,0.5,0.4), type='aep4');
F <- nonexceeds();
x <- quaaep4(F,aep4);
check.pdf(pdfaep4,aep4,plot=TRUE);

\dontrun{
delx <- .01;
x <- seq(-10,10, by=delx);
K <- 3;
PAR <- list(para=c(0,1, K, 0.5), type="aep4");
plot(x,pdfaep4(x, PAR), type="n",
     ylab="PROBABILITY DENSITY",
     ylim=c(0,0.6), xlim=range(x));
lines(x,pdfaep4(x,PAR), lwd=2);

PAR <- list(para=c(0,1, K, 1), type="aep4");
lines(x,pdfaep4(x, PAR), lty=2, lwd=2);

PAR <- list(para=c(0,1, K, 2), type="aep4");
lines(x,pdfaep4(x, PAR), lty=3, lwd=2);

PAR <- list(para=c(0,1, K, 4), type="aep4");
lines(x,pdfaep4(x, PAR), lty=4, lwd=2);
}

}
\keyword{distribution}
\keyword{probability density function}
