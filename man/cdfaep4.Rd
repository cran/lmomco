\name{cdfaep4}
\alias{cdfaep4}
\title{Cumulative Distribution Function of the 4-p Asymmetric Exponential Power Distribution}
\description{
This function computes the cumulative probability or nonexceedance probability
of the 4-parameter Asymmetric Exponential Power distribution given parameters (\eqn{\xi}, \eqn{\alpha}, \eqn{\kappa}, and \eqn{h}) of the distribution computed by \code{\link{paraep4}}. The cumulative distribution function of the distribution is

For \eqn{x < \xi},

\deqn{F(x) = \frac{\kappa^2}{(1+\kappa^2)} \; \gamma([(\xi - x)/(\alpha\kappa)]^h,\; 1/h) }

and for \eqn{x \ge \xi},

\deqn{F(x) = 1 - \frac{1}{(1+\kappa^2)} \; \gamma([\kappa(x - \xi)/\alpha]^h,\; 1/h) }

where \eqn{F(x)} is the nonexceedance probability for quantile \eqn{x},
\eqn{\xi} is a location parameter, \eqn{\alpha} is a scale parameter,
\eqn{\kappa} is a shape parameter, \eqn{h} is another shape parameter, \eqn{\gamma(Z, shape)} is the upper tail of the incomplete gamma function. The upper tail of the incomplete gamma function is \code{pgamma(Z, shape, lower.tail=FALSE)} in \R and mathematically is

\deqn{\gamma(Z, a) = \int_Z^\infty y^{a-1} \exp(-y) dy \; / \Gamma(a)\mbox{.}}

}
\usage{
cdfaep4(x, para, paracheck=TRUE)
}
\arguments{
  \item{x}{A real value.}
  \item{para}{The parameters from \code{\link{paraep4}} or similar.}
  \item{paracheck}{A logical controlling whether the parameters and checked for validity.}
}

\value{
  Nonexceedance probability (\eqn{F}) for \eqn{x}.
}
\references{
Ayebo, A., and Kozubowski, T.J., 2003, An asymmetric generalization of Gaussian and
Laplace laws: Journal of Probability and Statistical Science, v. 1, no. 2, pp. 187-210.

Delicado, P., and Goria, M.N., 2008, A small sample comparison of maximum likelihood,
moments and L-moments methods for the asymmetric exponential power distribution:
Computational Statistics and Data Analysis, v. 52, no. 3, pp. 1661-1673.

Asquith, W.H., 2014, Parameter Estimation for the 4-Parameter Asymmetric Exponential Power Distribution by the Method of L-moments using R: Computational Statistics and Data Analysis, v. 71, pp. 955-970.
}
\author{ W.H. Asquith}
\seealso{\code{\link{cdfaep4}}, \code{\link{quaaep4}}, \code{\link{paraep4}}}
\examples{
x <- -0.1
para <- vec2par(c(0, 100, 0.5, 4), type="aep4")
F <- cdfaep4(-.1,para)
estx <- quaaep4(F, para)
cat(c("F=",F,"  and estx=",estx,"\n"))

\dontrun{
delx <- .1
x <- seq(-20,20, by=delx);
K <- 1;

PAR <- list(para=c(0,1, K, 0.5), type="aep4");
plot(x,cdfaep4(x, PAR), type="n",
     ylab="NONEXCEEDANCE PROBABILITY",
     ylim=c(0,1), xlim=range(x));
lines(x,cdfaep4(x,PAR), lwd=4);
lines(quaaep4(cdfaep4(x,PAR),PAR), cdfaep4(x,PAR), col=2)

PAR <- list(para=c(0,1, K, 1), type="aep4");
lines(x,cdfaep4(x, PAR), lty=2, lwd=4);
lines(quaaep4(cdfaep4(x,PAR),PAR), cdfaep4(x,PAR), col=2)

PAR <- list(para=c(0,1, K, 2), type="aep4");
lines(x,cdfaep4(x, PAR), lty=3, lwd=4);
lines(quaaep4(cdfaep4(x,PAR),PAR), cdfaep4(x,PAR), col=2)

PAR <- list(para=c(0,1, K, 4), type="aep4");
lines(x,cdfaep4(x, PAR), lty=4, lwd=4);
lines(quaaep4(cdfaep4(x,PAR),PAR), cdfaep4(x,PAR), col=2)
}

}
\keyword{distribution}
\keyword{cumulative distribution function}
