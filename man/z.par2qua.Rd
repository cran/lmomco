\name{z.par2qua}
\alias{z.par2qua}
\title{Blipping Quantile Functions}
\description{
This function acts as a front end or dispatcher to the distribution-specific quantile functions but also provides for blipping according to
\deqn{x(F) = 0}
for \eqn{0 \le F \le p} and
\deqn{x_G\left(\frac{F-p}{1-p}\right)}
for \eqn{F > p}.
}
\usage{
z.par2qua(f, p, para, z=0, ...)
}
\arguments{
  \item{f}{Nonexceedance probabilities (\eqn{0 \le F \le 1}).}
  \item{p}{Nonexceedance probability of \code{z} value.}
  \item{para}{The parameters from \code{\link{lmom2par}} or \code{\link{vec2par}}.}
  \item{z}{Threshold value.}
  \item{...}{The additional arguments are passed to the quantile function such as \cr  \code{paracheck = FALSE} for the Generalized Lambda distribution (\code{\link{quagld}}).}
}
\value{
  Quantile value for \eqn{f}.
}
\author{ W.H. Asquith}
\references{
Asquith, W.H., 2011, Distributional analysis with L-moment statistics using the R environment for statistical computing: Createspace Independent Publishing Platform, ISBN 978--146350841--8.
}
\seealso{\code{\link{z.par2cdf}}, \code{\link{par2qua}}}
\examples{
# define the real parent (or close)
the.gpa <- vec2par(c(100,1000,0.1),type='gpa')
fake.data <- rlmomco(30,the.gpa) # simulate some data
fake.data <- sort(c(fake.data,0,0,0,0)) # add of zero observations

# next compute the parameters for the positive data
gpa <- pargpa(lmoms(fake.data[fake.data > 0]))
n <- length(fake.data) # sample size
p <- length(fake.data[fake.data == 0])/n # est. prob of zero value
F <- nonexceeds() # handy values
PP <- pp(fake.data) # compute plotting positions of sim. sample
plot(PP,fake.data,ylim=c(0,5000)) # plot the sample
lines(F,quagpa(F,the.gpa)) # the parent (without zeros)
lines(F,z.par2qua(F,p,gpa),lwd=3) # fitted model with zero conditional
# now repeat the above code over and over again and watch the results
}
\keyword{quantile function}
\keyword{blipping distributions}

