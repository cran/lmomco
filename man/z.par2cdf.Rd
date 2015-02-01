\name{z.par2cdf}
\alias{z.par2cdf}
\title{Blipping Cumulative Distribution Functions}
\description{
This function acts as a front end or dispatcher to the distribution-specific cumulative distribution functions  but also provides for blipping according to
\deqn{F(x) = 0}
for \eqn{x \le 0} and
\deqn{F(x) = p + (1-p)G(x)}
for \eqn{x > 0}.
}
\usage{
z.par2cdf(x, p, para, z=0, ...)
}
\arguments{
  \item{x}{A real value vector.}
  \item{p}{Nonexceedance probability of the \code{z} value. This probability could simply be the portion of record having zero values if \code{z=0}.}
  \item{para}{The parameters from \code{\link{lmom2par}} or \code{\link{vec2par}}.}
  \item{z}{Threshold value.}
  \item{...}{The additional arguments are passed to the cumulative distribution function such as \code{paracheck=FALSE} for the Generalized Lambda distribution (\code{\link{cdfgld}}).}
}
\value{
  Nonexceedance probability (\eqn{0 \le F \le 1}) for \code{x}.
}
\author{ W.H. Asquith}
\references{
Asquith, W.H., 2011, Distributional analysis with L-moment statistics using the R environment for statistical computing: Createspace Independent Publishing Platform, ISBN 978--146350841--8.
}
\seealso{\code{\link{z.par2qua}}, \code{\link{par2cdf}}}
\examples{
# see the example for z.par2qua for more context
## define the real parent (or close)
the.gpa <- vec2par(c(100,1000,0.1),type='gpa')
fake.data <- rlmomco(30,the.gpa) # simulate some data
fake.data <- sort(c(fake.data,0,0,0,0)) # add of zero observations

# next compute the parameters for the positive data
gpa <- pargpa(lmoms(fake.data[fake.data > 0]))
n <- length(fake.data) # sample size
p <- length(fake.data[fake.data == 0])/n # est. prob of zero value
F <- nonexceeds() # handy values, to get nice range of x
x <- z.par2qua(F,p,gpa) # x are now computed

PP <- pp(fake.data) # compute plotting positions of sim. sample
plot(PP,fake.data,ylim=c(0,5000)) # plot the sample
lines(cdfgpa(x,the.gpa),x) # the parent (without zeros)
lines(z.par2cdf(x,p,gpa),x,lwd=3) # fitted model with zero conditional
# now repeat the above code over and over again and watch the results
}
\keyword{cumulative distribution function}
\keyword{blipping distributions}

