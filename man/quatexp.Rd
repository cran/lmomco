\name{quatexp}
\alias{quatexp}
\title{Quantile Function of the Truncated Exponential Distribution}
\description{
This function computes the quantiles of the Truncated Exponential distribution given
parameters (\eqn{\xi} and \eqn{\alpha}) of the distribution computed by
\code{\link{partexp}}. The quantile function of the distribution is

\deqn{x(F) = -\alpha\log[1-F(1-\mathrm{exp}(-\xi/\alpha))]\mbox{,}}

where \eqn{x(F)} is the quantile for nonexceedance probability \eqn{F},
\eqn{\xi} is a location parameter, \eqn{\alpha} is a scale parameter, \eqn{0 \le x \le \xi}. The distribution has \eqn{0 < \tau_2 <= 1/2}, \eqn{\xi > 0}, and \eqn{1/\alpha \ne 0}.
}
\usage{
quatexp(f, para, paracheck=TRUE)
}
\arguments{
  \item{f}{Nonexceedance probability (\eqn{0 \le F \le 1}).}
  \item{para}{The parameters from \code{\link{partexp}} or similar.}
  \item{paracheck}{A logical controlling whether the parameters and checked for validity. Overriding of this check might be extremely important and needed for use of the distribution quantile function in the context of TL-moments with nonzero trimming.}
}
\value{
  Quantile value for nonexceedance probability \eqn{F}.
}
\references{
Vogel, R.M., Hosking, J.R.M., Elphick, C.S., Roberts, D.L., and Reed, J.M., 2008, Goodness of fit of probability distributions for sightings as species approach extinction: Bulletin of Mathematial Biology, v. 71, no. 3, pp. 701--719.
}
\author{ W.H. Asquith}
\seealso{\code{\link{pdftexp}}, \code{\link{cdftexp}}, \code{\link{partexp}}}
\examples{
  lmr <- vec2lmom(c(40,0.38), lscale=FALSE)
  quatexp(0.5,partexp(lmr))

\dontrun{
  F <- nonexceeds()
  L1 <- 50; T2 <- seq(0.51,0.005,by=-.001)
  PAR <- partexp(vec2lmom(c(L1,1/3), lscale=FALSE))
  plot(F, quatexp(F, PAR),
       type="l", lwd=2, col=2,
       xlab="NONEXCEEDANCE PROBABILITY",
       ylab="SIGHTING TIMES",
       ylim=c(0,300)) # uniform distribution

  for(t2 in T2) {
    PAR <- partexp(vec2lmom(c(L1,t2), lscale=FALSE))
    if(is.null(PAR)) next
    if(PAR$is.uni) {
      # For the T2 near 1/3 a kick over to uniform solution is
      # needed.  For the -0.001 steps shown above no uniform
      # distribution solutions will be "used" and no output.
      print(PAR$para) # by this print() will be seen.
    }
    lines(F, quatexp(F,PAR), col=rgb(0,0,0,.1))
  }
  # Because T2 started at > 1/2, ten warnings of LCV > 1/2
  # will result during execution of the for() loop.
}
}
\keyword{distribution}
