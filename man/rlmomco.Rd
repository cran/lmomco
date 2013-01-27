\name{rlmomco}
\alias{rlmomco}
\title{Random Variates of a Distribution}
\description{
This function generates random deviates for the specified distribution in the parameter object argument. See documentation about the parameter object is seen in \code{\link{lmom2par}} or \code{\link{vec2par}}. The prepended
\code{r} in the function name is to parallel the built-in distribution
syntax of R but of course reflects the \pkg{lmomco} name in the function. An assumption is made that the user knows that they are feeding appropirate (valid) parameters for the distribution. This is seen by the \preformatted{paracheck = FALSE} argument passed to the \code{par2qua} function.
}
\usage{
rlmomco(n,para)
}
\arguments{
  \item{n}{Number of samples to generate}
  \item{para}{The parameters from \code{\link{lmom2par}} or similar.}
}
\value{
  Vector of quantile values.
}
\author{ W.H. Asquith}
\seealso{\code{\link{dlmomco}}, \code{\link{plmomco}}, \code{\link{qlmomco}},
\code{\link{lmom2par}}, \code{\link{vec2par}}}
\examples{
lmr      <- lmom.ub(rnorm(20)) # generate 20 standard normal deviates
para     <- parnor(lmr) # estimate parameters of the normal
simulate <- rlmomco(20,para) # simulate 20 samples using lmomco package

lmr  <- vec2lmom(c(1000,500,.3)) # first three lmoments are known
para <- lmom2par(lmr,type="gev") # est. parameters of GEV distribution
Q    <- rlmomco(45,para) # simulate 45 samples
PP   <- pp(Q)            # compute the plotting positions
plot(PP,sort(Q))         # plot the data up
}
\keyword{random variate}
\keyword{quantile function}
