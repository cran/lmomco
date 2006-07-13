\name{gen.freq.curves}
\alias{gen.freq.curves}
\title{Plot Randomly Generated Frequency Curves from a Parent Distribution}
\description{
This function generates random samples of specified size from a specified parent distribution. Subsequently, the type of parent distribution is fit to the L-moments of the generated sample. The fitted distribution is then plotted with the built-in function \code{lines}. It is the user's responsibility to have an active \code{plot} already drawn. This function is useful to demonstration of sample size on the uncertainty of a fitted distribution---a motivation for this function is for class room exercise.
}
\usage{
gen.freq.curves(n, para, F=NULL, nsim=10, aslog=FALSE, asprob=FALSE, 
showparent=FALSE, ...)
}
\arguments{
  \item{n}{Sample size to draw from parent as specified by \code{para}.}
  \item{para}{The parameters from \code{\link{lmom2par}} or similar.}
  \item{F}{The nonexceedance probabilities for horizontal axis---defaults to \code{\link{nonexceeds}} when the argument is \code{NULL}.}
  \item{nsim}{The number of simulations to perform (frequency curves to draw)---the default is 10.}
  \item{aslog}{Compute \code{log10} of quantiles---note that \preformatted{NaNs produced in: log(x, base)} will be produced for less than zero values. Otherwise this is a harmless message.}
  \item{asprob}{The \code{qnorm} function is used to convert nonexceedance probabilities, which are produced by \code{\link{nonexceeds}}, to standard normal deviates. The normal distribution will plot as straight line when this argument is \code{TRUE} and \code{aslog=FALSE}.}
  \item{showparent}{The curve for the parent distribution is plotted on exit from the function if \code{TRUE}. Further plotting options can not be controlled---unlike the situation with the drawing of the simulated frequency curves.}
  \item{...}{Additional parameters are passed to the \code{lines} call within the function---except for the drawing of the parent distribution (see argument \code{showparent}.}
}
\value{
  No value is returned. This function is used for its graphical side effects.
}
\author{ W.H. Asquith}
\seealso{\code{\link{lmom2par}}, \code{\link{nonexceeds}}, \code{\link{rlmomco}}, \code{\link{lmoms}}
}
\examples{
para  <- vec2par(c(140,4),type='gam') # build a gamma parent
F     <- nonexceeds() # vector of nonexceedance probabilities
Q     <- quagam(F,para) # the quantiles of parent distribution
plot(qnorm(F),Q,type='n',xlab='STANDARD NORMAL DEVIATE') # empty
# simulated are grey, parent is black
gen.freq.curves(10,para,asprob=TRUE,showparent=TRUE,col=8)
}
\keyword{distribution}
