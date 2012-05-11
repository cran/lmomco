\name{genci}
\alias{genci}
\title{Generate Confidence Intervals for Quantiles of a Parent Distribution}
\description{
This function estimates the lower and upper limits of a specified confidence interval for a vector of quantile value of a specified parent distribution [quantile function \eqn{Q(F,\theta)} with parameters \eqn{\theta}] using Monte Carlo simulation. The quantile values, actually specified by a vector of nonexceedance probabilities (\eqn{F} for \eqn{0 \le F < 1}) of the values, are specified by the user. The user also provides the parameters of the parent distribution (see \code{\link{lmom2par}}). This function is a wrapper on \code{\link{qua2ci}}; please consult the documentation for that function for further details of the simulation.
}
\usage{
genci(para, n, F=NULL, ci=0.90, edist='nor', nsim=1000,
      expand=FALSE, verbose=FALSE, showpar=FALSE, quiet=FALSE)
}
\arguments{
  \item{para}{The parameters from \code{\link{lmom2par}} or similar.}
  \item{n}{The sample size that the Monte Carlo simulation will use.}
  \item{F}{Vector of nonexceedance probabilities (\eqn{0 \le F \le 1}) of the quantiles for which the confidence interval are needed. If \code{NULL}, then the vector as returned by \code{\link{nonexceeds}} is used.}
  \item{ci}{The confidence interval (\eqn{0.5 \le } \code{ci} \eqn{ < 1}). The interval is specified as the size of the interval. The default is 0.90 or the 90th percentile. The function will return the 5th (1-0.90)/2 and 95th (1-(1-0.90)/2) percentile cumulative probability of the error distribution for the parent quantile as specified by the nonexceedance probability argument (\code{F}). This argument is passed unused to \code{qua2ci}.}
  \item{edist}{The model for the error distribution. Although the normal (the default) is typically assumed in error analyses, it need not be, as support for other distributions supported by the \pkg{lmomco} package is available. However, one should seriously consider the values of the simulated L-moments when choosing an error distribution other than the normal. If the L-skew  (\eqn{\tau_4}) or L-kurtosis (\eqn{\tau_4}) values depart considerably from those of the normal (\eqn{\tau_3 = 0} and \eqn{\tau_4 = 0.122602}), alternative distributions would likely provide more reliable confidence interval estimation. This argument is passed unused to \code{qua2ci}.}
  \item{nsim}{The number of simulations for the sample size \code{n} to perform. Much larger simulation numbers are highly recommended---see discussion about \code{qua2ci}. This argument is passed unused to \code{qua2ci}. Users are encouraged to play with \code{qua2ci} to get a feel for the value of \code{edist} and \code{nsim}.}
  \item{expand}{Should the returned values be expanded to include information relating to the distribution type and L-moments of the distribution at the corresponding nonexceedance probabilities---in other words the information necessary to reconstruct the reported confidence interval. The default is \code{FALSE}. If \code{expand=FALSE} then a single \code{data.frame} of the lower and upper limits along with the true quantile value of the parent is returned. If \code{expand=TRUE}, then a more complicated \code{list} containing multiple \code{data.frame}s is returned.}
  \item{verbose}{The verbosity of the operation of the function. This argument is passed unused to \code{qua2ci}.}
  \item{showpar}{The parameters of the \code{edist} for each simulation for each \eqn{F} value passed to \code{qua2ci} are printed. This argument is passed unused to \code{qua2ci}.}
  \item{quiet}{Suppress incremental counter for a count down of the \eqn{F} values.}
}
\value{
  An \pkg{R} \code{data.frame} or \code{list} is returned (see discussion of argument \code{expand}). The following elements could be available.

  \item{nonexceed_prob}{A vector of \eqn{F} values, which is returned for convenience so that post operations such as plotting are easily coded.}
  \item{lower}{The lower value of the confidence interval having nonexceedance probability equal to \code{(1-ci)/2}.}
  \item{true}{The true quantile value from \eqn{Q(F,\theta)} for the corresponding \eqn{F} value.}
  \item{upper}{The upper value of the confidence interval having \eqn{F} equal to \code{1-(1-ci)/2}.}
  \item{lscale}{The second L-moment (L-scale, \eqn{\lambda_2}) of the distribution of quantiles for the corresponding \eqn{F}. This value is included in the primary returned \code{data.frame} because it measures the fundamental sampling variability.}
  \item{lcv}{The ratio of lscale to true. A measure of relative variability}
  \item{parent}{The paraments of the parent distribution if \code{expand=TRUE}.}
  \item{edist}{The type of error distribution used to model the confidence interval if the argument \code{expand=TRUE} is set.}
  \item{elmoms}{The L-moment of the distribution of quantiles for the corresponding \eqn{F} if the argument  \code{expand=TRUE} is set.}
  \item{epara}{The parameter list of the error distribution fit to the \code{elmoms} if the argument \code{expand=TRUE} is set.}
  \item{ifail}{A failure integer.}
  \item{ifailtext}{Text message associated with \code{ifail}.}
}
\author{ W.H. Asquith}
\seealso{\code{\link{lmoms}}, \code{lmom2par}, \code{\link{qua2ci}}, \code{\link{gen.freq.curves}}
}
\examples{
  # For all these examples, nsim is way too small.
  MEAN  <- 0    # mean of zero
  SIGMA <- 100  # standard deviation of 100
  PAR   <- vec2par(c(MEAN,SIGMA),type='nor') # make parameter object
  F     <- c(0.5, 0.8, 0.9, 0.96, 0.98, 0.99) # nonexceed probabilities
  # nsim is small for speed of example not accuracy.
  CI    <- genci(PAR,n=10,F=F,nsim=20)
  plot(CI$nonexceed_prob,CI$true,type='l',lwd=2)
  lines(CI$nonexceed_prob,CI$lower,col=2)
  lines(CI$nonexceed_prob,CI$upper,col=3)

\dontrun{
  pdf("twoCIplots.pdf")
  # The qnorm call has been added to produce "normal probability"
  # paper on the horizonal axis. The parent is heavy-tailed.
  GEV <- vec2par(c(10000,1500,-.3),type='gev') # a GEV distribution
  CI  <- genci(GEV,n=20,nsim=200,edist='gno')
  ymin <- log10(min(CI$lower[! is.na(CI$lower)]))
  ymax <- log10(max(CI$upper[! is.na(CI$upper)]))
  plot( qnorm(CI$nonexceed_prob),log10(CI$true),type='l',
                 ylim=c(ymin,ymax),lwd=2)
  lines(qnorm(CI$nonexceed_prob),log10(CI$lower),col=2)
  lines(qnorm(CI$nonexceed_prob),log10(CI$upper),col=3)
  # another error distribution model
  CI  <- genci(GEV,n=20,nsim=200,edist='aep4')
  lines(qnorm(CI$nonexceed_prob),log10(CI$lower),col=2, lty=2)
  lines(qnorm(CI$nonexceed_prob),log10(CI$upper),col=3, lty=2)
  dev.off()
}

}
\keyword{distribution}
