\name{qua2ci}
\alias{qua2ci}
\title{Estimate Confidence Interval for a Single Quantile of a Parent Distribution}
\description{
This function estimates the lower and upper limits of a specified confidence interval for an aribitrary quantile value of a specified parent distribution [quantile function \eqn{Q(F,\theta)} with parameters \eqn{\theta}] using Monte Carlo simulation. The quantile value, actually the nonexceedance probability (\eqn{F} for \eqn{0 \le F < 1}) of the value, is specified by the user. The user also provides the parameters of the parent distribution (see \code{\link{lmom2par}}).

For \code{nsim} simulation runs (ideally a very large number), samples of size \eqn{n} are drawn from \eqn{Q(F,\theta)} using the R-function \code{runif(n)} to generate the simulated \eqn{F} values and the \pkg{lmomco} function \code{par2qua} function to compute the quantile. The L-moments of the simulated sample are computed using \code{\link{lmoms}} and a distribution of the same type as the parent is fit to these simulated L-moments. The \eqn{F}-quantile of the just-fitted distribution is computed and placed into a vector. The process of simulating the sample, computing the L-moments, computing the parameters, and solving for the \eqn{F}-quantile is repeated for the specified number of simulation runs.

To estimate the confidence interval, the L-moments of the vector simulated quantiles are computed. Subsequently, the parameters of a user-specified distribution \dQuote{error} distribution are computed. The two quantiles of this error distribution for the specified confidence interval are computed. These two quantiles represent the estimated lower and upper limits for the confidence interval of the parent distribution for samples of size \eqn{n}.

Finally, it is often useful to have vectors of lower and upper limits for confidence intervals for a vector of \eqn{F} values. The function \code{\link{genci}} does just that and uses the \code{qua2ci} function as the computational engine.
}
\usage{
qua2ci(f, para, n, ci=0.90, edist='nor', nsim=1000,
verbose=FALSE, showpar=FALSE, maxlogdiff=6)
}
\arguments{
  \item{f}{Nonexceedance probability (\eqn{0 \le F \le 1}) of the quantile for which the confidence interval is needed.}
  \item{para}{The parameters from \code{\link{lmom2par}} or similar.}
  \item{n}{The sample size that the Monte Carlo simulation will use.}
  \item{ci}{The confidence interval (\eqn{0.5 \le } \code{ci} \eqn{ < 1}). The interval is specified as the size of the interval. The default is 0.90 or the 90th percentile. The function will return the 5th (1-0.90)/2 and 95th (1-(1-0.90)/2) percentile cumulative probability of the error distribution for the parent quantile as specified by the nonexceedance probability argument. The arguments \code{ci} and \code{f} therefore are separate features.}
  \item{edist}{The model for the error distribution. Although the normal (the default) is typically assumed in error analyses, it need not be, as support for other distributions supported by the \pkg{lmomco} package is available. However, one should seriously consider the values of the simulated L-moments when choosing an error distribution other than the normal. If the L-skew  (\eqn{\tau_4}) or L-kurtosis (\eqn{\tau_4}) values depart considerably from those of the normal (\eqn{\tau_3 = 0} and \eqn{\tau_4 = 0.122602}), alternative distributions would likely provide more reliable confidence interval estimation.}
  \item{nsim}{The number of simulations for the sample size \code{n} to perform. Large numbers produce more refined confidence limit estimates at the cost of CPU time. The default is anticipated to be large enough for evaluative-useage without too much computational delay. Much larger simulation numbers are highly recommended.}
  \item{verbose}{The verbosity of the operation of the function.}
  \item{showpar}{The parameters of the \code{edist} for each simulation are printed.}
  \item{maxlogdiff}{The maximum permitted difference in log10 space between a simulated quantile and the true value. It is possible that a well fit simulated sample to the parent distribution type provides crazy quantile estimates in the far reaches of either tail. The default value of 6 was chosen based on experience with the Kappa distribution fit to a typical heavy-right tail flood magnitude data set. However, the choice of 6-log cycles is ad hoc at best; users are encouraged to do their own exploration. If \code{verbose=TRUE} then a message will be printed when the \code{maxlogdiff} condition is tripped.}
}
\value{
  An R \code{list} is returned.

  \item{lower}{The lower value of the confidence interval having nonexceedance probability equal to (1-\code{ci})/2.}
  \item{true}{The value returned by \code{par2qua(f,para)}.}
  \item{upper}{The upper  value of the confidence interval having nonexceedance probability equal to 1-(1-\code{ci})/2.}
  \item{elmoms}{The L-moments from \code{\link{lmoms}} of the distribution of simulated of quantiles.}
  \item{epmoms}{The product moments from \code{\link{pmoms}} of the distribution of simulated quantiles.}
  \item{epara}{The parameters of the error distribution fit using the \code{elmoms}.}
  \item{ifail}{A diagnostic value. A value of zero means that successful exit was made.}
  \item{ifailtext}{A descriptive message related to the ifail value.}
}
\author{ W.H. Asquith}
\seealso{\code{\link{lmoms}}, \code{lmom2par}, \code{\link{par2qua}}, \code{\link{genci}}
}
\examples{
  # It is well known that standard deviation (sigma) of the
  # sample mean is equal to sigma/sample_size.
  MEAN  <- 0
  SIGMA <- 100
  PAR   <- vec2par(c(MEAN,SIGMA),type='nor')
  CI    <- qua2ci(0.5,PAR,n=10,nsim=20) # F=0.5-->median=mean
  # Theoretrical sample mean sigma = 100/10 = 10
  # L-moment theory: L-scale*sqrt(pi) = sigma
  # Thus, it follows that
  CI$elmoms$lambdas[2]/sqrt(pi)
  # approaches 10 as nsim --> Inf.


  # Another example.
  D   <- c(123,34,4,654,37,78, 93, 95, 120) # fake sample
  lmr <- lmoms(D)    # compute the lmoments of the data
  WEI <- parwei(lmr) # estimate parameters of the weibull
  CI  <- qua2ci(0.75,WEI,20,nsim=20,ci=0.95)
  # CI contains the estimate 95-percent
  # confidence interval for the 75th-percent of the parent
  # weibull distribution for size 20 samples from the parent
}
\keyword{distribution}
