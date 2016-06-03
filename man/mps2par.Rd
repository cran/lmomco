\name{mps2par}
\alias{mps2par}
\title{Use Maximum Product Spacing to the Parameters of a Distribution}
\description{
This function uses the method of maximum product spacing (MPS) to estimate the parameters of a distribution.

MPS (Dey \emph{et al.}, 2016, pp. 13--14) is a straightforward optimization problem that is formed by maximizing the geometric mean of the spacing between consecutively ordered observations standardized to a U-statistic. Let \eqn{\Theta} represent a vector of parameters for a candidate fit to the specified cumulative distribution function \eqn{G(x|\Theta)}, and let \eqn{U_i(\Theta) = G(X_{i:n}|\Theta)} be the nonexceedance probabilities of the observed values \eqn{x_i} for a sample of size \eqn{n}.  Define the differences
\deqn{D_i(\Theta) = U_i(\Theta) - U_{i-1}(\Theta)\mbox{\ for\ } i = 1, \ldots, n+1\mbox{,}}
with the additions to the vector \eqn{U} of \eqn{U_0(\Theta) = 0} and \eqn{U_{n+1}(\Theta) = 1}. The objective function is
\deqn{M(\Theta) = - \sum_{i=1}^{n+1} \log\, D_i(\Theta)\mbox{,}}
where the \eqn{\Theta} for a minimized \eqn{M} represent the parameters fit by MPS.
}
\usage{
mps2par(x, type, para.int=NULL, silent=TRUE, ...)
}
\arguments{
  \item{x}{A vector of data values.}
  \item{type}{Three character (minimum) distribution type (for example, \code{type="gev"}).}
  \item{para.int}{Initial parameters as a vector \eqn{\Theta}.}
  \item{silent}{A logical to silence the \code{try()} function wrapping the \code{optim()}.}
  \item{\ldots}{Additional arguments for the \code{optim()} function and other uses.}
}
\value{
  An \R \code{list} is returned.  This list should contain at least the following items, but some distributions such as the \code{revgum} have extra.
  \item{type}{The type of distribution in three character (minimum) format.}
  \item{para}{The parameters of the distribution.}
  \item{source}{Attribute specifying source of the parameters.}
  \item{MoranStatistic}{This is the minimized value for \eqn{M(\Theta)} for goodness-of-fit test according to Dey \emph{et al.} (2016, p. 14)}
  \item{optim}{The returned \code{list} of the \code{optim()} function.}
}
\note{
During the optimization process, the function requires evaluation at the initial parameters. The following error rarely will be seen:
\preformatted{
  Error in optim(para.int$para, afunc) :
    function cannot be evaluated at initial parameters
}
if \code{Inf} is returned on first call to the objective function. The \code{silent} by default though will silence this error. The example shown in this documentation produces 4 failures out of 1,000 for the random seed given.
}
\author{ W.H. Asquith}
\references{
Dey, D.K., Roy, Dooti, Yan, Jun, 2016, Univariate extreme value analysis, chapter 1, \emph{in} Dey, D.K., and Yan, Jun, eds., Extreme value modeling and risk analysis---Methods and applications: Boca Raton, FL, CRC Press, p. 1--22.
}
\seealso{\code{\link{lmom2par}}}
\examples{
\dontrun{
# Use 1000 replications for sample size of 75 and estimate the bias and variance of the
# method of L-moments and maximum product spacing (MPS) for the 100-year event using
# the Pearson Type III distribution. The parent are the L-moments of some base-10 logs
# of annual peak streamflow for a stream location somewhere in the United States.
set.seed(1596)
nsim <- 1000; n <- 15; Tyear <- 100; type <- "pe3"
parent.lmr <- vec2lmom(c(5.5, 0.15, 0.03))   # L-moments of the "parent"
parent  <- lmom2par(parent.lmr, type="pe3")  # "the parent"
Q100tru <- qlmomco(T2prob(Tyear), parent)    # "true value"
Q100lmr <- Q100mps <- rep(NA, nsim)          # empty vectors
T3lmr <- T4lmr <- T3mps <- T4mps <- rep(NA, nsim)
for(i in 1:nsim) { # simulate from the parent, compute L-moments
   tmpX <- rlmomco(n, parent); lmrX <- lmoms(tmpX)
   if(! are.lmom.valid(lmrX)) { # quiet check on viability
     lmrX <- pwm2lmom(pwms.pp(tmpX)) # try a pwm by plotting positions instead
     if(! are.lmom.valid(lmrX)) next
   }
   lmrpar <- lmom2par(lmrX, type=type)                  # Method of L-moments
   mpspar <-  mps2par(tmpX, type=type, para.int=lmrpar) # Method of MPS
   if(! is.null(lmrpar)) {
      Q100lmr[i] <- qlmomco(T2prob(Tyear), lmrpar); lmrlmr <- par2lmom(lmrpar)
      T3lmr[i] <- lmrlmr$ratios[3]; T4lmr[i] <- lmrlmr$ratios[4]
   }
   if(! is.null(mpspar)) {
      Q100mps[i] <- qlmomco(T2prob(Tyear), mpspar); mpslmr <- par2lmom(mpspar)
      T3mps[i] <- mpslmr$ratios[3]; T4mps[i] <- mpslmr$ratios[4]
   }
}
print(summary(Q100tru - Q100lmr)) # Method of L-moment   (mean = -0.00176)
print(summary(Q100tru - Q100mps)) # Method of MPS        (mean = -0.02722)
print(var(Q100tru - Q100lmr, na.rm=TRUE)) # Method of L-moments (0.009053)
print(var(Q100tru - Q100mps, na.rm=TRUE)) # Method of MPS       (0.009875)
# CONCLUSION: MPS is competitive but still behind the mightly L-moments.

LMR <- data.frame(METHOD=rep("Method L-moments",        nsim), T3=T3lmr, T4=T4lmr)
MPS <- data.frame(METHOD=rep("Maximum Product Spacing", nsim), T3=T3mps, T4=T4mps)
ZZ <- merge(LMR, MPS, all=TRUE)
boxplot(ZZ$T3~ZZ$METHOD, data=ZZ); mtext("L-skew Distributions")
boxplot(ZZ$T4~ZZ$METHOD, data=ZZ); mtext("L-kurtosis Distributions") #}
}
\keyword{distribution (parameters)}
\keyword{parameter (maximum product spacing to)}
\keyword{distribution (utility)}
\keyword{maximum product spacing}

