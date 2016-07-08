\name{mle2par}
\alias{mle2par}
\title{Use Maximum Likelihood to Estimate Parameters of a Distribution}
\description{
This function uses the method of maximum likelihood (MLE) to estimate the parameters of a distribution.

MLE is a straightforward optimization problem that is formed by maximizing the sum of the logarithms of probability densities. Let \eqn{\Theta} represent a vector of parameters for a candidate fit to the specified probability density function \eqn{g(x|\Theta)} and \eqn{x_i} represent the observed data for a sample of size \eqn{n}. The objective function is
\deqn{\mathcal{L}(\Theta) = -\sum_{i=1}^{n} \log\, g(x_i|\Theta)\mbox{,}}
where the \eqn{\Theta} for a maximized \eqn{{-}\mathcal{L}} represents the parameters fit by MLE. The initial parameter estimate by default will be seeded by the method of L-moments.
}
\usage{
mle2par(x, type, para.int=NULL, silent=TRUE, ...)
}
\arguments{
  \item{x}{A vector of data values.}
  \item{type}{Three character (minimum) distribution type (for example, \code{type="gev"}).}
  \item{para.int}{Initial parameters as a vector \eqn{\Theta}.}
  \item{silent}{A logical to silence the \code{try()} function wrapping the \code{optim()} function.}
  \item{...}{Additional arguments for the \code{optim()} function and other uses.}
}
\value{
  An \R \code{list} is returned.  This list should contain at least the following items, but some distributions such as the \code{revgum} have extra.
  \item{type}{The type of distribution in three character (minimum) format.}
  \item{para}{The parameters of the distribution.}
  \item{source}{Attribute specifying source of the parameters.}
  \item{optim}{The returned \code{list} of the \code{optim()} function.}
}
\note{
During the optimization process, the function requires evaluation at the initial parameters. The following error rarely will be seen:
\preformatted{
  Error in optim(para.int$para, afunc) :
    function cannot be evaluated at initial parameters
}
if \code{Inf} is returned on first call to the objective function. The \code{silent} by default though will silence this error.  Alternative starting parameters might help.  This function is not built around subordinate control functions to say keep the parameters within distribution-specific bounds.  However, in practice, the L-moment estimates should already be fairly close and the optimizer can take it from there.  More sophisticated MLE for many distributions is widely available in other \R packages. The \pkg{lmomco} package uses its own probability density functions.
}
\author{ W.H. Asquith}
\seealso{\code{\link{lmom2par}}, \code{\link{mps2par}}}
\examples{
\dontrun{
father <- vec2par(c(37,25,114), type="st3"); FF <- nonexceeds(); qFF <- qnorm(FF)
X <- rlmomco(78, father) # rerun if MLE and MPS fail to get a solution
plot(qFF,  qlmomco(FF, father), type="l", xlim=c(-3,3),
     xlab="STANDARD NORMAL VARIATE", ylab="QUANTILE") # parent (black)
lines(qFF, qlmomco(FF, lmom2par(lmoms(X), type="gev")), col=2) # L-moments (red)
lines(qFF, qlmomco(FF, mps2par(X, type="gev")), col=3)         #     MLE (green)
lines(qFF, qlmomco(FF, mle2par(X, type="gev")), col=4)         #      MPS (blue)
points(qnorm(pp(X)), sort(X)) # the simulated data}

\dontrun{
# REFLECTION SYMMETRY
set.seed(451)
X <- rlmomco(78, vec2par(c(2.12, 0.5, 0.6), type="pe3"))
# MLE and MPS are almost reflection symmetric, but L-moments always are.
mle2par( X, type="pe3")$para #  2.1796827 0.4858027  0.7062808
mle2par(-X, type="pe3")$para # -2.1796656 0.4857890 -0.7063917
mps2par( X, type="pe3")$para #  2.1867551 0.5135882  0.6975195
mps2par(-X, type="pe3")$para # -2.1868252 0.5137325 -0.6978034
parpe3(lmoms( X))$para       #  2.1796630 0.4845216  0.7928016
parpe3(lmoms(-X))$para       # -2.1796630 0.4845216 -0.7928016}
}
\keyword{distribution (parameters)}
\keyword{parameter (maximum likelihood to)}
\keyword{distribution (utility)}
\keyword{parameter (estimation)}

