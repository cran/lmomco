\name{Lcomoment.Lk12}
\alias{Lcomoment.Lk12}
\title{ Compute a Single Sample L-comoment }
\description{
Compute the \eqn{k \ge 1} order L-comoment (\eqn{\lambda_{k[r:n]}}) for a given pair of random variables. The order of the L-comoments is specified.
}
\usage{
Lcomoment.Lk12(X1,X2,k=1)
}
\arguments{
  \item{X1}{A vector of random variables (a sample of random variable 1).}
  \item{X2}{Another vector of random variables (a sample of random variable 2).}
  \item{k}{The order of the L-comoment to compute. The default is 1.}
}
\details{
L-comoments of random variable \eqn{X1} are computed from the concomitants of \eqn{X2}. That is, \eqn{X2} is sorted in ascending order to create the order statistics of \eqn{X2}. In turn, \eqn{X1} is reshuffled to the order of \eqn{X2} for form the concomitants of \eqn{X2} (denoted as \eqn{X^{(12)}}). The concomitants in turn are used in a weighted summation and expectation calculation to compute the L-comoment of \eqn{X1} with respect to \eqn{X2}. The inverse can also be done (\code{Lcomoment.Lk12(X2,X1,k=1)}) and is not necessarily equal to (\code{Lcomoment.Lk12(X1,X2,k=1)}). The notation of \code{Lk12} is to read \dQuote{Lambda for kth order L-comoment}, where the \code{12} portion of the notation reflects that of Serfling and Xiao (2006) and then Asquith (2011). The weights \eqn{w^{(k)}_{r:n}} for the computation are derived from calls by \code{\link{Lcomoment.Lk12}} to \code{\link{Lcomoment.Wk}}. The L-comment are
  \deqn{ \hat{\lambda}_{k[12]} = \frac{1}{n}\sum_{r=1}^{n}
                                     w^{(k)}_{r:n} x^{(12)}_{[r:n]}\mbox{.}
       }
The L-comoments of \eqn{X2} are computed from the concomitants of \eqn{X1}, and the \eqn{X^{(21)}} are formed by sorting \eqn{X1} in ascending order and in turn shuffling \eqn{X2} by the order of \eqn{X1}. The sample concomitants are thus formed (\eqn{x^{(12)}_{[r:n]}}). By similarity to the previous equation, the remaining L-comoments are
  \deqn{ \hat{\lambda}_{k[21]} = \frac{1}{n}\sum_{r=1}^{n}
                                     w^{(k)}_{r:n} x^{(21)}_{[r:n]}\mbox{.}
       }
}
\value{A single L-comoment.}
\note{
The function begins with a capital letter. This is intentionally done so that lower case namespace is preserved. By using a capital letter now, then \code{lcomoment.Lk12} or similar remains an available name in future releases.
}
\references{
Asquith, W.H., 2011, Distributional analysis with L-moment statistics using the R environment for statistical computing: CreateSpace Independent Publishing Platform, 2nd printing, ISBN 978--146350841--8.

Serfling, R., and Xiao, P., 2007, A contribution to multivariate L-moments---L-comoment matrices: Journal of Multivariate Analysis, v. 98, pp. 1765--1781.
}
\author{ W.H. Asquith}
\seealso{\code{\link{Lcomoment.matrix}}, \code{\link{Lcomoment.Wk}}}
\examples{
X1   <- rnorm(20); X2   <- rnorm(20)
Lk12 <- Lcomoment.Lk12(X1,X2,k=1)
}
\keyword{multivariate}
\keyword{L-comoment}
