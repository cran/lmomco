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
  \item{X1}{An vector of random variables (a sample of random variable 1).}
  \item{X2}{Another vector of random variables (a sample of random variable 2).}
  \item{k}{The order of the L-comoment to compute. The default is 1.}
}
\details{
L-comoments of random variable \eqn{X1} are computed from the concomitants of \eqn{X2}. That is, \eqn{X2} is
sorted in ascending order to create the order statistics of \eqn{X2}. \eqn{X1} is
in turn reshuffled to the order of \eqn{X2} for form the concomitants of \eqn{X2} 
(denoted as \eqn{X^{(12)}}). The concomitants inturn are used in a weighted summation and expectation
calculation to compute the L-comoment of \eqn{X1} with respect to \eqn{X2}. The inverse can
also be done (\code{Lcomoment.Lk12(X2,X1,k=1)}) and is not necessarily
equal to (\code{Lcomoment.Lk12(X1,X2,k=1)}). The notation of \code{Lk12} is
to read \dQuote{Lambda for kth order L-comoment}, where the \code{12} portion of the
notation reflects that of Serfling and Xiao (2006). The weights for
the computation are derived from calls by \code{Lcomoment.Lk12} to
\code{Lcomoment.Wk}.

  \deqn{ \hat{\lambda}_{k[12]} = \frac{1}{n}\sum_{r=1}^{n}
                                     w^{(k)}_{r:n} x^{(12)}_{[r:n]}
       }

The L-comoments of \eqn{X2} are computed from the concomitants of \eqn{X1} (\eqn{X^{(21)}}) are formed by sorting \eqn{X1} in ascending 
order and in turn shuffling \eqn{X2} by the order of \eqn{X1}. The sample concomitants are thus formed (\eqn{x^{(12)}_{[r:n]}}). By symmetry the L-comoment
is

  \deqn{ \hat{\lambda}_{k[21]} = \frac{1}{n}\sum_{r=1}^{n}
                                     w^{(k)}_{r:n} x^{(21)}_{[r:n]}
       }

}
\value{A single L-comoment.}
\note{
The function begins with a capital letter. This is intentionally done so that lower case 
namespace is preserved. L-comoments are new in the literature and experimental in this package. 
By using a capital letter now, then \code{lcomoment.Lk12} or similar remains an available name in future 
releases.
}
\source{
Serfling and Xiao (2006).
}
\references{
Hosking, J.R.M. and Wallis, J.R., 1997, Regional frequency analysis---An
approach based on L-moments: Cambridge University Press.

Serfling, R., and Xiao, P., 2006, Multivariate L-moments, preprint.
}
\author{ W.H. Asquith}

\seealso{\code{\link{Lcomoment.matrix}}, \code{\link{Lcomoment.Wk}}}
\examples{
X1   <- rnorm(20)
X2   <- rnorm(20)
Lk12 <- Lcomoment.Lk12(X1,X2,k=1)
}
\keyword{multivariate}
\keyword{distribution}
