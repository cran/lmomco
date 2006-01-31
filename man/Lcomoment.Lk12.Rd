\name{Lcomoment.Lk12}
\alias{Lcomoment.Lk12}
\title{ Compute a Single Sample L-comoment }
\description{
Compute the L-comoment (\eqn{\lambda_{k[r:n]}}) for a given pair of random variables. The order of
the L-comoments is specified.
}
\usage{
Lcomoment.Lk12(X1,X2,k=1)
}
\arguments{
  \item{X1}{An array of random variables.}
  \item{X2}{Another array of random variables.}
  \item{k}{The order of the L-comoment to compute. The default is 1.}
}
\details{
L-comoments are computed from the concomitants of X2. That is, X2 is
sorted in ascending order to create the order statistics of X2. X1 is
in turn reshuffled to the order of X2 for form the concomitants of X2 
(denoted as \eqn{X^{(12)}}). The
concomitants are inturn used in a weighted summation and expectation
calculation to compute the L-comoment of X1 to X2. The inverse can
also be done (\code{Lcomoment.Lk12(X2,X1,k=1)}) and is not necessarily
equal to (\code{Lcomoment.Lk12(X1,X2,k=1)}). The notation of Lk12 is
to read \dQuote{Lambda for kth order L-comoment}, where the 12 portion of the
notation reflects that of Serfling and Xiao (2006). The weights for
the computation are derived from calls by \code{Lcomoment.Lk12} to
\code{Lcomoment.Wk}.

  \deqn{ \hat{\lambda}_{k[12]} = n^{-1}\sum_{r=1}^{n}
                                     w^{(k)}_{r:n} X^{(12)}_{[r:n]}
       }

The concomitants of X1 (\eqn{X^{(21)}}) are formed by sorted X1 in ascending 
order and in turn shuffling X2 by the order of X1. By symmetry the L-comoment
is

  \deqn{ \hat{\lambda}_{k[21]} = n^{-1}\sum_{r=1}^{n}
                                     w^{(k)}_{r:n} X^{(21)}_{[r:n]}
       }

}
\value{A single L-comoment.}
\note{
The function begins with a capital letter. This is intentionally done so that lower case 
namespace is preserved. L-comoments are new in the literature and experimental in this package. 
By using a capital letter now, then \code{lcomoment.Lk12} remains an available name in future 
releases.
}
\source{
Serfling and Xiao (2006).
}
\references{
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
