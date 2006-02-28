\name{Lcomoment.Wk}
\alias{Lcomoment.Wk}
\title{ Weighting Coefficient for Sample L-comoment }
\description{
Compute the weight factors for computation of an L-comoment for order \code{k},
order statistic \code{r}, and sample size \code{n}.
}
\usage{
Lcomoment.Wk(k,r,n)
}
\arguments{
  \item{k}{Order of L-comoment being computed by parent calls to \code{Lcomoment.Wk}.}
  \item{r}{Order statistic index involved.}
  \item{n}{Sample size.}
}
\details{
This function computes the weight factors needed to calculation L-comoments
and is interfaced or used by \code{Lcomoment.Lk12}. This function is not
necessarily for end users. The weight factor \eqn{w^{(k)}_{r:n}} is the discrete Legendre
polynomial. The weight factors are well illustrated in figure 2.6 of Hosking
and Wallis (1997).

 \deqn{ w^{(k)}_{r:n} = \sum_{j=0}^{min\{r-1,k-1\}} (-1)^{k-1-j}
                                    \frac{C(k-1,j)C(k-1+j,j)C(r-1,j)}
				         {C(n-1,j)}
      \mbox{,}}

where \eqn{C(a,b)} is the combinatorial function (\code{choose} in R).
}
\value{A single L-comoment weight factor.}
\note{
The function begins with a capital letter. This is intentionally done so that lower case 
namespace is preserved. L-comoments are new in the literature and experimental in this package. 
By using a capital letter now, then \code{lcomoment.Wk} remains an available name in future 
releases.
}
\source{
Serfling and Xiao (2006).
}
\references{
Hosking, J.R.M. and Wallis, J.R., 1997, Regional frequency analysis--An
approach based on L-moments: Cambridge University Press.

Serfling, R., and Xiao, P., 2006, Multivariate L-moments, preprint.
}
\author{ W.H. Asquith}

\seealso{\code{\link{Lcomoment.Wk}}}
\examples{
Wk <- Lcomoment.Wk(2,3,5)
# To compute the weight factors for L-skew and L-coskew (k=3) computation
# for a sample of size 20.
Wk <- matrix(nrow=20,ncol=1)
for(r in seq(1,20)) Wk[r] <- Lcomoment.Wk(3,r,20)
# plot(seq(1,20),Wk)
}
\keyword{multivariate}
\keyword{distribution}
