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
                                    \frac{{k-1 \choose j}{k-1+j \choose j}
				          {r-1 \choose j}}
				         {{n-1 \choose j}}
      \mbox{.}}
}
\value{A single L-comoment weight factor.}
\note{
The function begins with a capital letter. This is intentionally done so that lower case
namespace is preserved. L-comoments are new in the literature and experimental in this package.
By using a capital letter now, then \code{lcomoment.Wk} remains an available name in future
releases.
}
\references{
Hosking, J.R.M. and Wallis, J.R., 1997, Regional frequency analysis---An
approach based on L-moments: Cambridge University Press.

Serfling, R., and Xiao, P., 2007, A contribution to multivariate L-moments---L-comoment matrices: Journal of Multivariate Analysis, v.~98, pp.~1765--1781.
}
\author{ W.H. Asquith}
\seealso{\code{\link{Lcomoment.Lk12}}}
\examples{
Wk <- Lcomoment.Wk(2,3,5)
print(Wk)

\dontrun{
# To compute the weight factors for L-skew and L-coskew (k=3) computation
# for a sample of size 20.
Wk <- matrix(nrow=20,ncol=1)
for(r in seq(1,20)) Wk[r] <- Lcomoment.Wk(3,r,20)
plot(seq(1,20),Wk, type="b")
}

# The following shows the actual weights used for computation of
# the first four L-moments. The sum of the each sample times the
# corresponding weight equals the L-moment.
fakedat <- sort(c(-10, 20, 30, 40));  n <- length(fakedat)
Wk1 <- Wk2 <- Wk3 <- Wk4 <- vector(mode="numeric", length=n);
for(i in 1:n) {
   Wk1[i] <- Lcomoment.Wk(1,i,n)/n
   Wk2[i] <- Lcomoment.Wk(2,i,n)/n
   Wk3[i] <- Lcomoment.Wk(3,i,n)/n
   Wk4[i] <- Lcomoment.Wk(4,i,n)/n
}
cat(c("Weights for mean",         round(Wk1, digits=4), "\n"))
cat(c("Weights for L-scale",      round(Wk2, digits=4), "\n"))
cat(c("Weights for 3rd L-moment", round(Wk3, digits=4), "\n"))
cat(c("Weights for 4th L-moment", round(Wk4, digits=4), "\n"))
my.lams <- c(sum(fakedat*Wk1), sum(fakedat*Wk2),
             sum(fakedat*Wk3), sum(fakedat*Wk4))
cat(c("Manual L-moments:", my.lams, "\n"))
cat(c("lmomco L-moments:", lmoms(fakedat, nmom=4)$lambdas,"\n"))
# The last two lines of output should be the same---note that lmoms()
# does not utilize Lcomoment.Wk(). So a double check is provided.
}
\keyword{multivariate}
\keyword{distribution}
