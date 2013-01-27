\name{Lcomoment.coefficients}
\alias{Lcomoment.coefficients}
\title{ L-comoment Coefficient Matrix }
\description{
Compute the L-comoment coefficients from an L-comoment matrix of order \eqn{k \ge 2} and the \eqn{k = 2} (2nd order) L-comoment matrix. However, if the first argument is 1st-order then the coefficients of L-covariation are computed. The function requires that each matrix has already computed by the function \code{Lcomoment.matrix}.
}
\usage{
Lcomoment.coefficients(Lk,L2)
}
\arguments{
  \item{Lk}{A \eqn{ k \ge 2} L-comoment matrix from \code{Lcomoment.matrix}.}
  \item{L2}{A \eqn{k = 2} L-comoment matrix from \code{Lcomoment.matrix(Dataframe,k=2)}.}
}
\details{
The coefficient of L-variation is computed by \code{Lcomoment.coefficients(L1,L2)} where \code{L1} is a 1st-order L-moment matrix and \code{L2} is a \eqn{k = 2} L-comoment matrix. Symbolically, the coefficient of L-covariation is

  \deqn{ \hat{\tau}_{[12]} = \frac{\hat{\lambda}_{2[12]}}
                                   {\hat{\lambda}_{1[12]}} \mbox{.}}

The higher L-comoment coefficients (L-coskew, L-cokurtosis, \ldots) are computed by the function \code{Lcomoment.coefficients(L3,L2)} (\eqn{k=3}),  \code{Lcomoment.coefficients(L4,L2)} (\eqn{k=4}), and so on. Symbolically, the higher L-comoment coefficients are

  \deqn{ \hat{\tau}_{k[12]} = \frac{\hat{\lambda}_{k[12]}}
                                   {\hat{\lambda}_{2[12]}}
                                   \mbox{, for } k \ge 3 \mbox{.}}

Finally, the usual univariate L-moment ratios as seen from \code{lmom.ub} or \code{lmoms} are along the diagonal. The \code{Lcomoment.coefficients} function does not make use of \code{lmom.ub} or \code{lmoms}.
}
\value{
  An R \code{list} is returned.

  \item{type}{The type of L-comoment representation in the matrix: \dQuote{Lcomoment.coefficients}.}
  \item{order}{The order of the coefficients. \code{order=2} L-covariation, \code{order=3} L-coskew, \ldots}
  \item{matrix}{A \eqn{k \ge 2} L-comoment coefficient matrix.}
}
\note{
The function begins with a capital letter. This is intentionally done so that lower case namespace is preserved. L-comoments are new in the literature and experimental in this package. By using a capital letter now, then \code{lcomoment.coefficients} remains an available name in future releases.
}
\references{
Serfling, R., and Xiao, P., 2007, A contribution to multivariate L-moments---L-comoment matrices: Journal of Multivariate Analysis, v. 98, pp. 1765--1781.
}
\author{ W.H. Asquith}

\seealso{\code{lmom.ub}, \code{\link{Lcomoment.matrix}}, \code{\link{Lcomoment.coefficients}}}
\examples{
D      <- data.frame(X1=rnorm(30),X2=rnorm(30),X3=rnorm(30))
L1     <- Lcomoment.matrix(D,k=1)
L2     <- Lcomoment.matrix(D,k=2)
L3     <- Lcomoment.matrix(D,k=3)
LkLCV  <- Lcomoment.coefficients(L1,L2)
LkTAU3 <- Lcomoment.coefficients(L3,L2)
}
\keyword{multivariate}
