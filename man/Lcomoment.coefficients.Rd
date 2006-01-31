\name{Lcomoment.coefficients}
\alias{Lcomoment.coefficients}
\title{ L-comoment Coefficient Matrix }
\description{
Compute the L-comoment coefficients from an L-comoment matrix of order \eqn{k \ge 2}
and the \eqn{k = 2} (2nd order) L-comoment matrix. This function requires that each matrix is 
already computed by the function \code{Lcomoment.matrix}.
}
\usage{
Lcomoment.coefficients(Lk,L2)
}
\arguments{
  \item{Lk}{A \eqn{ k \ge 2} L-comoment matrix from \code{Lcomoment.matrix}.}
  \item{L2}{A \eqn{k = 2} L-comoment matrix from \code{Lcomoment.matrix(Dataframe,k=2)}.}
}
\details{
L-correlation is computed by \code{Lcomoment.coefficients(L2,L2)} where \code{L2} is
a \eqn{k = 2} L-comoment matrix.  L-coskew, L-cokurtosis, and so on are computed
by \code{Lcomoment.coefficients(L3,L2)}, \code{Lcomoment.coefficients(L4,L2)},
and so on. The usual univariate L-moments as seen from \code{lmom.ub} are
along the diagonal. This function does not make use of \code{lmom.ub}. The
L-correlation is computed by extraction of the diagonal and dividing each row
in the matrix by the corresponding value from the diagonal.
}
\value{
  An R \code{list} is returned.

  \item{type}{The type of L-comoment representation in the matrix: \dQuote{Lcomoment.coefficients}.}
  \item{order}{The order of the matrix--extracted from the first matrix in arguments.}
  \item{matrix}{A \eqn{k \ge 2} L-comoment coefficient matrix.}
}
\note{
The function begins with a capital letter. This is intentionally done so that lower case 
namespace is preserved. L-comoments are new in the literature and experimental in this package. 
By using a capital letter now, then \code{lcomoment.coefficients} remains an available name in future 
releases.
}
\source{
Serfling and Xiao (2006).
}
\references{
Serfling, R., and Xiao, P., 2006, Multivariate L-moments, preprint.
}
\author{ W.H. Asquith}

\seealso{\code{lmom.ub}, \code{\link{Lcomoment.matrix}}, \code{\link{Lcomoment.coefficients}}}
\examples{
D      <- data.frame(X1=rnorm(30),X2=rnorm(30),X3=rnorm(30))
L2     <- Lcomoment.matrix(D,k=2)
L3     <- Lcomoment.matrix(D,k=3)
LkTAU3 <- Lcomoment.coefficients(L3,L2)
}
\keyword{multivariate}
\keyword{distribution}
