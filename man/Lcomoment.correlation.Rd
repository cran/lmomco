\name{Lcomoment.correlation}
\alias{Lcomoment.correlation}
\title{ L-correlation Matrix (L-correlation through Sample L-comoments) }
\description{
Compute the L-correlation from an L-comoment matrix of order \eqn{k = 2}.
This function assumes that each matrix is already computed by the function
\code{Lcomoment.matrix}.
}
\usage{
Lcomoment.correlation(L2)
}
\arguments{
  \item{L2}{A \eqn{k = 2} L-comoment matrix from \code{Lcomoment.matrix(Dataframe,k=2)}.}
}
\details{
L-correlation is computed by \code{Lcomoment.coefficients(L2,L2)} where \code{L2} is second order L-comoment matrix. The usual L-scale values as seen from \code{lmom.ub} or \code{lmoms} are along the diagonal. This function does not make use of \code{lmom.ub} or \code{lmoms} and can be used to verify  computation of \eqn{\tau} (coefficient of L-variation).
}
\value{
  An R \code{list} is returned.

  \item{type}{The type of L-comoment representation in the matrix: \dQuote{Lcomoment.coefficients}.}
  \item{order}{The order of the matrix---extracted from the first matrix in arguments.}
  \item{matrix}{A \eqn{k \ge 2} L-comoment coefficient matrix.}
}
\note{
The function begins with a capital letter. This is intentionally done so that lower case namespace is preserved. L-comoments are new in the literature and experimental in this package. By using a capital letter now, then \code{lcomoment.correlation} remains an available name in future releases.
}
\source{
Serfling and Xiao (2006).
}
\references{
Serfling, R., and Xiao, P., 2006, Multivariate L-moments, preprint.
}
\author{ W.H. Asquith}

\seealso{\code{lmom.ub}, \code{\link{Lcomoment.matrix}}, \code{\link{Lcomoment.correlation}}}
\examples{
D   <- data.frame(X1=rnorm(30),X2=rnorm(30),X3=rnorm(30))
L2  <- Lcomoment.matrix(D,k=2)
RHO <- Lcomoment.correlation(L2)
}
\keyword{multivariate}
\keyword{distribution}
