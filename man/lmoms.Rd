\name{lmoms}
\alias{lmoms}
\title{ The Sample L-moments and L-moment Ratios }
\description{
Compute the sample L-moments for a vector.
}
\usage{
lmoms(x,nmom=5)
}
\arguments{
  \item{x}{A vector of data values.}
  \item{nmom}{The number of moments to compute. Default is 5.}
}
\value{
  An R \code{list} is returned.

  \item{lambdas}{Vector of the L-moments. First element is
\eqn{\hat{\lambda}^{(0,0)}_1}, second element is \eqn{\hat{\lambda}^{(0,0)}_2}, and so on.}
  \item{ratios}{Vector of the L-moment ratios. Second element is
\eqn{\hat{\tau}^{(0,0)}}, third element is \eqn{\hat{\tau}^{(0,0)}_3} and so on.}
  \item{trim}{Level of symmetrical trimming used in the computation, which will equal \code{NULL} if asymmetrical trimming was used.}
  \item{leftrim}{Level of left-tail trimming used in the computation.}
  \item{rightrim}{Level of right-tail trimming used in the computation.}
  \item{source}{An attribute identifying the computational source of the L-moments: \dQuote{lmoms}.}
}
\note{
This function computes the L-moments through the generalization of the TL-moments (\code{\link{TLmoms}}). In fact, this function calls the default TL-moments with no trimming of the sample. This function is equivalent to \code{\link{lmom.ub}}, but returns a different data structure.
}
\references{
Elamir, E.A.H., and Seheult, A.H., 2003, Trimmed L-moments: Computational statistics
and data analysis, vol. 43, pp. 299-314.

Hosking, J.R.M., 1990, L-moments---Analysis and estimation of
distributions using linear combinations of order statistics: Journal
of the Royal Statistical Society, Series B, vol. 52, p. 105--124.
}
\author{ W.H. Asquith}

\seealso{\code{\link{lmom.ub}}, \code{\link{TLmoms}}, \code{\link{lmorph}}, and \code{\link{lmoms.bernstein}}}
\examples{
X1 <- rnorm(30)
L5 <- lmoms(X1)
L4 <- lmoms(X1,nmom=4)
}
\keyword{L-moment (sample)}
