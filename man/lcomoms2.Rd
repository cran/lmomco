\name{lcomoms2}
\alias{lcomoms2}
\title{ The Sample L-comoments for Two Variables }
\description{
Compute the sample L-moments for two variable \code{data.frame}. The \dQuote{2} in the function name is to refer to fact that this function operates on only two variables. The length of the variables must be greater than the number of L-comoments requested.
}
\usage{
lcomoms2(DATAFRAME, nmom=3, asdiag=FALSE, opdiag=FALSE, ...)
}
\arguments{
  \item{DATAFRAME}{A vector of data values.}
  \item{nmom}{The number of moments to compute. Default is 3.}
  \item{asdiag}{Return the \code{diag}onal of the matrices. Default is \code{FALSE}.}
  \item{opdiag}{Return the opposing diagonal of the matrices. Default is \code{FALSE}. This function returns the opposing diagonal from first two to second.}
  \item{...}{Additional arguments to pass.}
}
\value{
  An R \code{list} is returned of the first

  \item{L1}{Matrix or diagonals of first L-comoment.}
  \item{L2}{Matrix or diagonals of second L-comoment.}
  \item{T2}{Matrix or diagonals of L-comoment correlation.}
  \item{T3}{Matrix or diagonals of L-comoment skew.}
  \item{T4}{Matrix or diagonals of L-comoment kurtosis.}
  \item{T5}{Matrix or diagonals of L-comoment Tau5.}
  \item{source}{An attribute identifying the computational source of the L-comoments: \dQuote{lcomoms2}.}
}
\note{
This function computes the L-comoments through the generalization of the \code{\link{Lcomoment.matrix}} and \code{\link{Lcomoment.coefficients}} functions.
}
\references{
Serfling, R., and Xiao, P., 2007, A contribution to multivariate L-moments---L-comoment matrices: Journal of Multivariate Analysis, v. 98, pp. 1765--1781.
}
\author{ W.H. Asquith}
\seealso{\code{\link{Lcomoment.matrix}} and \code{\link{Lcomoment.coefficients}}}
\examples{
# Random simulation of standard normal and then combine with
# a random standard exponential distribution
X <- rnorm(200); Y <- X + rexp(200)

z <- lcomoms2(data.frame(X=X, Y=Y))
print(z)


z <- lcomoms2(data.frame(X=X, Y=Y), diag=TRUE)
print(z$T3) # the L-skew values of the margins

z <- lcomoms2(data.frame(X=X, Y=Y), opdiag=TRUE)
print(z$T3) # the L-coskew values
}
\keyword{multivariate}
