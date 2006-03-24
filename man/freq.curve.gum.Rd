\name{freq.curve.gum}
\alias{freq.curve.gum}
\title{Frequency Curve of the Gumbel Distribution}
\description{
This function returns the quantiles of the Gumbel distribution given a
vector of nonexceedance probabilities and the parameters of the distribution.
Because in magnitude and frequency analysis the frequency curve is typically
the objective, this is a convenient function to increase analysis efficiency.
}
\usage{
freq.curve.gum(fs, para)
}
\arguments{
  \item{fs}{Vector of nonexceedance probabilities.}
  \item{para}{Parameters of the distribution as from \code{pargum}.}
}
\value{
  A vector of quantiles for the distribution.
}
\references{
Hosking, J.R.M., 1990, L-moments---Analysis and estimation of
distributions using linear combinations of order statistics: Journal
of the Royal Statistical Society, Series B, vol. 52, p. 105--124.

Hosking, J.R.M., 1996, FORTRAN routines for use with the method of L-moments:
Version 3, IBM Research Report RC20525, T.J. Watson Research Center,
Yorktown Heights, New York.

Hosking, J.R.M. and Wallis, J.R., 1997, Regional frequency analysis---An
approach based on L-moments: Cambridge University Press.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{pargum}}, \code{\link{quagum}}, \code{\link{nonexceeds}} }
\examples{
  fs <- nonexceeds()
  lmr <- lmom.ub(c(123,34,4,654,37,78))
  para <- pargum(lmr)
  plot(fs,freq.curve.gum(fs,para))
}
\keyword{distribution}
