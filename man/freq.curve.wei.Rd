\name{freq.curve.wei}
\alias{freq.curve.wei}
\title{Frequency Curve of the Weibull Distribution}
\description{
This function returns the quantiles of the Weibull distribution given a
vector of nonexceedance probabilities and the parameters of the distribution.
Because in magnitude and frequency analysis the frequency curve is typically
the objective, this is a convenient function to increase analysis efficiency.
}
\usage{
freq.curve.wei(fs, para)
}
\arguments{
  \item{fs}{Vector of nonexceedance probabilities.}
  \item{para}{Parameters of the distribution as from \code{parwei}.}
}
\value{
  A vector of quantiles for the distribution.
}
\references{
Hosking, J.R.M. and Wallis, J.R., 1997, Regional frequency analysis---An
approach based on L-moments: Cambridge University Press.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{parwei}}, \code{\link{quawei}}, \code{\link{nonexceeds}} }
\examples{
  fs <- nonexceeds()
  lmr <- lmom.ub(c(123,34,4,654,37,78))
  para <- parwei(lmr)
  plot(fs,freq.curve.wei(fs,para))
}
\keyword{distribution}
