\name{freq.curve.cau}
\alias{freq.curve.cau}
\title{Frequency Curve of the Cauchy Distribution}
\description{
This function returns the quantiles of the Cauchy distribution given a
vector of nonexceedance probabilities and the parameters of the distribution.
Because in magnitude and frequency analysis the frequency curve is typically
the objective, this is a convenient function to increase analysis efficiency.
}
\usage{
freq.curve.cau(fs, para)
}
\arguments{
  \item{fs}{Vector of nonexceedance probabilities.}
  \item{para}{Parameters of the distribution from \code{parcau} or \code{vec2par}.}
}
\value{
  A vector of quantiles for the distribution.
}
\references{
Elamir, E.A.H., and Seheult, A.H., 2003, Trimmed L-moments: Computational statistics
and data analysis, vol. 43, pp. 299-314.

Gilchrist, W.G., 2000, Statistical modeling with quantile functions: 
Chapman and Hall/CRC, Boca Raton, FL.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{quacau}}, \code{\link{nonexceeds}} }
\examples{
  fs <- nonexceeds()
  para <- vec2par(c(12,12),type='cau')
  plot(fs,freq.curve.cau(fs,para))
}
\keyword{distribution}
