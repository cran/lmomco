\name{freq.curve.gld}
\alias{freq.curve.gld}
\title{Frequency Curve of the Generalized Lambda Distribution}
\description{
This function returns the quantiles of the Generalized Lambda distribution given a
vector of nonexceedance probabilities and the parameters of the distribution.
Because in magnitude and frequency analysis the frequency curve is typically
the objective, this is a convenient function to increase analysis efficiency.
}
\usage{
freq.curve.gld(fs, para)
}
\arguments{
  \item{fs}{Vector of nonexceedance probabilities.}
  \item{para}{Parameters of the distribution as from \code{vec2par}.}
}
\value{
  A vector of quantiles for the distribution.
}
\references{
Karian, Z.A., and Dudewicz, E.J., 2000, Fitting statistical distributions--
The generalized lambda distribution and generalized bootstrap methods: 
CRC Press, Boca Raton, FL, 438 p.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{quagld}}, \code{\link{nonexceeds}} }
\examples{
  fs <- nonexceeds()
  para <- vec2par(c(123,34,4,3),type="gld")
  plot(fs,freq.curve.gld(fs,para))
}
\keyword{distribution}
