\name{LaguerreHalf}
\alias{LaguerreHalf}
\title{Laguerre Polynomial (Half)}
\description{
This function estimates the Laguerre polynomial, which is useful in applications involving the variance of the Rice distribution.
}
\usage{
LaguerreHalf(x)
}
\arguments{
  \item{x}{A value.}
}
\value{
The value for the Laguerre polynomial is returned.
}
\author{ W.H. Asquith}
\seealso{\code{\link{pdfrice}}}
\examples{
LaguerreHalf(-100^2/(2*10^2))
}
\keyword{special function}
