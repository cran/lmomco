\name{parcau}
\alias{parcau}
\title{Estimate the Parameters of the Cauchy Distribution}
\description{
This function estimates the parameters of the Cauchy distribution from the
trimmed L-moments (TL-moments) having trim level 1.
}
\usage{
parcau(lmom)
}
\arguments{
  \item{lmom}{TL-moments from \code{\link{TLmoms}} with \code{trim=1}.}
}
\value{
  An R \code{list} is returned.

  \item{type}{The type of distribution: \code{cau}.}
  \item{para}{The parameters of the distribution.}
  \item{source}{The source of the parameters: \dQuote{parcau}.}
}
\references{
Elamir, E.A.H., and Seheult, A.H., 2003, Trimmed L-moments: Computational Statistics
and Data Analysis, vol. 43, pp. 299--314.

Gilchrist, W.G., 2000, Statistical modeling with quantile functions:
Chapman and Hall/CRC, Boca Raton, FL.
}
\author{ W.H. Asquith}
\seealso{\code{\link{TLmoms}}, \code{\link{lmomcau}}
}
\examples{
X1 <- rcauchy(20)
parcau(TLmoms(X1,trim=1))
}
\keyword{distribution (parameters)}
