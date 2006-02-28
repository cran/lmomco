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
  
}
\details{
Unlike many of the other distributions in this package,
the parameter estimation occurs by passing the data into the function
and not from passing of an L-moment object (see \code{\link{lmom.ub}}).
Contrast this practice with \code{\link{pargum}}
for example.) The reason this is so is because the usual L-moments are
undefined for the Cauchy distribution, but the trimmed L-moments with
a symmetrical trimming parameter are defined. Specifically, the L-moments by
trimming the smallest and largest order statistic expections of the Cauchy are
defined by Elamir and Seheult (2003). The function \code{parcau} calls
\code{TLlmoms(x,trim=1)}) internally to compute the trimmed L-moments.
The relation between the parameters and the trimmed L-moments is

\deqn{\xi = \lambda^{(1)}_1 }

\deqn{\alpha = \frac{\lambda^{(1)}_2}{0.698} }

}
\references{
Elamir, E.A.H., and Seheult, A.H., 2003, Trimmed L-moments: Computational statistics
and data analysis, vol. 43, pp. 299-314.

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
\keyword{distribution}
