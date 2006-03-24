\name{lmomcau}
\alias{lmomcau}
\title{Trimmed L-moments of the Cauchy Distribution}
\description{
This function estimates the trimmed L-moments of the Cauchy distribution given
the parameters (\eqn{\xi} and \eqn{\alpha}) from \code{\link{parcau}}.
The trimmed L-moments in terms of the parameters are

\deqn{\lambda^{(1)}_1 = \xi \mbox{,}}
\deqn{\lambda^{(1)}_2 = 0.698\alpha \mbox{,}}
\deqn{\tau^{(1)}_3 = 0 \mbox{, and}}
\deqn{\tau^{(1)}_4 = 0.343 \mbox{.}}
}
\usage{
lmomcau(para)
}
\arguments{
  \item{para}{The parameters of the distribution.}
}
\value{
  An R \code{list} is returned.

  \item{lambdas}{Vector of the trimmed L-moments. First element is
\eqn{\lambda^{(1)}_1}, second element is \eqn{\lambda^{(1)}_2}, and so on.}
  \item{ratios}{Vector of the L-moment ratios. Second element is 
\eqn{\tau^{(1)}}, third element is \eqn{\tau^{(1)}_3} and so on. }
  \item{source}{An attribute identifying the computational source 
of the L-moments: \dQuote{lmomcau}}
  \item{trim}{Level of symmetrical trimming used---\code{trim=1}.}
}
\references{
Elamir, E.A.H., and Seheult, A.H., 2003, Trimmed L-moments: Computational Statistics
and Data Analysis, vol. 43, pp. 299--314.

Gilchrist, W.G., 2000, Statistical modeling with quantile functions: 
Chapman and Hall/CRC, Boca Raton, FL.
}
\author{ W.H. Asquith}
\seealso{\code{\link{parcau}}, \code{\link{quacau}}, \code{\link{cdfcau}} }
\examples{
X1 <- rcauchy(20)
lmomcau(parcau(TLmoms(X1,trim=1)))
}
\keyword{distribution}
