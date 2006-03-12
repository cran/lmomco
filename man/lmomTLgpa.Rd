\name{lmomTLgpa}
\alias{lmomTLgpa}
\title{L-moments of the Generalized Pareto Distribution}
\description{
This function estimates the trimmed L-moments (TL-moments) for \eqn{t=1} of the Generalized Pareto distribution
given the parameters (\eqn{\xi}, \eqn{\alpha}, and \eqn{\kappa}) from \code{\link{parTLgpa}}.
The TL-moments in terms of the parameters are

\deqn{\lambda_1 = \xi + \frac{\alpha(\kappa+5)}{(\kappa+3)(\kappa+2)}}
\deqn{\lambda_2 = \frac{6\alpha}{(\kappa+4)(\kappa+3)(\kappa+2}}
\deqn{\tau_3 = \frac{10(1-\kappa)}{9(\kappa+5)}}
\deqn{\tau_4 = \frac{5(\kappa-1)(\kappa-2)}{4(\kappa+6)(\kappa+5)}}
}
\usage{
lmomTLgpa(para)
}
\arguments{
  \item{para}{The parameters of the distribution.}
}
\value{
  An R \code{list} is returned.

  \item{lambdas}{Vector of the TL-moments. First element is
\eqn{\lambda^{(1)}_1}, second element is \eqn{\lambda^{(1)}_2}, and so on.}
  \item{ratios}{Vector of the L-moment ratios. Second element is 
\eqn{\tau^{(1)}}, third element is \eqn{tau^{(1)}_3} and so on. }
  \item{trim}{Trim level = 1}
  \item{source}{An attribute identifying the computational source 
of the TL-moments: \dQuote{lmomTLgpa}.}
}
\references{
Elamir, E.A.H., and Seheult, A.H., 2003, Trimmed L-moments: Computational statistics
and data analysis, vol. 43, pp. 299-314.

Hosking, J.R.M., 1990, L-moments--Analysis and estimation of
distributions using linear combinations of order statistics: Journal
of the Royal Statistical Society, Series B, vol. 52, p. 105-124.

Hosking, J.R.M. and Wallis, J.R., 1997, Regional frequency analysis--An
approach based on L-moments: Cambridge University Press.
}
\author{ W.H. Asquith}
\seealso{\code{\link{parTLgpa}}, \code{\link{quagpa}}, \code{\link{cdfgpa}} }
\examples{
TL <- TLmoms(c(123,34,4,654,37,78,21,3400),trim=1)
TL
lmomTLgpa(parTLgpa(TL))
}
\keyword{distribution}
