\name{pp.f}
\alias{pp.f}
\title{ Quantile Function of the Ranks of Plotting Positions }
\description{
There are two major forms (outside of the general plotting-position formula \code{\link{pp}}) for estimation of the \eqn{p_r}th probability of the \eqn{r}th order statistic for a sample of size \eqn{n}: the mean is \eqn{p'_r = r/(n+1)} (Weibull plotting position) and the median is \eqn{p^\star_r = IIB(0.5, r, n+1-r)}, where \eqn{f} represents the nonexceedance probability of the plotting position.  \eqn{IIB} is the \dQuote{inverse of the incomplete beta function} or the quantile function of the beta distribution as provided by \code{qbeta(f, a, b)}.  If \eqn{f=0.5}, then the median is returned but conveniently implemented in \code{\link{pp.median}}.   See Gilchrist (2011, chapter 12, p. 510 in Karian and Dudewicz, 2011).
}
\usage{
pp.f(f, x)
}
\arguments{
  \item{f}{A nonexceedance probability;}
  \item{x}{A vector of data. The ranks and the length of the vector are computed within the function.}
}
\value{
  An R \code{vector} is returned.
}
\note{
The function uses the \code{rank()} function, which has specific settings to handle tied data. For implementation here, the \code{ties.method="first"} method to \code{rank()} is used.
}
\references{
Karian, Z.A., and Dudewicz, E.J., 2011, Handbook of fitting statistical distributions with R: Boca Raton, FL, CRC Press.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{pp}} \code{\link{pp.median}} }
\examples{
#
}
\keyword{plotting position}
\keyword{rankit}
