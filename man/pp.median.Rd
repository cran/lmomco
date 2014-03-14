\name{pp.median}
\alias{pp.median}
\title{ Quantile Function of the Ranks of Plotting Positions }
\description{
There are two major forms (outside of the general plotting-position formula \code{\link{pp}}) for estimation of the \eqn{p_r}th probability of the \eqn{r}th order statistic for a sample of size \eqn{n}: the mean is \eqn{p'_r = r/(n+1)} (Weibull plotting position) and the median is \eqn{p^\star_r = IIB(0.5, r, n+1-r)}, where \eqn{f=0.5} represents the median.  \eqn{IIB} is the \dQuote{inverse of the incomplete beta function} or the quantile function of the beta distribution as provided by \code{qbeta(f, a, b)}.  See Gilchrist (2011, chapter 12, p. 510 in Karian and Dudewicz, 2011). The \eqn{p'_r} are known in some fields as \dQuote{mean rankit} and \eqn{p^\star_r} as \dQuote{median rankit.}
}
\usage{
pp.median(x)
}
\arguments{
  \item{x}{A vector of data. The ranks and the length of the vector are computed within the function.}
}
\value{
  An R \code{vector} is returned.
}
\note{
The function internally calls \code{\link{pp.f}} uses the \code{rank()} function, which has specific settings to handle tied data. For implementation here, the \code{ties.method="first"} method to \code{rank()} is used.
}
\references{
Karian, Z.A., and Dudewicz, E.J., 2011, Handbook of fitting statistical distributions with R: Boca Raton, FL, CRC Press.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{pp}}, \code{\link{pp.f}} }
\examples{
\dontrun{
X <- rexp(10)*rexp(10)
means <- pp(X, sort=FALSE)
median  <- pp.median(X)
supposed.median <- pp(X, a=0.3175, sort=FALSE)
lmr <- lmoms(X)
par <- parwak(lmr)
F <- nonexceeds()
plot(F, qlmomco(F,par), type="l", log="y")
points(means, X)
points(median, X, col=2)
points(supposed.median, X, pch=16, col=2, cex=0.5)
# The plot shows that the median and supposed.median by the plotting-position
# formula are effectively equivalent. Thus, the partical application it seems
# that a=0.3175 would be good enough in lieu of the complexity of the
# quantile function of the beta distribution.
}
}
\keyword{plotting position}
\keyword{rankit}
