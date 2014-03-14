\name{cdfst3}
\alias{cdfst3}
\title{Cumulative Distribution Function of the 3-Parameter Student T Distribution}
\description{
This function computes the cumulative probability or nonexceedance probability of the  3-parameter Student T distribution given parameters (\eqn{\xi}, \eqn{\alpha}, \eqn{\nu}) of the distribution computed by \code{\link{parst3}}.  There is no explicit solution for the cumulative distribution function for value \code{X} but built-in \R functions can be used. For \eqn{\nu \ge 1000}, \code{pnorm(X, mean=U, sd=A)} for \code{U} = \eqn{\xi} and \code{A} = \eqn{\alpha} for \eqn{1.000001 \le \nu \le 1000}, \code{pt((X-U)/A, N)} for \code{N} = \eqn{\nu} and where \code{pnorm()} is for the Normal distribution and \code{pt()} is the 1-parameter Student T distribution.
}
\usage{
cdfst3(x, para, paracheck=TRUE)
}
\arguments{
  \item{x}{A real value.}
  \item{para}{The parameters from \code{\link{parst3}} or similar.}
  \item{paracheck}{A logical on whether the parameter should be check for validity.}
}
\value{
  Nonexceedance probability (\eqn{F}) for \eqn{x}.
}
\references{
Asquith, W.H., 2011, Distributional analysis with L-moment statistics using the R environment for statistical computing: CreateSpace Independent Publishing Platform, 2nd printing, ISBN 978-1463508418.
}
\author{ W.H. Asquith}
\seealso{\code{\link{quast3}}, \code{\link{parst3}}, \code{\link{pdfst3}}}
\examples{
  lmr <- lmoms(c(123,34,4,654,37,78))
  cdfst3(191.5143, parst3(lmr)) # 75th percentile
}
\keyword{distribution}
\keyword{cumulative distribution function}
