\name{quast3}
\alias{quast3}
\title{Quantile Function of the 3-Parameter Student T Distribution}
\description{
This function computes the quantiles of the 3-parameter Student T distribution given parameters (\eqn{\xi}, \eqn{\alpha}, \eqn{\nu}) of the distribution computed by \code{\link{parst3}}. There is no explicit solution for the quantile function for nonexceedance probability \code{F} but built-in \R functions can be used. For \eqn{\nu \ge 1000}, \code{qnorm(F, mean=U, sd=A)} for \code{U} = \eqn{\xi} and \code{A} = \eqn{\alpha} for \eqn{1.000001 \le \nu \le 1000}, \code{U + A*qt(F, N)} for \code{N} = \eqn{\nu} and where \code{qnorm()} is for the Normal distribution and \code{qt()} is the 1-parameter Student T distribution.
}
\usage{
quast3(f, para, paracheck=TRUE)
}
\arguments{
  \item{f}{Nonexceedance probability (\eqn{0 \le F \le 1}).}
  \item{para}{The parameters from \code{\link{parst3}} or similar.}
  \item{paracheck}{A logical on whether the parameter should be check for validity.}
}
\value{
  Quantile value for nonexceedance probability \eqn{F}.
}
\references{
Asquith, W.H., 2011, Distributional analysis with L-moment statistics using the R environment for statistical computing: CreateSpace Independent Publishing Platform, 2nd printing, ISBN 978-1463508418.
}
\author{ W.H. Asquith}
\seealso{\code{\link{cdfst3}}, \code{\link{parst3}}, \code{\link{pdfst3}}}
\examples{
  lmr <- lmoms(c(123,34,4,654,37,78))
  quast3(0.75,parst3(lmr))
}
\keyword{distribution}
\keyword{quantile function}
