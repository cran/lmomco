\name{pdfst3}
\alias{pdfst3}
\title{Probability Density Function of the 3-Parameter Student T Distribution}
\description{
This function computes the probability density of the 3-parameter Student T distribution given parameters (\eqn{\xi}, \eqn{\alpha}, \eqn{\nu}) of the distribution computed by \code{\link{parst3}}.  The probability density function has an explicit solution but not shown here. For value \code{X}, the built-in \R functions can be used. For \eqn{\nu \ge 1000}, \code{dnorm(X, mean=U, sd=A)} for \code{U} = \eqn{\xi} and \code{A}=\eqn{\alpha} for \eqn{1.000001 \le \nu \le 1000}, \code{dt((X-U)/A, N)/A} for \code{N}=\eqn{\nu} and where \code{dnorm()} is for the Normal distribution and \code{dt()} is the 1-parameter Student T distribution.
}
\usage{
pdfst3(x, para, paracheck=TRUE)
}
\arguments{
  \item{x}{A real value.}
  \item{para}{The parameters from \code{\link{parst3}} or similar.}
  \item{paracheck}{A logical on whether the parameter should be check for validity.}
}
\value{
  Probability density (\eqn{f}) for \eqn{x}.
}
\references{
Asquith, W.H., 2011, Distributional analysis with L-moment statistics using the R environment for statistical computing: CreateSpace Independent Publishing Platform, 2nd printing, ISBN 978-1463508418.
}
\author{ W.H. Asquith}
\seealso{\code{\link{cdfst3}}, \code{\link{quast3}}, \code{\link{parst3}}}
\examples{
\dontrun{
xs <- -200:200
   para <- vec2par(c(37,25,114), type="st3")
plot(xs, pdfst3(xs, para), type='l')
   para <- vec2par(c(11,36,1000), type="st3")
lines(xs, pdfst3(xs, para), lty=2)
   para <- vec2par(c(-7,60,40), type="st3")
lines(xs, pdfst3(xs, para), lty=3)
}
}
\keyword{distribution}
\keyword{probability density function}
