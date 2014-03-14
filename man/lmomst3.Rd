\name{lmomst3}
\alias{lmomst3}
\title{L-moments of the 3-Parameter Student T Distribution}
\description{
This function estimates the first six L-moments of the 3-parameter Student T distribution
given the parameters (\eqn{\xi}, \eqn{\alpha}, \eqn{\nu}) from \code{\link{parst3}}. The parameter \eqn{\tau_4(\nu)} was solved numerically and a polynomial approximation made. The polynomial in turn with a root-solver is used to solve \eqn{\nu(\tau_4)} in \code{\link{parst3}}. The other two parameters are readily solved for when \eqn{\nu} is available. The polynomial based on \eqn{\log{\tau_4}} and \eqn{\log{\nu}} has nine coefficients with a residual standard error (in natural logarithm units of \eqn{\tau_4}) of 0.0001565 for 3250 degrees of freedom and an adjusted R-squared of 1. A polynomial approximation is used to estimate the \eqn{\tau_6} as a function of \eqn{\tau_4}; the polynomial was based on the \code{\link{theoLmoms}} estimating \eqn{\tau_4} and \eqn{\tau_6}. The \eqn{\tau_6} polynomial has nine coefficients with a residual standard error units of \eqn{\tau_6} of 1.791e-06 for 3593 degrees of freedom and an adjusted R-squared of 1.
}
\usage{
lmomst3(para, bypoly=TRUE)
}
\arguments{
  \item{para}{The parameters of the distribution.}
  \item{bypoly}{A logical as to whether a polynomial approximation of \eqn{\tau_4} as a function of \eqn{\nu} will be used. The default is \code{TRUE} because this polynomial is used to reverse the estimate for \eqn{\nu} as a function of \eqn{\tau_4}. A polynomial of \eqn{\tau_6(\tau_4)} is always used.}
}
\value{
  An R \code{list} is returned.
}
\references{
Asquith, W.H., 2011, Distributional analysis with L-moment statistics using the R environment for statistical computing: CreateSpace Independent Publishing Platform, 2nd printing, ISBN 978-1463508418.
}
\author{ W.H. Asquith with A.R. Biessen}
\seealso{\code{\link{cdfst3}}, \code{\link{parst3}}, \code{\link{pdfst3}}, \code{\link{quast3}} }
\examples{
lmomst3(vec2par(c(1124,12.123,10), type="st3"))
}
\keyword{L-moment (distribution)}
