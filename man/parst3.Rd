\name{parst3}
\alias{parst3}
\title{Estimate the Parameters of the 3-Parameter Student T Distribution}
\description{
This function estimates the parameters of the 3-parameter Student T distribution given the L-moments of the data in an L-moment object such as that returned by \code{\link{lmoms}}. The L-moments in terms of the parameters are complicated and solved numerically. The largest value of \eqn{\nu} recognized is 1000, which is the Normal distribution and the smallest value recognized is 1.000001, which was arrived from manual experiments. As \eqn{\nu \rightarrow \infty} the distribution limits to the Cauchy, but the implementation here does not switch over to the Cauchy. Therefore in \pkg{lmomco} \eqn{1.000001 \le \nu \le 1000}. The \eqn{\nu} is the \dQuote{degrees of freedom} parameter that is well-known with the 1-parameter Student T distribution.
}
\usage{
parst3(lmom, checklmom=TRUE)
}
\arguments{
  \item{lmom}{A L-moment object created by \code{\link{lmoms}}
or \code{\link{pwm2lmom}}.}
  \item{checklmom}{Should the \code{lmom} be checked for validity using the \code{are.lmom.valid} function. Normally this should be left as the default and it is very unlikely that the L-moments will not be viable (particularly in the \eqn{\tau_4} and \eqn{\tau_3} inequality). However, for some circumstances or large simulation exercises then one might want to bypass this check.}
}
\value{
  An R \code{list} is returned.

  \item{type}{The type of distribution: \code{st3}.}
  \item{para}{The parameters of the distribution.}
  \item{source}{The source of the parameters: \dQuote{parst3}.}
}
\references{
Asquith, W.H., 2011, Distributional analysis with L-moment statistics using the R environment for statistical computing: CreateSpace Independent Publishing Platform, 2nd printing, ISBN 978-1463508418.
}
\author{ W.H. Asquith}
\seealso{\code{\link{cdfst3}}, \code{\link{lmomst3}}, \code{\link{pdfst3}}, \code{\link{quast3}}
}
\examples{
  parst3(vec2lmom(c(10,2,0,.1226)))$para
  parst3(vec2lmom(c(10,2,0,.14)))$para
  parst3(vec2lmom(c(10,2,0,0.2)))$para
  parst3(vec2lmom(c(10,2,0,0.4)))$para
  parst3(vec2lmom(c(10,2,0,0.9)))$para
}
\keyword{distribution (parameters)}
