\name{pargov}
\alias{pargov}
\title{Estimate the Parameters of the Govindarajulu Distribution}
\description{
This function estimates the parameters of the Govindarajulu distribution given
the L-moments of the data in an L-moment object such as that returned by
\code{lmom.ub}. The relation between distribution parameters and L-moments
is seen under \code{lmomgov}.
}
\usage{
pargov(lmom, checklmom=TRUE)
}
\arguments{
  \item{lmom}{A L-moment object created by \code{\link{lmom.ub}}
or \code{\link{pwm2lmom}}.}
  \item{checklmom}{Should the \code{lmom} be checked for validity using the \code{are.lmom.valid} function. Normally this should be left as the default and it is very unlikely that the L-moments will not be viable (particularly in the \eqn{\tau_4} and \eqn{\tau_3} inequality). However, for some circumstances or large simulation exercises then one might want to bypass this check.}
}

\value{
  An R \code{list} is returned.

  \item{type}{The type of distribution: \code{gov}.}
  \item{para}{The parameters of the distribution.}
  \item{source}{The source of the parameters: \dQuote{pargov}.}
}
\references{
Gilchrist, W.G., 2000, Statistical modelling with quantile functions: Chapman and Hall/CRC, Boca Raton.

Nair, N.U., Sankaran, P.G., Balakrishnan, N., 2013, Quantile-based reliability analysis: Springer, New York.
}
\author{ W.H. Asquith}
\seealso{\code{\link{lmom.ub}}, \code{\link{lmomgov}},
\code{\link{cdfgov}}, \code{\link{pdfgov}}, \code{\link{quagov}}
}
\examples{
lmr <- lmom.ub(rnorm(20))
pargov(lmr)
}
\keyword{distribution (parameters)}
