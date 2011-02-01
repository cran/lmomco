\name{parkap}
\alias{parkap}
\title{Estimate the Parameters of the Kappa Distribution}
\description{
This function estimates the parameters of the Kappa distribution given
the L-moments of the data in an L-moment object such as that returned by
\code{lmom.ub}. The relation between distribution parameters and L-moments
is seen under \code{lmomkap}.
}
\usage{
parkap(lmom,checklmom=TRUE)
}
\arguments{
  \item{lmom}{A L-moment object created by \code{\link{lmom.ub}}
or \code{\link{pwm2lmom}}.}
  \item{checklmom}{Should the \code{lmom} be checked for validity using the \code{are.lmom.valid} function. Normally this should be left as the default and it is very unlikely that the L-moments will not be viable (particularly in the \eqn{\tau_4} and \eqn{\tau_3} inequality). However, for some circumstances or large simulation exercises then one might want to bypass this check.}
}

\value{
  An R \code{list} is returned.

  \item{type}{The type of distribution: \code{kap}.}
  \item{para}{The parameters of the distribution.}
  \item{source}{The source of the parameters: \dQuote{parkap}.}
  \item{support}{The support (or range) of the fitted distribution.}
  \item{ifail}{A numeric failure code.}
  \item{ifailtext}{A text message for the failure code.}
}
\references{
Hosking, J.R.M., 1990, L-moments---Analysis and estimation of
distributions using linear combinations of order statistics: Journal
of the Royal Statistical Society, Series B, vol. 52, p. 105--124.

Hosking, J.R.M., 1996, FORTRAN routines for use with the method of L-moments:
Version 3, IBM Research Report RC20525, T.J. Watson Research Center,
Yorktown Heights, New York.

Hosking, J.R.M. and Wallis, J.R., 1997, Regional frequency analysis---An
approach based on L-moments: Cambridge University Press.
}
\author{ W.H. Asquith}
\seealso{\code{\link{lmom.ub}}, \code{\link{lmomkap}},
\code{\link{cdfkap}}, \code{\link{quakap}}
}
\examples{
lmr <- lmom.ub(rnorm(20))
parkap(lmr)
}
\keyword{distribution}
