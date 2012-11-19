\name{parexp}
\alias{parexp}
\title{Estimate the Parameters of the Exponential Distribution}
\description{
This function estimates the parameters of the Exponential distribution given
the L-moments of the data in an L-moment object such as that returned by
\code{lmom.ub}. The relation between distribution parameters and L-moments
is seen under \code{lmomexp}.
}
\usage{
parexp(lmom,checklmom=TRUE)
}
\arguments{
  \item{lmom}{A L-moment object created by \code{\link{lmom.ub}} or
\code{\link{pwm2lmom}}.}
  \item{checklmom}{Should the \code{lmom} be checked for validity using the \code{are.lmom.valid} function. Normally this should be left as the default and it is very unlikely that the L-moments will not be viable (particularly in the \eqn{\tau_4} and \eqn{\tau_3} inequality). However, for some circumstances or large simulation exercises then one might want to bypass this check.}
}

\value{
  An R \code{list} is returned.

  \item{type}{The type of distribution: \code{exp}.}
  \item{para}{The parameters of the distribution.}
  \item{source}{The source of the parameters: \dQuote{parexp}.}
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
\seealso{\code{\link{lmom.ub}}, \code{\link{lmomexp}},
\code{\link{cdfexp}}, \code{\link{quaexp}}
}
\examples{
lmr <- lmom.ub(rnorm(20))
parexp(lmr)
}
\keyword{distribution}
