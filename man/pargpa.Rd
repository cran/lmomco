\name{pargpa}
\alias{pargpa}
\title{Estimate the Parameters of the Generalized Pareto Distribution}
\description{
This function estimates the parameters of the Generalized Pareto distribution given
the L-moments of the data in an L-moment object such as that returned by
\code{lmom.ub}.
}
\usage{
pargpa(lmom)
}
\arguments{
  \item{lmom}{A L-moment object created by \code{\link{lmom.ub}}
or \code{\link{pwm2lmom}}.}
}

\value{
  An R \code{list} is returned.

  \item{type}{The type of distribution: \code{gpa}.}
  \item{para}{The parameters of the distribution.}
}
\references{
Hosking, J.R.M., 1990, L-moments--Analysis and estimation of
distributions using linear combinations of order statistics: Journal
of the Royal Statistical Society, Series B, vol. 52, p. 105-124.

Hosking, J.R.M., 1996, FORTRAN routines for use with the method of L-moments:
Version 3, IBM Research Report RC20525, T.J. Watson Research Center,
Yorktown Heights, New York.

Hosking, J.R.M. and Wallis, J.R., 1997, Regional frequency analysis--An
approach based on L-moments: Cambridge University Press.
}
\author{ W.H. Asquith}
\seealso{\code{\link{lmom.ub}}, \code{\link{lmomgpa}},
\code{\link{cdfgpa}}, \code{\link{quagpa}}
}
\examples{
lmr <- lmom.ub(rnorm(20))
pargpa(lmr)
}
\keyword{distribution}
