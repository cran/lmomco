\name{INT.kapicase2}
\alias{INT.kapicase2}
\title{ INTernal Function for Kappa Distribution--ICASE 2 }
\description{
This is an internal function supporting flags named ICASE in the Hosking FORTRAN
algorithms related to the processing of the Kappa distribution. Users are not expected to have
any need to use this function themselves.
}
\usage{
INT.kapicase2(U, A, G, H)
}
\arguments{
  \item{U}{Location parameter of Kappa distribution.}
  \item{A}{Scale parameter of Kappa distribution.}
  \item{G}{Shape parameter of Kappa distribution.}
  \item{H}{Higher-shape parameter of Kappa distribution.}
}
\value{
  The Probability-Weighted Moments of the Kappa distribution for the ICASE number.
}
\references{
Hosking, J.R.M., 1996, FORTRAN routines for use with the method of L-moments:
Version 3, IBM Research Report RC20525, T.J. Watson Research Center,
Yorktown Heights, New York.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{lmomkap}} }
\keyword{distribution}
