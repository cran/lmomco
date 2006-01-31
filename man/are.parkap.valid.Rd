\name{are.parkap.valid}
\alias{are.parkap.valid}
\title{Are the Distribution Parameters Consistent with the Kappa Distribution}
\description{
The distribution parameter object returned by functions of this package such as by
\code{parkap} are consistent with the corresponding distribution, 
otherwise a list would not have been returned.  However, other
functions (\code{cdfkap}, \code{quakap}, and
\code{lmomkap} require consistent parameters to return the cumulative
probability (nonexceedance), quantile, and L-moments of the distribution,
respectively.  These functions internally use the \code{are.parkap.valid}
function.  The FORTRAN source code of Hosking provides the basis
for the function.
}
\usage{
are.parkap.valid(para)
}
\arguments{
  \item{para}{A distribution parameter list returned by \code{parkap}.}
}
\value{
  \item{TRUE}{If the parameters are \code{kap} consistent.}
  
  \item{FALSE}{If the parameters are not \code{kap} consistent.}
}
\references{
Hosking, J.R.M., 1996, FORTRAN routines for use with the method of L-moments:
Version 3, IBM Research Report RC20525, T.J. Watson Research Center,
Yorktown Heights, New York.

Hosking, J.R.M. and Wallis, J.R., 1997, Regional frequency analysis--An
approach based on L-moments: Cambridge University Press.
}
\note{
This function calls \code{is.kap} to verify consistency between 
the distribution parameter object and the intent of the user.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{is.kap}} }
\examples{
para <- parkap(lmom.ub(c(123,34,4,654,37,78)))
if(are.parkap.valid(para)) Q <- quakap(0.5,para)

}
\keyword{distribution}
