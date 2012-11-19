\name{are.partexp.valid}
\alias{are.partexp.valid}
\title{Are the Distribution Parameters Consistent with the Truncated Exponential Distribution}
\description{
The distribution parameter object returned by functions of this package such as by
\code{partexp} are consistent with the corresponding distribution,
otherwise a list would not have been returned.  However, other
functions (\code{cdftexp}, \code{quatexp}, and
\code{lmomtexp} require consistent parameters to return the cumulative
probability (nonexceedance), quantile, and L-moments of the distribution,
respectively.  These functions internally use the \code{are.partexp.valid}
function.  The FORTRAN source code of Hosking provides the basis
for the function.
}
\usage{
are.partexp.valid(para,nowarn=FALSE)
}
\arguments{
  \item{para}{A distribution parameter list returned by \code{parexp}.}
  \item{nowarn}{A logical switch on warning surpression. If \code{TRUE} then \code{options(warn=-1)} is made and restored on return. This switch is to permit calls in which warnings are not desired as the user knows how to handle the returned value---say in an optimization algorithm.}
}
\value{
  \item{TRUE}{If the parameters are \code{texp} consistent.}
  \item{FALSE}{If the parameters are not \code{texp} consistent.}
}
\references{
Hosking, J.R.M., 1996, FORTRAN routines for use with the method of L-moments:
Version 3, IBM Research Report RC20525, T.J. Watson Research Center,
Yorktown Heights, New York.

Hosking, J.R.M. and Wallis, J.R., 1997, Regional frequency analysis---An
approach based on L-moments: Cambridge University Press.
}
\note{
This function calls \code{is.texp} to verify consistency between
the distribution parameter object and the intent of the user.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{is.texp}} }
\examples{
para <- parexp(lmom.ub(c(123,34,4,654,37,78)))
if(are.parexp.valid(para)) Q <- quaexp(0.5,para)

}
\keyword{distribution}
