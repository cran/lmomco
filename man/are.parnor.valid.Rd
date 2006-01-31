\name{are.parnor.valid}
\alias{are.parnor.valid}
\title{Are the Distribution Parameters Consistent with the Normal Distribution}
\description{
The distribution parameter object returned by functions of this package such as by
\code{parnor} are consistent with the corresponding distribution, 
otherwise a list would not have been returned.  However, other
functions (\code{cdfnor}, \code{quanor}, and
\code{lmomnor} require consistent parameters to return the cumulative
probability (nonexceedance), quantile, and L-moments of the distribution,
respectively.  These functions internally use the \code{are.parnor.valid}
function.  The FORTRAN source code of Hosking provides the basis
for the function.
}
\usage{
are.parnor.valid(para)
}
\arguments{
  \item{para}{A distribution parameter list returned by \code{parnor}.}
}
\value{
  \item{TRUE}{If the parameters are \code{nor} consistent.}
  
  \item{FALSE}{If the parameters are not \code{nor} consistent.}
}
\references{
Hosking, J.R.M., 1996, FORTRAN routines for use with the method of L-moments:
Version 3, IBM Research Report RC20525, T.J. Watson Research Center,
Yorktown Heights, New York.

Hosking, J.R.M. and Wallis, J.R., 1997, Regional frequency analysis--An
approach based on L-moments: Cambridge University Press.
}
\note{
This function calls \code{is.nor} to verify consistency between 
the distribution parameter object and the intent of the user.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{is.nor}} }
\examples{
para <- parnor(lmom.ub(c(123,34,4,654,37,78)))
if(are.parnor.valid(para)) Q <- quanor(0.5,para)

}
\keyword{distribution}
