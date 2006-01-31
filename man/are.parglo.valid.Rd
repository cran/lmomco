\name{are.parglo.valid}
\alias{are.parglo.valid}
\title{Are the Distribution Parameters Consistent with the Generalized Logistic Distribution}
\description{
The distribution parameter object returned by functions of this package such as by
\code{parglo} are consistent with the corresponding distribution, 
otherwise a list would not have been returned.  However, other
functions (\code{cdfglo}, \code{quaglo}, and
\code{lmomglo} require consistent parameters to return the cumulative
probability (nonexceedance), quantile, and L-moments of the distribution,
respectively.  These functions internally use the \code{are.parglo.valid}
function.  The FORTRAN source code of Hosking provides the basis
for the function.
}
\usage{
are.parglo.valid(para)
}
\arguments{
  \item{para}{A distribution parameter list returned by \code{parglo}.}
}
\value{
  \item{TRUE}{If the parameters are \code{glo} consistent.}
  
  \item{FALSE}{If the parameters are not \code{glo} consistent.}
}
\references{
Hosking, J.R.M., 1996, FORTRAN routines for use with the method of L-moments:
Version 3, IBM Research Report RC20525, T.J. Watson Research Center,
Yorktown Heights, New York.

Hosking, J.R.M. and Wallis, J.R., 1997, Regional frequency analysis--An
approach based on L-moments: Cambridge University Press.
}
\note{
This function calls \code{is.glo} to verify consistency between 
the distribution parameter object and the intent of the user.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{is.glo}} }
\examples{
para <- parglo(lmom.ub(c(123,34,4,654,37,78)))
if(are.parglo.valid(para)) Q <- quaglo(0.5,para)

}
\keyword{distribution}
