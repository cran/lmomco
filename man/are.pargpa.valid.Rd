\name{are.pargpa.valid}
\alias{are.pargpa.valid}
\title{Are the Distribution Parameters Consistent with the Generalized Pareto Distribution}
\description{
The distribution parameter object returned by functions of this package such as by
\code{pargpa} are consistent with the corresponding distribution, 
otherwise a list would not have been returned.  However, other
functions (\code{cdfgpa}, \code{quagpa}, and
\code{lmomgpa} require consistent parameters to return the cumulative
probability (nonexceedance), quantile, and L-moments of the distribution,
respectively.  These functions internally use the \code{are.pargpa.valid}
function.  The FORTRAN source code of Hosking provides the basis
for the function.
}
\usage{
are.pargpa.valid(para)
}
\arguments{
  \item{para}{A distribution parameter list returned by \code{pargpa}.}
}
\value{
  \item{TRUE}{If the parameters are \code{gpa} consistent.}
  
  \item{FALSE}{If the parameters are not \code{gpa} consistent.}
}
\references{
Hosking, J.R.M., 1996, FORTRAN routines for use with the method of L-moments:
Version 3, IBM Research Report RC20525, T.J. Watson Research Center,
Yorktown Heights, New York.

Hosking, J.R.M. and Wallis, J.R., 1997, Regional frequency analysis--An
approach based on L-moments: Cambridge University Press.
}
\note{
This function calls \code{is.gpa} to verify consistency between 
the distribution parameter object and the intent of the user.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{is.gpa}} }
\examples{
para <- pargpa(lmom.ub(c(123,34,4,654,37,78)))
if(are.pargpa.valid(para)) Q <- quagpa(0.5,para)

}
\keyword{distribution}
