\name{are.parwak.valid}
\alias{are.parwak.valid}
\title{Are the Distribution Parameters Consistent with the Wakeby Distribution}
\description{
The distribution parameter object returned by functions of this package such as by
\code{parwak} are consistent with the corresponding distribution, 
otherwise a list would not have been returned.  However, other
functions (\code{cdfwak}, \code{quawak}, and
\code{lmomwak} require consistent parameters to return the cumulative
probability (nonexceedance), quantile, and L-moments of the distribution,
respectively.  These functions internally use the \code{are.parwak.valid}
function.  The FORTRAN source code of Hosking provides the basis
for the function.
}
\usage{
are.parwak.valid(para)
}
\arguments{
  \item{para}{A distribution parameter list returned by \code{parwak}.}
}
\value{
  \item{TRUE}{If the parameters are \code{wak} consistent.}

  \item{FALSE}{If the parameters are not \code{wak} consistent.}
}
\references{
Hosking, J.R.M., 1996, FORTRAN routines for use with the method of L-moments:
Version 3, IBM Research Report RC20525, T.J. Watson Research Center,
Yorktown Heights, New York.

Hosking, J.R.M. and Wallis, J.R., 1997, Regional frequency analysis--An
approach based on L-moments: Cambridge University Press.
}
\note{
This function calls \code{is.wak} to verify consistency between 
the distribution parameter object and the intent of the user.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{is.wak}} }
\examples{
para <- parwak(lmom.ub(c(123,34,4,654,37,78)))
if(are.parwak.valid(para)) Q <- quawak(0.5,para)

}
\keyword{distribution}
