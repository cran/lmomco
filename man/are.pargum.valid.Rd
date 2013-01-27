\name{are.pargum.valid}
\alias{are.pargum.valid}
\title{Are the Distribution Parameters Consistent with the Gumbel Distribution}
\description{
The distribution parameter object returned by functions of this package such as by
\code{pargum} are consistent with the corresponding distribution,
otherwise a list would not have been returned.  However, other
functions (\code{cdfgum}, \code{quagum}, and
\code{lmomgum} require consistent parameters to return the cumulative
probability (nonexceedance), quantile, and L-moments of the distribution,
respectively.  These functions internally use the \code{are.pargum.valid}
function.  The FORTRAN source code of Hosking provides the basis
for the function.
}
\usage{
are.pargum.valid(para,nowarn=FALSE)
}
\arguments{
  \item{para}{A distribution parameter list returned by \code{pargum}.}
  \item{nowarn}{A logical switch on warning surpression. If \code{TRUE} then \code{options(warn=-1)} is made and restored on return. This switch is to permit calls in which warnings are not desired as the user knows how to handle the returned value---say in an optimization algorithm.}
}
\value{
  \item{TRUE}{If the parameters are \code{gum} consistent.}
  \item{FALSE}{If the parameters are not \code{gum} consistent.}
}
\references{
Hosking, J.R.M., 1996, FORTRAN routines for use with the method of L-moments:
Version 3, IBM Research Report RC20525, T.J. Watson Research Center,
Yorktown Heights, New York.

Hosking, J.R.M. and Wallis, J.R., 1997, Regional frequency analysis---An
approach based on L-moments: Cambridge University Press.
}
\note{
This function calls \code{is.gum} to verify consistency between
the distribution parameter object and the intent of the user.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{is.gum}} }
\examples{
para <- pargum(lmom.ub(c(123,34,4,654,37,78)))
if(are.pargum.valid(para)) Q <- quagum(0.5,para)
}
\keyword{utility (distribution)}
