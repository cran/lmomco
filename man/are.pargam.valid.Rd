\name{are.pargam.valid}
\alias{are.pargam.valid}
\title{Are the Distribution Parameters Consistent with the Gamma Distribution}
\description{
The distribution parameter object returned by functions of this package such as by
\code{pargam} are consistent with the corresponding distribution, 
otherwise a list would not have been returned.  However, other
functions (\code{cdfgam}, \code{quagam}, and
\code{lmomgam} require consistent parameters to return the cumulative
probability (nonexceedance), quantile, and L-moments of the distribution,
respectively.  These functions internally use the \code{are.pargam.valid}
function.  The FORTRAN source code of Hosking provides the basis
for the function. The parameters are restricted to the following conditions.

\deqn{\alpha > 0 \mbox{ and } \beta > 0\mbox{.}}
}
\usage{
are.pargam.valid(para,nowarn=FALSE)
}
\arguments{
  \item{para}{A distribution parameter list returned by \code{pargam}.}
  \item{nowarn}{A logical switch on warning surpression. If \code{TRUE} then \code{options(warn=-1)} is made and restored on return. This switch is to permit calls in which warnings are not desired as the user knows how to handle the returned value---say in an optimization algorithm.}
}
\value{
  \item{TRUE}{If the parameters are \code{gam} consistent.}
  \item{FALSE}{If the parameters are not \code{gam} consistent.}
}
\references{
Hosking, J.R.M., 1996, FORTRAN routines for use with the method of L-moments:
Version 3, IBM Research Report RC20525, T.J. Watson Research Center,
Yorktown Heights, New York.

Hosking, J.R.M. and Wallis, J.R., 1997, Regional frequency analysis---An
approach based on L-moments: Cambridge University Press.
}
\note{
This function calls \code{is.gam} to verify consistency between 
the distribution parameter object and the intent of the user.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{is.gam}} }
\examples{
para <- pargam(lmom.ub(c(123,34,4,654,37,78)))
if(are.pargam.valid(para)) Q <- quagam(0.5,para)

}
\keyword{distribution}
