\name{are.parlap.valid}
\alias{are.parlap.valid}
\title{Are the Distribution Parameters Consistent with the Laplace Distribution}
\description{
The distribution parameter object returned by functions of this package such as by
\code{parlap} are consistent with the corresponding distribution, otherwise a list would not have been returned.  However, other functions (\code{cdflap}, \code{qualap}, and \code{lmomlap} require consistent parameters to return the cumulative probability (nonexceedance), quantile, and L-moments of the distribution, respectively.  These functions internally use the \code{are.parlap.valid}
function.

\deqn{\alpha > 0\mbox{.}}

}
\usage{
are.parlap.valid(para,nowarn=FALSE)
}
\arguments{
  \item{para}{A distribution parameter list returned by \code{parlap}.}
  \item{nowarn}{A logical switch on warning surpression. If \code{TRUE} then \code{options(warn=-1)} is made and restored on return. This switch is to permit calls in which warnings are not desired as the user knows how to handle the returned value---say in an optimization algorithm.}
}
\value{
  \item{TRUE}{If the parameters are \code{lap} consistent.}
  \item{FALSE}{If the parameters are not \code{lap} consistent.}
}
\references{
Hosking, J.R.M., 1986, The theory of probability weighted moments: IBM Research Report RC12210, T.J. Watson Research Center,
Yorktown Heights, New York.
}
\note{
This function calls \code{is.lap} to verify consistency between
the distribution parameter object and the intent of the user.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{is.lap}} }
\examples{
para <- parlap(lmom.ub(c(123,34,4,654,37,78)))
if(are.parlap.valid(para)) Q <- qualap(0.5,para)
}
\keyword{distribution}
