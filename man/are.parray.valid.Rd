\name{are.parray.valid}
\alias{are.parray.valid}
\title{Are the Distribution Parameters Consistent with the Rayleigh Distribution}
\description{
The distribution parameter object returned by functions of this package such as by
\code{parray} are consistent with the corresponding distribution,
otherwise a list would not have been returned.  However, other
functions (\code{cdfray}, \code{quaray}, and
\code{lmomray} require consistent parameters to return the cumulative
probability (nonexceedance), quantile, and L-moments of the distribution,
respectively.  These functions internally use the \code{are.parray.valid}
function.
}
\usage{
are.parray.valid(para,nowarn=FALSE)
}
\arguments{
  \item{para}{A distribution parameter list returned by \code{parray}.}
  \item{nowarn}{A logical switch on warning surpression. If \code{TRUE} then \code{options(warn=-1)} is made and restored on return. This switch is to permit calls in which warnings are not desired as the user knows how to handle the returned value---say in an optimization algorithm.}
}
\value{
  \item{TRUE}{If the parameters are \code{ray} consistent.}
  \item{FALSE}{If the parameters are not \code{ray} consistent.}
}
\references{
Hosking, J.R.M., 1986, The theory of probability weighted moments:
Research Report RC12210, IBM Research Division, Yorkton Heights, N.Y.
}
\note{
This function calls \code{is.ray} to verify consistency between
the distribution parameter object and the intent of the user.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{is.ray}} }
\examples{
para <- parray(lmom.ub(c(123,34,4,654,37,78)))
if(are.parray.valid(para)) Q <- quaray(0.5,para)

}
\keyword{distribution}
