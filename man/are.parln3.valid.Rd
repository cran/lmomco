\name{are.parln3.valid}
\alias{are.parln3.valid}
\title{Are the Distribution Parameters Consistent with the 3-Parameter Log-Normal Distribution}
\description{
The distribution parameter object returned by functions of this package such as by
\code{pargum} are consistent with the corresponding distribution,
otherwise a list would not have been returned.  However, other
functions (\code{cdfln3}, \code{qualn3}, and
\code{lmomln3} require consistent parameters to return the cumulative
probability (nonexceedance), quantile, and L-moments of the distribution,
respectively.  These functions internally use the \code{are.parln3.valid}
function.
}
\usage{
are.parln3.valid(para,nowarn=FALSE)
}
\arguments{
  \item{para}{A distribution parameter list returned by \code{parln3}.}
  \item{nowarn}{A logical switch on warning surpression. If \code{TRUE} then \code{options(warn=-1)} is made and restored on return. This switch is to permit calls in which warnings are not desired as the user knows how to handle the returned value---say in an optimization algorithm.}
}
\value{
  \item{TRUE}{If the parameters are \code{ln3} consistent.}
  \item{FALSE}{If the parameters are not \code{ln3} consistent.}
}
\references{
NEED
}
\note{
This function calls \code{is.ln3} to verify consistency between
the distribution parameter object and the intent of the user.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{is.ln3}} }
\examples{
para <- parln3(lmom.ub(c(123,34,4,654,37,78)))
if(are.parln3.valid(para)) Q <- qualn3(0.5,para)
}
\keyword{utility (distribution)}
