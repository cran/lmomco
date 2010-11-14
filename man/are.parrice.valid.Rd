\name{are.parrice.valid}
\alias{are.parrice.valid}
\title{Are the Distribution Parameters Consistent with the Rice Distribution}
\description{
The distribution parameter object returned by functions of this package such as by
\code{parrice} are consistent with the corresponding distribution,
otherwise a list would not have been returned.  However, other
functions (\code{cdfrice}, \code{quarice}, and
\code{lmomrice} require consistent parameters to return the cumulative
probability (nonexceedance), quantile, and L-moments of the distribution,
respectively.  These functions internally use the \code{are.parrice.valid}
function.
}
\usage{
are.parrice.valid(para,nowarn=FALSE)
}
\arguments{
  \item{para}{A distribution parameter list returned by \code{parrice}.}
  \item{nowarn}{A logical switch on warning surpression. If \code{TRUE} then \code{options(warn=-1)} is made and restored on return. This switch is to permit calls in which warnings are not desired as the user knows how to handle the returned value---say in an optimization algorithm.}
}
\value{
  \item{TRUE}{If the parameters are \code{rice} consistent.}
  \item{FALSE}{If the parameters are not \code{rice} consistent.}
}
\note{
This function calls \code{is.rice} to verify consistency between
the distribution parameter object and the intent of the user.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{is.rice}} }
\examples{
#para <- parrice(lmom.ub(c(123,34,4,654,37,78)))
#if(are.parrice.valid(para)) Q <- quarice(0.5,para)

}
\keyword{distribution}
