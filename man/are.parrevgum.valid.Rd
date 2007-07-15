\name{are.parrevgum.valid}
\alias{are.parrevgum.valid}
\title{Are the Distribution Parameters Consistent with the Reverse Gumbel Distribution}
\description{
The distribution parameter object returned by functions of this package such as by
\code{parrevgum} are consistent with the corresponding distribution, 
otherwise a list would not have been returned.  However, other
functions (\code{cdfrevgum}, \code{quarevgum}, and
\code{lmomrevgum} require consistent parameters to return the cumulative
probability (nonexceedance), quantile, and L-moments of the distribution,
respectively.  These functions internally use the \code{are.parrevgum.valid}
function.
}
\usage{
are.parrevgum.valid(para,nowarn=FALSE)
}
\arguments{
  \item{para}{A distribution parameter list returned by \code{parrevgum}.}
  \item{nowarn}{A logical switch on warning surpression. If \code{TRUE} then \code{options(warn=-1)} is made and restored on return. This switch is to permit calls in which warnings are not desired as the user knows how to handle the returned value---say in an optimization algorithm.}
}
\value{
  \item{TRUE}{If the parameters are \code{revgum} consistent.}
  \item{FALSE}{If the parameters are not \code{revgum} consistent.}
}
\references{
}
\note{
This function calls \code{is.revgum} to verify consistency between 
the distribution parameter object and the intent of the user.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{is.revgum}} }
\examples{
para <- vec2par(c(.9252, .1636, .7),type='revgum')
if(are.parrevgum.valid(para)) Q <- quarevgum(0.5,para)
}
\keyword{distribution}
