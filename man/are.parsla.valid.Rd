\name{are.parsla.valid}
\alias{are.parsla.valid}
\title{Are the Distribution Parameters Consistent with the Slash Distribution}
\description{
The distribution parameter object returned by functions of this package such as by \code{vec2par} are consistent with the corresponding distribution, otherwise a list would not have been returned.  However, other functions (\code{cdfsla} and \code{quasla}) require consistent parameters to return the cumulative probability (nonexceedance), quantile, and L-moments of the distribution, respectively.
}
\usage{
are.parsla.valid(para, nowarn=FALSE)
}
\arguments{
  \item{para}{A distribution parameter list returned by \code{parsla} or \code{vec2par}.}
  \item{nowarn}{A logical switch on warning surpression. If \code{TRUE} then \code{options(warn=-1)} is made and restored on return. This switch is to permit calls in which warnings are not desired as the user knows how to handle the returned value---say in an optimization algorithm.}
}
\value{
  \item{TRUE}{If the parameters are \code{sla} consistent.}
  \item{FALSE}{If the parameters are not \code{sla} consistent.}
}
\references{
Rogers, W.H., and Tukey, J.W., 1972, Understanding some long-tailed symmetrical distributions: Statistica Neerlandica, v. 26, no. 3, pp. 211-226.
}
\note{
This function calls \code{is.sla} to verify consistency between
the distribution parameter object and the intent of the user.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{is.sla}} }
\examples{
para <- vec2par(c(12,1.2),type='sla')
if(are.parsla.valid(para)) Q <- quasla(0.5,para)
}
\keyword{utility (distribution)}
