\name{are.paraep.valid}
\alias{are.paraep.valid}
\title{Are the Distribution Parameters Consistent with the Asymmetric Exponential Power Distribution}
\description{
The distribution parameter object returned by functions of this package such as by
\code{paraep} are consistent with the corresponding distribution,
otherwise a list would not have been returned.  However, other
functions (\code{cdfaep}, \code{quaaep}, and
\code{lmomaep} require consistent parameters to return the cumulative
probability (nonexceedance), quantile, and L-moments of the distribution,
respectively.  These functions internally use the \code{are.paraep.valid}
function.
}
\usage{
are.paraep.valid(para,nowarn=FALSE)
}
\arguments{
  \item{para}{A distribution parameter list returned by \code{paraep}.}
  \item{nowarn}{A logical switch on warning surpression. If \code{TRUE} then \code{options(warn=-1)} is made and restored on return. This switch is to permit calls in which warnings are not desired as the user knows how to handle the returned value---say in an optimization algorithm.}
}
\value{
  \item{TRUE}{If the parameters are \code{aep} consistent.}
  \item{FALSE}{If the parameters are not \code{aep} consistent.}
}
\references{
Ayebo, A., and Kozubowski, T.J., 2003, An asymmetric generalization of Gaussian and Laplace
laws: Journal of Probability and Statistical Science, v. 1, no. 2, pp. 187-210.

Delicado, P., and Goria, M.N., 2008, A small sample comparison of maximum likelihood,
moments and L-moments methods for the asymmetric exponential power distribution:
Computational Statistics and Data Analysis, v. 52, no. 3, pp. 1661-1673.
}
\note{
This function calls \code{is.aep} to verify consistency between
the distribution parameter object and the intent of the user.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{is.aep}} }
\examples{
para <- vec2par(c(0,1, 0.5, 4), type="aep")
if(are.paraep.valid(para)) Q <- quaaep(0.5,para)
}
\keyword{distribution}
