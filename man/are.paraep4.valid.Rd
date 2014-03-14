\name{are.paraep4.valid}
\alias{are.paraep4.valid}
\title{Are the Distribution Parameters Consistent with the 4-p Asymmetric Exponential Power Distribution}
\description{
The distribution parameter object returned by functions of this package such as by
\code{paraep4} are consistent with the corresponding distribution,
otherwise a list would not have been returned.  However, other
functions (\code{cdfaep4}, \code{quaaep4}, and
\code{lmomaep4} require consistent parameters to return the cumulative
probability (nonexceedance), quantile, and L-moments of the distribution,
respectively.  These functions internally use the \code{are.paraep4.valid}
function.
}
\usage{
are.paraep4.valid(para,nowarn=FALSE)
}
\arguments{
  \item{para}{A distribution parameter list returned by \code{paraep4}.}
  \item{nowarn}{A logical switch on warning surpression. If \code{TRUE} then \code{options(warn=-1)} is made and restored on return. This switch is to permit calls in which warnings are not desired as the user knows how to handle the returned value---say in an optimization algorithm.}
}
\value{
  \item{TRUE}{If the parameters are \code{aep4} consistent.}
  \item{FALSE}{If the parameters are not \code{aep4} consistent.}
}
\references{
Ayebo, A., and Kozubowski, T.J., 2003, An asymmetric generalization of Gaussian and Laplace
laws: Journal of Probability and Statistical Science, v. 1, no. 2, pp. 187-210.

Delicado, P., and Goria, M.N., 2008, A small sample comparison of maximum likelihood,
moments and L-moments methods for the asymmetric exponential power distribution:
Computational Statistics and Data Analysis, v. 52, no. 3, pp. 1661-1673.

Asquith, W.H., 2014, Parameter Estimation for the 4-Parameter Asymmetric Exponential Power Distribution by the Method of L-moments using R: Computational Statistics and Data Analysis, v. 71, pp. 955-970.
}
\note{
This function calls \code{is.aep4} to verify consistency between
the distribution parameter object and the intent of the user.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{is.aep4}} }
\examples{
para <- vec2par(c(0,1, 0.5, 4), type="aep4")
if(are.paraep4.valid(para)) Q <- quaaep4(0.5,para)
}
\keyword{utility (distribution)}
