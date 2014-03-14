\name{are.pargov.valid}
\alias{are.pargov.valid}
\title{Are the Distribution Parameters Consistent with the Govindarajulu Distribution}
\description{
The distribution parameter object returned by functions of this package such as by
\code{pargov} are consistent with the corresponding distribution,
otherwise a list would not have been returned.  However, other
functions (\code{cdfgov}, \code{quagov}, and
\code{lmomgov} require consistent parameters to return the cumulative
probability (nonexceedance), quantile, and L-moments of the distribution,
respectively.  These functions internally use the \code{are.pargov.valid}
function.
}
\usage{
are.pargov.valid(para,nowarn=FALSE)
}
\arguments{
  \item{para}{A distribution parameter list returned by \code{pargov}.}
  \item{nowarn}{A logical switch on warning surpression. If \code{TRUE} then \code{options(warn=-1)} is made and restored on return. This switch is to permit calls in which warnings are not desired as the user knows how to handle the returned value---say in an optimization algorithm.}
}
\value{
  \item{TRUE}{If the parameters are \code{gov} consistent.}
  \item{FALSE}{If the parameters are not \code{gov} consistent.}
}
\references{
Gilchrist, W.G., 2000, Statistical modelling with quantile functions: Chapman and Hall/CRC, Boca Raton.

Nair, N.U., Sankaran, P.G., Balakrishnan, N., 2013, Quantile-based reliability analysis: Springer, New York.
}
\note{
This function calls \code{is.gov} to verify consistency between
the distribution parameter object and the intent of the user.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{is.gov}} }
\examples{
para <- pargov(lmom.ub(c(123,34,4,654,37,78)))
if(are.pargov.valid(para)) Q <- quagov(0.5,para)
}
\keyword{utility (distribution)}
