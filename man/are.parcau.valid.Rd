\name{are.parcau.valid}
\alias{are.parcau.valid}
\title{Are the Distribution Parameters Consistent with the Cauchy Distribution}
\description{
The distribution parameter object returned by functions of this package such as by
\code{vec2par} are consistent with the corresponding distribution,
otherwise a list would not have been returned.  However, other
functions (\code{cdfcau} and \code{quacau}) require consistent parameters to return the cumulative
probability (nonexceedance), quantile, and L-moments of the distribution,
respectively.
}
\usage{
are.parcau.valid(para,nowarn=FALSE,...)
}
\arguments{
  \item{para}{A distribution parameter list returned by \code{parcau} or \code{vec2par}.}
  \item{nowarn}{A logical switch on warning surpression. If \code{TRUE} then \code{options(warn=-1)} is made and restored on return. This switch is to permit calls in which warnings are not desired as the user knows how to handle the returned value---say in an optimization algorithm.}
  \item{...}{Additional arguments for the \code{are.parCCC.valid} call that is made internally.}
}
\value{
  \item{TRUE}{If the parameters are \code{cau} consistent.}
  \item{FALSE}{If the parameters are not \code{cau} consistent.}
}
\references{
Elamir, E.A.H., and Seheult, A.H., 2003, Trimmed L-moments: Computational Statistics
and Data Analysis, vol. 43, pp. 299--314.

Gilchrist, W.G., 2000, Statistical modeling with quantile functions:
Chapman and Hall/CRC, Boca Raton, FL.
}
\note{
This function calls \code{is.cau} to verify consistency between
the distribution parameter object and the intent of the user.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{is.cau}} }
\examples{
para <- vec2par(c(12,12),type='cau')
if(are.parcau.valid(para)) Q <- quacau(0.5,para)
}
\keyword{utility (distribution)}
