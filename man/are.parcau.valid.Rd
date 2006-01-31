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
are.parcau.valid(para)
}
\arguments{
  \item{para}{A distribution parameter list returned by \code{vec2par}.}
}
\value{
  \item{TRUE}{If the parameters are \code{cau} consistent.}
  
  \item{FALSE}{If the parameters are not \code{cau} consistent.}
}
\references{
Gilchirst, W.G., 2000, Statistical modeling with quantile functions: 
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
\keyword{distribution}
