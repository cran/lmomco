\name{are.pargld.valid}
\alias{are.pargld.valid}
\title{Are the Distribution Parameters Consistent with the Generalized Lambda Distribution}
\description{
The distribution parameter object returned by functions of this package such as by
\code{vec2par} are consistent with the corresponding distribution, 
otherwise a list would not have been returned.  However, other
functions (\code{quagld}) require consistent parameters to ensure that the
Generalized Lambda Distribution is monotonic increasing on \eqn{0 \le F \le 1},
in which \eqn{F} is nonexceedance probability.
}
\usage{
are.pargld.valid(para)
}
\arguments{
  \item{para}{A distribution parameter list returned by \code{vec2par}.}
}
\value{
  \item{TRUE}{If the parameters are \code{gld} consistent.}
  
  \item{FALSE}{If the parameters are not \code{gld} consistent.}
}
\references{
Karian, Z.A., and Dudewicz, E.J., 2000, Fitting statistical distributions--
The generalized lambda distribution and generalized bootstrap methods: 
CRC Press, Boca Raton, FL, 438 p.
}
\note{
This function calls \code{is.gld} to verify consistency between 
the distribution parameter object and the intent of the user.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{is.gld}} }
\examples{
para <- vec2par(c(123,34,4,3),type='gld')
if(are.pargld.valid(para)) Q <- quagld(0.5,para)

}
\keyword{distribution}
