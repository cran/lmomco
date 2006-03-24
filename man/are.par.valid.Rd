\name{are.par.valid}
\alias{are.par.valid}
\title{Are the Distribution Parameters Consistent with the Distribution}
\description{
This function is a dispatcher on top of the \code{are.parCCC.valid} functions, 
where \code{CCC} represents the distribution type:  \code{exp}, \code{gam}, \code{gev}, \code{glo}, \code{gno},
\code{gpa}, \code{gum}, \code{kap}, \code{nor}, \code{pe3}, or \code{wak}. Internally, this function is called
only by \code{vec2par} in the process of converting a vector into a proper
distribution parameter object for this package.
}
\usage{
are.par.valid(para,...)
}
\arguments{
  \item{para}{A distribution parameter object having at least attributes \code{type} and \code{para}.}
  \item{...}{Additional arguments for the \code{are.parCCC.valid} call that is made internally.}
}
\value{
  \item{TRUE}{If the parameters are consistent with the distribution specified by the \code{type} attribute of the parameter object.}
  \item{FALSE}{If the parameters are not consistent with the distribution specified by the \code{type} attribute of the parameter object.}
}
\references{
Hosking, J.R.M., 1996, FORTRAN routines for use with the method of L-moments:
Version 3, IBM Research Report RC20525, T.J. Watson Research Center,
Yorktown Heights, New York.

Hosking, J.R.M. and Wallis, J.R., 1997, Regional frequency analysis---An
approach based on L-moments: Cambridge University Press.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{vec2par}} }
\examples{
vec  <- c(12,120)           # parameters of exponential distribution
para <- vec2par(vec,'exp')  # build exponential distribution parameter object
# The following two conditionals are equivalent as are.parexp.valid() is called
# within are.par.valid().
if(are.par.valid(para))    Q <- quaexp(0.5,para)
if(are.parexp.valid(para)) Q <- quaexp(0.5,para)

}
\keyword{distribution}
