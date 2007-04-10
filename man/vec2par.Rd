\name{vec2par}
\alias{vec2par}
\title{Convert a Vector of Parameters to a Parameter Object of a Distribution}
\description{
This function converts the L-moments of the data to the parameters of a
distribution. The type of distribution is specified in the 
argument list: \code{cau}, \code{exp}, \code{gam}, \code{gev}, \code{glo}, \code{gno}, \code{gpa},
\code{gum}, \code{kap}, \code{nor}, \code{pe3}, and \code{wak}.
}
\usage{
vec2par(vec, type, nowarn=FALSE, ...)
}
\arguments{
  \item{vec}{A vector of parameter values for the distribution specified by type.}
  \item{type}{Three character distribution type (for example, \code{type='gev'}).}
  \item{nowarn}{A logical switch on warning surpression. If \code{TRUE} then \code{options(warn=-1)} is made and restored on return. This switch is to permit calls in which warnings are not desired as the user knows how to handle the returned value---say in an optimization algorithm.}
  \item{...}{Additional arguments for the \code{\link{are.par.valid}} call that is made internally.}
}
\value{
  An R \code{list} is returned.

  \item{type}{The type of distribution in three character format.}
  \item{para}{The parameters of the distribution.}
  \item{source}{Attribution specifying source of the parametes---\dQuote{vec2par}.}
}
\author{ W.H. Asquith}
\seealso{\code{\link{lmom2par}}}
\examples{
para <- vec2par(c(12,123,0.5),'gev')
Q <- quagev(0.5,para)
}
\keyword{distribution}
