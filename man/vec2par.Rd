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
vec2par(vec, type)
}
\arguments{
  \item{vec}{A vector of parameter values for the distribution specified by type.}
  \item{type}{Three character distribution type (for example, \code{type='gev'}).}
}
\value{
  An R \code{list} is returned.

  \item{type}{The type of distribution in three character format.}
  \item{para}{The parameters of the distribution.}
}
\author{ W.H. Asquith}
\seealso{\code{\link{lmom2par}}}
\examples{
para <- vec2par(c(12,123,0.5),'gev')
Q <- quagev(0.5,para)
}
\keyword{distribution}
