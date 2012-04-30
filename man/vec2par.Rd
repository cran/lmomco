\name{vec2par}
\alias{vec2par}
\title{Convert a Vector of Parameters to a Parameter Object of a Distribution}
\description{
This function converts a vector of parameters to a parameter object of a distribution. The type of distribution is specified in the
argument list: \code{cau}, \code{exp}, \code{gam}, \code{gev}, \code{glo}, \code{gno}, \code{gpa},
\code{gum}, \code{kap}, \code{kur}, \code{lap}, \code{ln3}, \code{nor}, \code{pe3}, \code{ray}, \code{revgum}, \code{rice}, \code{texp}, \code{wak}, and \code{wei}. If the distribution type is not identified, then the function issues a warning, but goes ahead and creates the parameter list and of course can not check for the validity of the parameters.
}
\usage{
vec2par(vec, type, nowarn=FALSE, paracheck=TRUE, ...)
}
\arguments{
  \item{vec}{A vector of parameter values for the distribution specified by type.}
  \item{type}{Three character distribution type (for example, \code{type='gev'}).}
  \item{nowarn}{A logical switch on warning surpression. If \code{TRUE} then \code{options(warn=-1)} is made and restored on return. This switch is to permit calls in which warnings are not desired as the user knows how to handle the returned value---say in an optimization algorithm.}
  \item{paracheck}{A logical controlling whether the parameters and checked for validity. Overriding of this check might be extremely important and needed for use of the distribution quantile function in the context of TL-moments with nonzero trimming.}
  \item{...}{Additional arguments for the \code{\link{are.par.valid}} call that is made internally.}
}
\value{
  An R \code{list} is returned. This list should contain at least the following items, but some distributions such as the \code{revgum} have extra.

  \item{type}{The type of distribution in three character format.}
  \item{para}{The parameters of the distribution.}
  \item{source}{Attribute specifying source of the parameters---\dQuote{vec2par}.}
}
\details{
If the distribution is a Reverse Gumbel (\code{type=revgum})) or Generalized Pareto (\code{type=gpa}) , which are two-parameter or three-parameter distributions, the third or fourth value in the vector is the \eqn{\zeta} of the distribution. \eqn{\zeta} represents the fraction of the sample that is noncensored, or number of observed (noncensored) values divided by the sample size. The \eqn{\zeta} represents censoring on the right, that is there are unknown observations above a threshold or the largest observed sample. Consultation of \code{\link{parrevgum}} or \code{\link{pargpaRC}} should elucidate the censoring discussion.
}
\author{ W.H. Asquith}
\seealso{\code{\link{lmom2par}}}
\examples{
para <- vec2par(c(12,123,0.5),'gev')
Q <- quagev(0.5,para)

my.custom <- vec2par(c(2,2),'myowndist') # Rice distribution
}
\keyword{distribution}
