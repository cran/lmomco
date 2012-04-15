\name{is.aep}
\alias{is.aep}
\title{Is a Distribution Parameter Object Typed as Asymmetric Exponential Power}
\description{
The distribution parameter object returned by functions of this module such as by
\code{paraep} are typed by an attribute \code{type}.
This function checks that \code{type} is \code{aep} for the Asymmetric Exponential Power distribution.
}
\usage{
is.aep(para)
}
\arguments{
  \item{para}{A parameter \code{list} returned from \code{paraep}.}
}
\value{
  \item{TRUE}{If the \code{type} attribute is \code{aep}.}

  \item{FALSE}{If the \code{type} is not \code{aep}.}
}
\author{ W.H. Asquith}
\seealso{ \code{\link{paraep}} }
\examples{
para <- vec2par(c(0,1, 0.5, 4), type="aep")
if(is.aep(para) == TRUE) {
  Q <- quaaep(0.55,para)
}
}
\keyword{distribution}
