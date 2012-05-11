\name{is.aep4}
\alias{is.aep4}
\title{Is a Distribution Parameter Object Typed as 4-p Asymmetric Exponential Power}
\description{
The distribution parameter object returned by functions of this module such as by
\code{paraep4} are typed by an attribute \code{type}.
This function checks that \code{type} is \code{aep4} for the 4-parameter Asymmetric Exponential Power distribution.
}
\usage{
is.aep4(para)
}
\arguments{
  \item{para}{A parameter \code{list} returned from \code{paraep4}.}
}
\value{
  \item{TRUE}{If the \code{type} attribute is \code{aep4}.}

  \item{FALSE}{If the \code{type} is not \code{aep4}.}
}
\author{ W.H. Asquith}
\seealso{ \code{\link{paraep4}} }
\examples{
para <- vec2par(c(0,1, 0.5, 4), type="aep4")
if(is.aep4(para) == TRUE) {
  Q <- quaaep4(0.55,para)
}
}
\keyword{distribution}
