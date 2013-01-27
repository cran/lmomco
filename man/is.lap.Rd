\name{is.lap}
\alias{is.lap}
\title{Is a Distribution Parameter Object Typed as Laplace}
\description{
The distribution parameter object returned by functions of this module such as by
\code{parlap} are typed by an attribute \code{type}.
This function checks that \code{type} is \code{lap} for the Laplace distribution.
}
\usage{
is.lap(para)
}
\arguments{
  \item{para}{A parameter \code{list} returned from \code{parlap}.}
}
\value{
  \item{TRUE}{If the \code{type} attribute is \code{lap}.}

  \item{FALSE}{If the \code{type} is not \code{lap}.}
}
\author{ W.H. Asquith}
\seealso{ \code{\link{parlap}} }
\examples{
para <- parlap(lmom.ub(c(123,34,4,654,37,78)))
if(is.lap(para) == TRUE) {
  Q <- qualap(0.5,para)
}
}
\keyword{utility (distribution)}
