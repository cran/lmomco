\name{is.wak}
\alias{is.wak}
\title{Is a Distribution Parameter Object Typed as Wakeby}
\description{
The distribution parameter object returned by functions of this module such as by
\code{parwak} are typed by an attribute \code{type}.
This function checks that \code{type} is \code{wak} for the Wakeby distribution.
}
\usage{
is.wak(para)
}
\arguments{
  \item{para}{A parameter \code{list} returned from \code{parwak}.}
}
\value{
  \item{TRUE}{If the \code{type} attribute is \code{wak}.}

  \item{FALSE}{If the \code{type} is not \code{wak}.}
}
\author{ W.H. Asquith}
\seealso{ \code{\link{parwak}} }
\examples{
para <- parwak(lmom.ub(c(123,34,4,654,37,78)))
if(is.wak(para) == TRUE) {
  Q <- quawak(0.5,para)
}
}
\keyword{utility (distribution)}
