\name{is.gev}
\alias{is.gev}
\title{Is a Distribution Parameter Object Typed as Generalized Extreme Value}
\description{
The distribution parameter object returned by functions of this module such as by
\code{pargev} are typed by an attribute \code{type}.
This function checks that \code{type} is \code{gev} for the Generalized Extreme Value distribution.
}
\usage{
is.gev(para)
}
\arguments{
  \item{para}{A parameter \code{list} returned from \code{pargev}.}
}
\value{
  \item{TRUE}{If the \code{type} attribute is \code{gev}.}

  \item{FALSE}{If the \code{type} is not \code{gev}.}
}
\author{ W.H. Asquith}
\seealso{ \code{\link{pargev}} }
\examples{
para <- pargev(lmom.ub(c(123,34,4,654,37,78)))
if(is.gev(para) == TRUE) {
  Q <- quagev(0.5,para)
}
}
\keyword{utility (distribution)}
