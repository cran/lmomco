\name{is.exp}
\alias{is.exp}
\title{Is a Distribution Parameter Object Typed as Exponential}
\description{
The distribution parameter object returned by functions of this module such as by
\code{parexp} are typed by an attribute \code{type}.
This function checks that \code{type} is \code{exp} for the Exponential distribution.
}
\usage{
is.exp(para)
}
\arguments{
  \item{para}{A parameter \code{list} returned from \code{parexp}.}
}
\value{
  \item{TRUE}{If the \code{type} attribute is \code{exp}.}

  \item{FALSE}{If the \code{type} is not \code{exp}.}
}
\author{ W.H. Asquith}
\seealso{ \code{\link{parexp}} }
\examples{
para <- parexp(lmom.ub(c(123,34,4,654,37,78)))
if(is.exp(para) == TRUE) {
  Q <- quaexp(0.5,para)
}
}
\keyword{utility (distribution)}
