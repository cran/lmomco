\name{is.glo}
\alias{is.glo}
\title{Is a Distribution Parameter Object Typed as Generalized Logistic}
\description{
The distribution parameter object returned by functions of this module such as by
\code{parglo} are typed by an attribute \code{type}.
This function checks that type is \code{glo} for the Generalized Logistic distribution.
}
\usage{
is.glo(para)
}
\arguments{
  \item{para}{A parameter \code{list} returned from \code{parglo}.}
}
\value{
  \item{TRUE}{If the \code{type} attribute is \code{glo}.}
  
  \item{FALSE}{If the \code{type} is not \code{glo}.}
}
\author{ W.H. Asquith}
\seealso{ \code{\link{parglo}} }
\examples{
para <- parglo(lmom.ub(c(123,34,4,654,37,78)))
if(is.glo(para) == TRUE) {
  Q <- quaglo(0.5,para)
}
}
\keyword{distribution}
