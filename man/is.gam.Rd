\name{is.gam}
\alias{is.gam}
\title{Is a Distribution Parameter Object Typed as Gamma}
\description{
The distribution parameter object returned by functions of this module such as by
\code{pargam} are typed by an attribute \code{type}.
This function checks that type is \code{gam} for the Gamma distribution.
}
\usage{
is.gam(para)
}
\arguments{
  \item{para}{A parameter \code{list} returned from \code{pargam}.}
}
\value{
  \item{TRUE}{If the \code{type} attribute is \code{gam}.}
  
  \item{FALSE}{If the \code{type} is not \code{gam}.}
}
\author{ W.H. Asquith}
\seealso{ \code{\link{pargam}} }
\examples{
para <- pargam(lmom.ub(c(123,34,4,654,37,78)))
if(is.gam(para) == TRUE) {
  Q <- quagam(0.5,para)
}
}
\keyword{distribution}
