\name{is.nor}
\alias{is.nor}
\title{Is a Distribution Parameter Object Typed as Normal}
\description{
The distribution parameter object returned by functions of this module such as by
\code{parnor} are typed by an attribute \code{type}.
This function checks that type is \code{nor} for the Normal distribution.
}
\usage{
is.nor(para)
}
\arguments{
  \item{para}{A parameter \code{list} returned from \code{parnor}.}
}
\value{
  \item{TRUE}{If the \code{type} attribute is \code{nor}.}
  
  \item{FALSE}{If the \code{type} is not \code{nor}.}
}
\author{ W.H. Asquith}
\seealso{ \code{\link{parnor}} }
\examples{
para <- parnor(lmom.ub(c(123,34,4,654,37,78)))
if(is.nor(para) == TRUE) {
  Q <- quanor(0.5,para)
}
}
\keyword{distribution}
