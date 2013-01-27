\name{is.gum}
\alias{is.gum}
\title{Is a Distribution Parameter Object Typed as Gumbel}
\description{
The distribution parameter object returned by functions of this module such as by
\code{pargum} are typed by an attribute \code{type}.
This function checks that \code{type} is \code{gum} for the Gumbel distribution.
}
\usage{
is.gum(para)
}
\arguments{
  \item{para}{A parameter \code{list} returned from \code{pargum}.}
}
\value{
  \item{TRUE}{If the \code{type} attribute is \code{gum}.}

  \item{FALSE}{If the \code{type} is not \code{gum}.}
}
\author{ W.H. Asquith}
\seealso{ \code{\link{pargum}} }
\examples{
para <- pargum(lmom.ub(c(123,34,4,654,37,78)))
if(is.gum(para) == TRUE) {
  Q <- quagum(0.5,para)
}
}
\keyword{utility (distribution)}
