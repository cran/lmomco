\name{is.revgum}
\alias{is.revgum}
\title{Is a Distribution Parameter Object Typed as Reverse Gumbel}
\description{
The distribution parameter object returned by functions of this module such as by
\code{parrevgum} are typed by an attribute \code{type}.
This function checks that \code{type} is \code{revgum} for the Reverse Gumbel distribution.
}
\usage{
is.revgum(para)
}
\arguments{
  \item{para}{A parameter \code{list} returned from \code{parrevgum}.}
}
\value{
  \item{TRUE}{If the \code{type} attribute is \code{revgum}.}

  \item{FALSE}{If the \code{type} is not \code{revgum}.}
}
\author{ W.H. Asquith}
\seealso{ \code{\link{parrevgum}} }
\examples{
para <- vec2par(c(.9252, .1636, .7),type='revgum')
if(is.revgum(para)) {
  Q <- quarevgum(0.5,para)
}
}
\keyword{utility (distribution)}
