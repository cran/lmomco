\name{is.emu}
\alias{is.emu}
\title{Is a Distribution Parameter Object Typed as Eta-Mu}
\description{
The distribution parameter object returned by functions of this module such as by
\code{paremu} are typed by an attribute \code{type}. This function checks that \code{type} is \code{emu} for the Eta-Mu (\eqn{\eta:\mu}) distribution.
}
\usage{
is.emu(para)
}
\arguments{
  \item{para}{A parameter \code{list} returned from \code{paremu}.}
}
\value{
  \item{TRUE}{If the \code{type} attribute is \code{emu}.}

  \item{FALSE}{If the \code{type} is not \code{emu}.}
}
\author{ W.H. Asquith}
\seealso{ \code{\link{paremu}} }
\examples{
para <- vec2par(c(0.25, 1.4), type='emu')
if(is.emu(para)) {
  Q <- quaemu(0.5,para)
}
}
\keyword{distribution}
