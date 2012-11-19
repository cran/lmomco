\name{is.ln3}
\alias{is.ln3}
\title{Is a Distribution Parameter Object Typed as 3-Parameter Log-Normal}
\description{
The distribution parameter object returned by functions of this module such as by
\code{parln3} are typed by an attribute \code{type}.
This function checks that \code{type} is \code{ln3} for the 3-parameter log-Normal distribution.
}
\usage{
is.ln3(para)
}
\arguments{
  \item{para}{A parameter \code{list} returned from \code{parln3}.}
}
\value{
  \item{TRUE}{If the \code{type} attribute is \code{ln3}.}
  
  \item{FALSE}{If the \code{type} is not \code{ln3}.}
}
\author{ W.H. Asquith}
\seealso{ \code{\link{parln3}} }
\examples{
para <- vec2par(c(.9252, .1636, .7),type='ln3')
if(is.ln3(para)) {
  Q <- qualn3(0.5,para)
}
}
\keyword{distribution}
