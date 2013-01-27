\name{is.wei}
\alias{is.wei}
\title{Is a Distribution Parameter Object Typed as Weibull}
\description{
The distribution parameter object returned by functions of this module such as by \code{parwei} are typed by an attribute \code{type}. This function checks that \code{type} is \code{wei} for the Weibull distribution.
}
\usage{
is.wei(para)
}
\arguments{
  \item{para}{A parameter \code{list} returned from \code{parwei}.}
}
\value{
  \item{TRUE}{If the \code{type} attribute is \code{wei}.}

  \item{FALSE}{If the \code{type} is not \code{wei}.}
}
\author{ W.H. Asquith}
\seealso{ \code{\link{parwei}} }
\examples{
para <- parwei(lmom.ub(c(123,34,4,654,37,78)))
if(is.wei(para) == TRUE) {
  Q <- quawei(0.5,para)
}
}
\keyword{utility (distribution)}
