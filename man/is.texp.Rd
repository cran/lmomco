\name{is.texp}
\alias{is.texp}
\title{Is a Distribution Parameter Object Typed as Truncated Exponential}
\description{
The distribution parameter object returned by functions of this module such as by
\code{partexp} are typed by an attribute \code{type}. This function checks that \code{type} is \code{texp} for the Truncated Exponential distribution.
}
\usage{
is.texp(para)
}
\arguments{
  \item{para}{A parameter \code{list} returned from \code{partexp}.}
}
\value{
  \item{TRUE}{If the \code{type} attribute is \code{texp}.}

  \item{FALSE}{If the \code{type} is not \code{texp}.}
}
\author{ W.H. Asquith}
\seealso{ \code{\link{partexp}} }
\examples{
#
}
\keyword{distribution}
