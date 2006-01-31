\name{is.gpa}
\alias{is.gpa}
\title{Is a Distribution Parameter Object Typed as Generalized Pareto}
\description{
The distribution parameter object returned by functions of this module such as by
\code{pargpa} are typed by an attribute \code{type}.
This function checks that type is \code{gpa} for the Generalized Pareto distribution.
}
\usage{
is.gpa(para)
}
\arguments{
  \item{para}{A parameter \code{list} returned from \code{pargpa}.}
}
\value{
  \item{TRUE}{If the \code{type} attribute is \code{gpa}.}
  
  \item{FALSE}{If the \code{type} is not \code{gpa}.}
}
\author{ W.H. Asquith}
\seealso{ \code{\link{pargpa}} }
\examples{
para <- pargpa(lmom.ub(c(123,34,4,654,37,78)))
if(is.gpa(para) == TRUE) {
  Q <- quagpa(0.5,para)
}
}
\keyword{distribution}
