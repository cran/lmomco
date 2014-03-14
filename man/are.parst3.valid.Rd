\name{are.parst3.valid}
\alias{are.parst3.valid}
\title{Are the Distribution Parameters Consistent with the 3-Parameter Student T Distribution}
\description{
The distribution parameter object returned by functions of this package such as by \code{parst3} are consistent with the corresponding distribution, otherwise a list would not have been returned.
}
\usage{
are.parst3.valid(para, nowarn=FALSE)
}
\arguments{
  \item{para}{A distribution parameter list returned by \code{parst3}.}
  \item{nowarn}{A logical switch on warning surpression. If \code{TRUE} then \code{options(warn=-1)} is made and restored on return. This switch is to permit calls in which warnings are not desired as the user knows how to handle the returned value---say in an optimization algorithm.}
}
\value{
  \item{TRUE}{If the parameters are \code{st3} consistent.}
  \item{FALSE}{If the parameters are not \code{st3} consistent.}
}
\note{
This function calls \code{is.st3} to verify consistency between the distribution parameter object and the intent of the user.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{is.st3}}, \code{\link{parst3}} }
\examples{
#
}
\keyword{utility (distribution)}
