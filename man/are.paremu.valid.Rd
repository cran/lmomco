\name{are.paremu.valid}
\alias{are.paremu.valid}
\title{Are the Distribution Parameters Consistent with the Eta-Mu Distribution}
\description{
The distribution parameter object returned by functions of this package such as by \code{paremu} are consistent with the corresponding distribution, otherwise a list would not have been returned.  However, other functions (\code{\link{pdfemu}}, \code{\link{cdfemu}}, \code{\link{quaemu}}, and \code{\link{lmomemu}} require consistent parameters to return the cumulative probability (nonexceedance), quantile, and L-moments of the distribution, respectively.  These functions internally use the \code{are.paremu.valid} function. The documentation \code{\link{pdfemu}} provides the conditions on valid parameters and their interrelation.
}
\usage{
are.paremu.valid(para, nowarn=FALSE)
}
\arguments{
  \item{para}{A distribution parameter list returned by \code{paremu}.}
  \item{nowarn}{A logical switch on warning surpression. If \code{TRUE} then \code{options(warn=-1)} is made and restored on return. This switch is to permit calls in which warnings are not desired as the user knows how to handle the returned value---say in an optimization algorithm.}
}
\value{
  \item{TRUE}{If the parameters are \code{emu} consistent.}
  \item{FALSE}{If the parameters are not \code{emu} consistent.}
}
\note{
This function calls \code{is.emu} to verify consistency between
the distribution parameter object and the intent of the user.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{is.emu}} }
\examples{
para <- vec2par(c(0.4, .04), type="emu")
if(are.paremu.valid(para)) Q <- quaemu(0.5,para)
}
\keyword{utility (distribution)}
