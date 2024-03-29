\name{are.parkmu.valid}
\alias{are.parkmu.valid}
\title{Are the Distribution Parameters Consistent with the Kappa-Mu Distribution}
\description{
Is the distribution parameter object consistent with the corresponding distribution?  The distribution functions  (\code{\link{pdfkmu}}, \code{\link{cdfkmu}}, \code{\link{quakmu}}, and \code{\link{lmomkmu}}) require consistent parameters to return the cumulative probability (nonexceedance), density, quantile, and L-moments of the distribution, respectively.  These functions internally use the \code{\link{are.parkmu.valid}} function. The documentation for \code{\link{pdfkmu}} provides the conditions for valid parameters.
}
\usage{
are.parkmu.valid(para, nowarn=FALSE)
}
\arguments{
  \item{para}{A distribution parameter list returned by \code{\link{parkmu}} or \code{\link{vec2par}}.}
  \item{nowarn}{A logical switch on warning suppression. If \code{TRUE} then \code{options(warn=-1)} is made and restored on return. This switch is to permit calls in which warnings are not desired as the user knows how to handle the returned value---say in an optimization algorithm.}
}
\value{
  \item{TRUE}{If the parameters are \code{kmu} consistent.}
  \item{FALSE}{If the parameters are not \code{kmu} consistent.}
}
\note{
This function calls \code{\link{is.kmu}} to verify consistency between
the distribution parameter object and the intent of the user.
}
\author{W.H. Asquith}
\seealso{\code{\link{is.kmu}}, \code{\link{parkmu}} }
\examples{
para <- vec2par(c(0.5, 1.5), type="kmu")
if(are.parkmu.valid(para)) Q <- quakmu(0.5,para)
}
\keyword{utility (distribution)}
\keyword{Distribution: Kappa-Mu}
\keyword{utility (distribution/parameter validation)}

