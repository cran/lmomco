\name{lmom2par}
\alias{lmom2par}
\title{Convert L-moments to the the Parameters of a Distribution}
\description{
This function converts the L-moments of the data to the parameters of a
distribution. The type of distribution is specified in the 
argument list: \code{exp}, \code{gam}, \code{gev}, \code{glo}, \code{gno},
\code{gpa}, \code{gum}, \code{kap}, \code{nor}, \code{pe3}, or \code{wak}. There
is no Cauchy distribution support by the \code{lmom2par} function because Cauchy uses TL-moments and not the ordinary L-moments. For Cauchy functions such as \code{\link{parcau}} and \code{\link{lmomcau}} should be used in accordance with their documentation. 
}
\usage{
lmom2par(lmom, type, ...)
}
\arguments{
  \item{lmom}{An L-moment object such as that returned by \code{lmom.ub} or \code{pwm2lmom}.}
  \item{type}{Three character distribution type (for example, \code{type='gev'}).}
  \item{\ldots}{Additional arguments for the \code{parCCC} functions.}
}
\value{
  An R \code{list} is returned.

  \item{type}{The type of distribution in three character format.}
  \item{para}{The parameters of the distribution.}
}
\references{
Hosking, J.R.M., 1990, L-moments--Analysis and estimation of
distributions using linear combinations of order statistics: Journal
of the Royal Statistical Society, Series B, vol. 52, p. 105-124.

Hosking, J.R.M., 1996, FORTRAN routines for use with the method of L-moments:
Version 3, IBM Research Report RC20525, T.J. Watson Research Center,
Yorktown Heights, New York.

Hosking, J.R.M. and Wallis, J.R., 1997, Regional frequency analysis--An
approach based on L-moments: Cambridge University Press.
}
\author{ W.H. Asquith}
\seealso{\code{\link{lmom2par}}}
\examples{
lmr      <- lmom.ub(rnorm(20))
para     <- lmom2par(lmr,type='nor')
frompara <- par2lmom(para)
lmom.diff(frompara,lmr)
}
\keyword{distribution}
