\name{par2lmom}
\alias{par2lmom}
\title{Convert the Parameters of a Distribution to the L-moments}
\description{
This function converts the parameters of a distribution to the L-moment as
represented in an L-moment object. This function dispatches to \code{lmomCCC}
where CCC represents the three character distribution identifier: \code{exp}, 
\code{gam}, \code{gev}, \code{glo}, \code{gno}, \code{gpa},
\code{gum}, \code{kap}, \code{nor}, \code{pe3}, and \code{wak}.
}
\usage{
par2lmom(para)
}
\arguments{
  \item{para}{A parameter object of a distribution.}
}
\value{
  An L-moment object (an R \code{list}) is returned.
}
\author{ W.H. Asquith}
\seealso{\code{\link{lmom.ub}}, \code{\link{lmom2par}}}
\examples{
lmr      <- lmom.ub(rnorm(20))
para     <- parnor(lmr)
frompara <- par2lmom(para)
lmom.diff(frompara,lmr)
}
\keyword{distribution}
