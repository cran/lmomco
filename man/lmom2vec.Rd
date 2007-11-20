\name{lmom2vec}
\alias{lmom2vec}
\title{ Convert an L-moment object to a Vector of L-moments}
\description{
This function converts an L-moment object in the structure used by \pkg{lmomco} into a simple vector of \eqn{\lambda_1}, \eqn{\lambda_2}, \eqn{\tau_3}, \eqn{\tau_4}, \eqn{\tau_5}.
}
\usage{
lmom2vec(lmom, ...)
}
\arguments{
  \item{lmom}{L-moment object as from functions such as \code{lmoms}, \code{lmom.ub}, and \code{vec2lmom}.}
  \item{...}{Not presently used.}
}
\value{
A vector of the first five L-moments (\eqn{\lambda_1}, \eqn{\lambda_2}, \eqn{\tau_3}, \eqn{\tau_4}, \eqn{\tau_5}) or mean, L-scale, L-skew, L-kurtosis, and Tau5.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{lmom.ub}}, \code{\link{lmoms}}, \code{\link{lmorph}}, and \code{\link{vec2lmom}}
}
\examples{
  lmr <- lmom.ub(rnorm(40))
  lmom2vec(lmr)


  lmr <- vec2lmom(c(140,150,.3,.2,-.1))
  lmom2vec(lmr)
}
\keyword{distribution}
