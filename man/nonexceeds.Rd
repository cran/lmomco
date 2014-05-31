\name{nonexceeds}
\alias{nonexceeds}
\title{ Some Common or Useful Nonexceedance Probabilities }
\description{
This function returns a vector nonexceedance probabilities.
}
\usage{
nonexceeds(f01=FALSE, less=FALSE)
}
\arguments{
  \item{f01}{A logical and if \code{TRUE} then 0 and 1 are included in the returned vector.}
  \item{less}{A logical and if \code{TRUE} the default values are trimmed back.}
}
\value{
  A vector of selected nonexceedance probabilities \eqn{F} useful in assessing the \dQuote{frequency curve} in hydrologic applications (noninclusive). This vector is intended to be helpful and self-documenting when common \eqn{F} values are desired to explore deep into both distribution tails.
}
\author{ W.H. Asquith }
\seealso{ \code{\link{check.fs}}, \code{\link{prob2T}}, \code{\link{T2prob}} }
\examples{
lmr <- lmoms(rnorm(20))
para <- parnor(lmr)
quanor(nonexceeds(), para)
}
\keyword{misc}
\keyword{utility (probability)}

