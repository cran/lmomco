\name{lmom.test.gno}
\alias{lmom.test.gno}
\title{ Test L-moment and Parameter Algorithms of the Generalized Normal Distribution }
\description{
This function computes the L-moments of the data and the parameters of the
Generalized Normal distribution and in turn computes the L-moments from the fitted 
parameters.
}
\usage{
lmom.test.gno(data)
}
\arguments{
  \item{data}{A vector of data.}
}
\value{
  Comparison of the median of the distribution and reverse computation
  of the median from the 0.5 nonexceedance probability.

  Output from \code{\link{lmom.diff}}.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{lmomgno}}, \code{\link{pargno}} }
\examples{
lmom.test.gno(c(123,34,4,654,37,78))

lmom.test.gno(rnorm(50))
}
\keyword{distribution}
