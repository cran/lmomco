\name{lmom.test.gpa}
\alias{lmom.test.gpa}
\title{ Test L-moment and Parameter Algorithms of the Generalized Pareto Distribution }
\description{
This function computes the L-moments of the data and the parameters of the
Generalized Pareto distribution and in turn computes the L-moments from the fitted 
parameters.
}
\usage{
lmom.test.gpa(data)
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
\seealso{ \code{\link{lmomgpa}}, \code{\link{pargpa}} }
\examples{
lmom.test.gpa(c(123,34,4,654,37,78))

lmom.test.gpa(rnorm(50))
}
\keyword{distribution}
