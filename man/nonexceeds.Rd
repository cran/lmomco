\name{nonexceeds}
\alias{nonexceeds}
\title{ Common Nonexceedance Probabilities }
\description{
This function returns a vector nonexceedance probabilities.
}
\usage{
nonexceeds()
}
\value{
  A vector of selected nonexceedance probabilities useful in assessing the \dQuote{frequency
  curve} in hydrologic applications (noninclusive). The nonexceedance probabilities
  extend further into the right-hand tail of the \dQuote{distribution} to the 0.996 and 0.998
  nonexceedance probability values as these are equivalent to the 250- and 500-year events
  respectively. The relation between annual recurrence interval and nonexceedance
  probability (when annual data are analyzed) is

  \deqn{ F = 1 - \frac{1}{T} \mbox{,}}

where T is the \eqn{T}-year event.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{quaexp}}, \code{\link{quagam}}, \code{\link{quagev}},
\code{\link{quaglo}}, \code{\link{quagno}}, \code{\link{quagpa}},
\code{\link{quagum}}, \code{\link{quakap}}, \code{\link{quanor}},
\code{\link{quape3}}, \code{\link{quawak}}, and \code{\link{check.fs}} }
\examples{
lmr <- lmom.ub(rnorm(20))
para <- parnor(lmr)
quanor(nonexceeds(),para)
}
\keyword{distribution}
