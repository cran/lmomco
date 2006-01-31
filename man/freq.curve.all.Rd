\name{freq.curve.all}
\alias{freq.curve.all}
\title{ Compute Frequency Curve for All Distributions}
\description{
This function is dispatcher on top of the suite of \code{freq.curve.CCC}
functions that compute frequency curves for the L-moments. Frequency curves in hydrologic science is a term typically renaming the more conventional quantile function. The notation
\code{CCC} represents the three character notation for the distribution: 
\code{exp}, \code{gam}, \code{gev}, \code{glo}, \code{gno}, \code{gpa},
\code{gum}, \code{kap}, \code{nor}, \code{pe3}, and \code{wak}.}
\usage{
freq.curve.all(lmom)
}
\arguments{
  \item{lmom}{A L-moment object from \code{lmom.ub} or similar.}
}
\value{
  An extensive R \code{list} of frequency curves.
}
\author{ W.H. Asquith}
\seealso{
\code{\link{freq.curve.exp}},
\code{\link{freq.curve.gam}},
\code{\link{freq.curve.gev}},
\code{\link{freq.curve.glo}},
\code{\link{freq.curve.gno}},
\code{\link{freq.curve.gpa}},
\code{\link{freq.curve.gum}},
\code{\link{freq.curve.kap}},
\code{\link{freq.curve.nor}},
\code{\link{freq.curve.pe3}}, and
\code{\link{freq.curve.wak}}

}
\keyword{distribution}
