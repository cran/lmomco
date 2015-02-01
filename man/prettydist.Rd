\name{prettydist}
\alias{prettydist}
\title{ A Pretty List of Distribution Names }
\description{
Return a full name of one or more distributions from the abbreviation for the distribution. The official list of abbreviations is seen under \code{\link{dist.list}}.
}
\usage{
prettydist(x)
}
\arguments{
\item{x}{A vector of \pkg{lmomco} distribution abbreviations.}
}
\value{
A vector of distribution identifiers.
}
\author{ W.H. Asquith}
\examples{
the.lst <- dist.list() # the authoritative list of abbreviations
prettydist(the.lst)
}
\keyword{utility (distribution)}
