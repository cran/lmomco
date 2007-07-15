\name{dist.list}
\alias{dist.list}
\title{ List of Distribution Names }
\description{
Return a list of the three character syntax identifying distributions supported within the \pkg{lmomco} package. The distributions are \code{cau}, \code{exp}, \code{gam}, \code{gev}, \code{gld}, \code{glo}, \code{gno},
\code{gpa}, \code{gum}, \code{kap}, \code{nor}, \code{pe3}, \code{revgum}, \code{wak}, and \code{wei}.
}
\usage{
dist.list()
}
\arguments{
No arguments are needed.
}
\value{
A vector of distribution identifiers.
}
\author{ W.H. Asquith}
\examples{
# Build an L-moment object
LM <- vec2lmom(c(10000,1500,0.3,0.1,0.04))
lm2 <- lmorph(LM)  # convert to vectored format
lm1 <- lmorph(lm2) # and back to named format

dist <- dist.list()

# demonstrate that lmom2par internally converts
# to needed L-moment object
for(i in seq(1,length(dist))) {
  # skip Cauchy (needs TL-moments) and GLD (speed)
  # Reverse Gumbel needs censoring . . .
  if(dist[i] == 'cau' | dist[i] == 'gld' | dist[i] == 'revgum') next
  print(lmom2par(lm1,type=dist[i]))
  print(lmom2par(lm2,type=dist[i]))
}

}
\keyword{distribution}
