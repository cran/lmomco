\name{dist.list}
\alias{dist.list}
\title{ List of Distribution Names }
\description{
Return a list of the three character syntax identifying distributions supported within the \pkg{lmomco} package. The distributions are \code{aep}, \code{cau}, \code{emu}, \code{exp}, \code{gam}, \code{gev}, \code{gld}, \code{glo}, \code{gno}, \code{gpa}, \code{gum}, \code{kap}, \code{kur}, \code{lap}, \code{ln3}, \code{nor}, \code{pe3}, \code{ray}, \code{revgum}, \code{rice}, \code{texp}, \code{wak}, and \code{wei}.
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


\dontrun{

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
  if(dist[i] == 'cau' | dist[i] == 'gld' | dist[i] == 'aep' |
     dist[i] == 'emu' | dist[i] == 'kmu' | dist[i] == 'revgum' |
     dist[i] == 'kur' | dist[i] == 'rice' | dist[i] == 'texp') next
  print(lmom2par(lm1,type=dist[i]))
  print(lmom2par(lm2,type=dist[i]))
}
}

}
\keyword{distribution (utility)}
