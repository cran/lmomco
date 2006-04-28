\name{rlmomco}
\alias{rlmomco}
\title{Random Deviates of a Distribution}
\description{
This function generates random deviates for the specified distribution in the parameter object argument. See documentation about \code{para} object in \code{\link{lmom2par}}. The prepended \code{r} in the function name is to parallel the built-in distribution syntax of R.
}
\usage{
rlmomco(n,para)
}
\arguments{
  \item{n}{Number of sample to generate}
  \item{para}{The parameters from \code{\link{lmom2par}} or similar.}
}
\value{
  Vector of quantile values.
}
\author{ W.H. Asquith}
\seealso{\code{\link{lmom2par}}}
\examples{
lmr      <- lmom.ub(rnorm(20))
para     <- parnor(lmr)
simulate <- rlmomco(20,para)
}
\keyword{distribution}
