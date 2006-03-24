\name{par2qua}
\alias{par2qua}
\title{Quantile Function of the Distributions}
\description{
This function acts as a front end or dispatcher to the distribution-specific
quantile functions. 
}
\usage{
par2qua(f,para,...)
}
\arguments{
  \item{f}{Nonexceedance probability (\eqn{0 \le F \le 1}).}
  \item{para}{The parameters from \code{\link{lmom2par}} or similar.}
  \item{...}{The additional arguments are passed to the quantile function such as \code{paracheck=FALSE} for the generalized Lambda distribution (\code{\link{quagld}}).}
}
\value{
  Quantile value for \eqn{F}.
}
\author{ W.H. Asquith}
\seealso{\code{\link{par2cdf}}, \code{\link{lmom2par}}}
\examples{
lmr     <- lmom.ub(rnorm(20))
para    <- parnor(lmr)
median  <- par2qua(0.5,para)
}
\keyword{distribution}
