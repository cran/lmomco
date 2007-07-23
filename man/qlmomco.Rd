\name{qlmomco}
\alias{qlmomco}
\title{Quantile Function of the Distributions}
\description{
This function acts as an alternative front end to \code{\link{par2qua}}. 
The nomenclature of the \code{qlmomco} function is to mimic that of 
built-in \pkg{R} functions.
}
\usage{
qlmomco(f,para)
}
\arguments{
  \item{f}{Nonexceedance probability (\eqn{0 \le F \le 1}).}
  \item{para}{The parameters from \code{\link{lmom2par}} or similar.}
}
\value{
  Quantile value for \eqn{F} for the specified parameters.
}
\author{ W.H. Asquith}
\seealso{\code{\link{rlmomco}}, \code{\link{rlmomco}}
}
\examples{
para <- vec2par(c(0,1),type='nor') # standard normal parameters
p75  <- qlmomco(.75,para) # 75th percentile of one standard deviation
}
\keyword{distribution}
