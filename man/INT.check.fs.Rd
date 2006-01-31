\name{INT.check.fs}
\alias{INT.check.fs}
\title{ INTernal Function to Check Vector of Nonexceedance Probabilities }
\description{
This function checks that a nonexceedance probability (\eqn{F}) is in the \eqn{0 \le F \le 1} range.
It does not check that the distribution whether the function as specified by current parameters if valid
for \eqn{F = 0} or \eqn{F = 1}. End point checking 
is left to additional internal checks within the functions implementing the distribution. The function
is intended for internal use within this library to build logic flow throughout the distribution functions.
Users are not expected to need this function themselves. The \code{INT.check.fs} function is separate because 
of the heavy use of the logic across a myriad of functions in this package.
}
\usage{
INT.check.fs(fs)
}
\arguments{
  \item{fs}{A vector of nonexceedance probablity values.}
}
\value{
  \item{TRUE}{The nonexceedance probabilities are valid.}
  
  \item{FALSE}{The nonexceedance probabilities are invalid.}
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
