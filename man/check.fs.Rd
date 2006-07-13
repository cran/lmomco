\name{check.fs}
\alias{check.fs}
\title{ Check Vector of Nonexceedance Probabilities }
\description{
This function checks that a nonexceedance probability (\eqn{F}) is in the \eqn{0 \le F \le 1} range.
It does not check that the distribution whether the function as specified by current parameters if valid
for \eqn{F = 0} or \eqn{F = 1}. End point checking 
is left to additional internal checks within the functions implementing the distribution. The function
is intended for internal use within this library to build logic flow throughout the distribution functions.
Users are not expected to need this function themselves. The \code{check.fs} function is separate because 
of the heavy use of the logic across a myriad of functions in this package.
}
\usage{
check.fs(fs)
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
\code{\link{quacau}},
\code{\link{quaexp}}, 
\code{\link{quagam}}, 
\code{\link{quagev}},
\code{\link{quagld}},  
\code{\link{quaglo}}, \cr
\code{\link{quagno}}, 
\code{\link{quagpa}},
\code{\link{quagum}}, 
\code{\link{quakap}}, 
\code{\link{quanor}}, 
\code{\link{quape3}}, \cr
\code{\link{quawak}}, and
\code{\link{quawei}}
}
\examples{
F <- c(0.5,0.7,0.9,1.1)
if(check.fs(F) == FALSE) cat("Bad nonexceedances 0<F<1\n")
}
\keyword{distribution}
