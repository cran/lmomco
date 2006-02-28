\name{are.lmom.valid}
\alias{are.lmom.valid}
\title{Are the L-moments valid}
\description{
The second through fifth order L-moments are perhaps the most common in 
analysis situations.  These L-moments have particular constraints on 
magnitudes and relation to each other.  This function 
evaluates and L-moment object whether: L-scale (\eqn{\lambda_2 > 0}), 
L-CV (\eqn{0 < \tau < 1}), (\eqn L-skew (\eqn{-1 < \tau_3 < 1}),
L-kurtosis (\eqn{0.25(5\tau_3^2 - 1) \le \tau_4 < 1}), and
\eqn{|\tau_5| < 1}.
}
\usage{
are.lmom.valid(lmom)
}
\arguments{
  \item{lmom}{A L-moment object created by \code{lmom.ub} or \code{pwm2lmom}.}
}
\value{
  \item{TRUE}{L-moments are valid.}
  
  \item{FALSE}{L-moments are not valid.}
}
\references{
Hosking, J.R.M., 1990, L-moments--Analysis and estimation of
distributions using linear combinations of order statistics: Journal
of the Royal Statistical Society, Series B, vol. 52, p. 105-124.

Hosking, J.R.M. and Wallis, J.R., 1997, Regional frequency analysis--An
approach based on L-moments: Cambridge University Press.
}
\author{ W.H. Asquith}
\seealso{\code{\link{lmom.ub}}, \code{\link{pwm2lmom}} }
\examples{
lmr <- lmom.ub(rnorm(20))
if(are.lmom.valid(lmr)) print("They are.")
}
\keyword{distribution}
