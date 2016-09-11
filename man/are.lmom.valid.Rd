\name{are.lmom.valid}
\alias{are.lmom.valid}
\title{Are the L-moments valid}
\description{
The L-moments have particular constraints on magnitudes and relation to each other.  This function evaluates and L-moment object whether the bounds for L-scale (\eqn{\lambda_2 > 0}), L-skew (\eqn{|\tau_3| < 1)}, L-kurtosis (\eqn{0.25(5\tau_3^2 - 1) \le \tau_4 < 1}), and \eqn{|\tau_5| < 1} are satisfied. The TL-moments as implemented by the \code{TL} functions in \pkg{lmomco} are not applicable to the boundaries consulted within this function.
}
\usage{
are.lmom.valid(lmom)
}
\arguments{
  \item{lmom}{An L-moment object created by \code{\link{lmoms}}, \code{\link{lmom.ub}}, \code{\link{pwm2lmom}}.}
}
\value{
  \item{TRUE}{L-moments are valid.}

  \item{FALSE}{L-moments are not valid.}
}
\references{
Hosking, J.R.M., 1990, L-moments---Analysis and estimation of distributions using linear combinations of order statistics: Journal of the Royal Statistical Society, Series B, v. 52, pp. 105--124.

Hosking, J.R.M. and Wallis, J.R., 1997, Regional frequency analysis---An approach based on L-moments: Cambridge University Press.
}
\author{ W.H. Asquith}
\seealso{\code{\link{lmom.ub}}, \code{\link{lmoms}}, \code{\link{pwm2lmom}} }
\examples{
lmr <- lmoms(rnorm(20))
if(are.lmom.valid(lmr)) print("They are.")
\dontrun{
X <- c(1.7106278,  1.7598761,  1.2111335,  0.3447490,  1.8312889,
       1.3938445, -0.5376054, -0.2341009, -0.4333601, -0.2545229)
are.lmom.valid(lmoms(X))
are.lmom.valid(pwm2lmom(pwm.pp(X, a=0.5)))
}
}
\keyword{L-moment (theoretical)}
\keyword{L-moment (validation)}

