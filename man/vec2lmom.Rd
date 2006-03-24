\name{vec2lmom}
\alias{vec2lmom}
\title{Convert a Vector of L-moments to a L-moment Object}
\description{
This function converts a vector of L-moments to a L-moment
object of this package. The object is an R \code{list}. This function is intended
to facilitate the use of L-moments that the user might have from other sources. The
first five L-moments are supported (\eqn{\lambda_1}, \eqn{\lambda_2}, \eqn{\lambda_3}, 
\eqn{\lambda_4}, \eqn{\lambda_5}, \eqn{\tau}, \eqn{\tau_3}, \eqn{\tau_4}, and \eqn{\tau_5}). 
Because in typical practice, the \eqn{k \ge 3} order L-moments are dimensionless ratios 
(\eqn{\tau_3}, \eqn{\tau_4}, and \eqn{\tau_5}), this function computes \eqn{\lambda_3}, 
\eqn{\lambda_4}, \eqn{\lambda_5} from \eqn{\lambda_2} and the ratios. However, typical 
practice is not set on the use of \eqn{\lambda_2} or \eqn{\tau} as measure of dispersion. 
Therefore, this function takes an \code{lscale} optional logical 
(\code{TRUE|FALSE}) argument--if \eqn{\lambda_2} is provided and \code{lscale=TRUE}, then
\eqn{\tau} is computed by the function and if \eqn{\tau} is provided, then 
\eqn{\lambda_2} is computed by the function.
}
\usage{
vec2lmom(vec,lscale)
}
\arguments{
  \item{vec}{A vector of L-moment values in \eqn{\lambda_1}, \eqn{\lambda_2} or \eqn{\tau}, 
\eqn{\tau_3}, \eqn{\tau_4}, and \eqn{\tau_5} order.}
  \item{lscale}{A logical switch on the type of the second value of first argument.
                L-scale (\eqn{\lambda_2}) or LCV (\eqn{\tau}). Default is \code{TRUE},
                the second value in the first argument is \eqn{\lambda_2}.}
}
\value{
  An R \code{list} is returned.

  \item{L1}{Arithmetic mean.}
  \item{L2}{L-scale---analogous to standard deviation.}
  \item{LCV}{coefficient of L-variation---analogous to coe. of variation.}
  \item{TAU3}{The third L-moment ratio or L-skew---analogous to skew.}
  \item{TAU4}{The fourth L-moment ratio or L-kurtosis---analogous to kurtosis.}
  \item{TAU5}{The fifth L-moment ratio.}
  \item{L3}{The third L-moment.}
  \item{L4}{The fourth L-moment.}
  \item{L5}{The fifth L-moment.}
}
\author{ W.H. Asquith}
\seealso{\code{\link{lmom.ub}}, \code{\link{vec2pwm}}}
\examples{
lmr <- vec2lmom(c(12,0.6,0.34,0.20,0.05),lscale=FALSE)
}
\keyword{distribution}
