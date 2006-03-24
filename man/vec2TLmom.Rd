\name{vec2TLmom}
\alias{vec2TLmom}
\title{Convert a Vector of TL-moments to a TL-moment Object}
\description{
This function converts a vector of trimmed L-moments (TL-moments) to a TL-moment
object of this package. The object is an R \code{list}. This function is intended
to facilitate the use of TL-moments that the user might have from other sources. The
first five TL-moments are supported (\eqn{\lambda^{(t)}_1}, \eqn{\lambda^{(t)}_2}, \eqn{\lambda^{(t)}_3}, 
\eqn{\lambda_4}, \eqn{\lambda^{(t)}_5}, \eqn{\tau^{(t)}}, \eqn{\tau^{(t)}_3}, \eqn{\tau^{(t)}_4}, and \eqn{\tau^{(t)}_5}). 
Because in typical practice, the \eqn{k \ge 3} order L-moments are dimensionless ratios 
(\eqn{\tau^{(t)}_3}, \eqn{\tau^{(t)}_4}, and \eqn{\tau^{(t)}_5}), this function computes \eqn{\lambda^{(t)}_3}, 
\eqn{\lambda^{(t)}_4}, \eqn{\lambda^{(t)}_5} from \eqn{\lambda^{(t)}_2} and the ratios. However, typical 
practice is not set on the use of \eqn{\lambda^{(t)}_2} or \eqn{\tau^{(t)}} as measure of dispersion. 
Therefore, this function takes an \code{lscale} optional logical 
(\code{TRUE|FALSE}) argument---if \eqn{\lambda^{(t)}_2} is provided and \code{lscale=TRUE}, then
\eqn{\tau} is computed by the function and if \eqn{\tau} is provided, then 
\eqn{\lambda^{(t)}_2} is computed by the function. The trim level of the TL-moment is required.
}
\usage{
vec2TLmom(vec,trim,lscale)
}
\arguments{
  \item{vec}{A vector of L-moment values in \eqn{\lambda^{(t)}_1}, \eqn{\lambda^{(t)}_2} or \eqn{\tau^{(t)}}, \eqn{\tau^{(t)}_3}, \eqn{\tau^{(t)}_4}, and \eqn{\tau^{(t)}_5} order.}
  \item{trim}{The trimming level of the TL-moments (can be zero for the ordinary L-moments.)}
  \item{lscale}{A logical switch on the type of the second value of first argument.
                L-scale (\eqn{\lambda_2}) or LCV (\eqn{\tau}). Default is \code{TRUE},
                the second value in the first argument is \eqn{\lambda_2}.}
}
\value{
  An R \code{list} is returned where \eqn{t} represents the \code{trim} level.

  \item{lambdas}{Vector of the TL-moments. First element is
\eqn{\lambda^{(t)}_1}, second element is \eqn{\lambda^{(t)}_2}, and so on.}
  \item{ratios}{Vector of the L-moment ratios. Second element is 
\eqn{\tau^{(t)}}, third element is \eqn{\tau^{(t)}_3} and so on.}
  \item{trim}{Level of symmetrical trimming used in the computation. 
Default is 0---the usual L-moments are returned.}
  \item{source}{An attribute identifying the computational source of the L-moments: \dQuote{TLmoms}.}
}
\note{
It is important to note that the \dQuote{TL-moment object} returned by \code{TLmoms} is different in structure to that returned by \code{\link{TLmoms}}, and similar as the TL-moments should not be confused with the ordinary L-moments. Implementation in the package might change. At present asymmetrical TL-moment construction using this function is not available.s
}
\author{ W.H. Asquith}
\seealso{\code{\link{lmom.ub}}, \code{\link{TLmoms}}, \code{\link{vec2lmom}}}
\examples{
TL <- vec2TLmom(c(12,0.6,0.34,0.20,0.05),lscale=FALSE,trim=1)
}
\keyword{distribution}
