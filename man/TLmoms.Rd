\name{TLmoms}
\alias{TLmoms}
\title{ The Trimmed L-moments and L-moment Ratios }
\description{
Compute the trimmed L-moments (TL-moments) for a vector. The level of symmetrical trimming is
specified. A TL-moment is 

\deqn{ \lambda^{(t)}_r = \frac{1}{r}\sum^{n-t}_{i=t+1}
           \left[ \frac{\sum^{r-1}_{k=0}{ (-1)^k C(r-1,k)
                                                 C(i-1,r+t-1-k)
                                                 C(n-i,t+k)
                                         }}{C(n,r+2t)}
           \right] x_{i:n}}

where \eqn{C(a,b)} represents the combinatorial notation (in R \code{choose}), 
\eqn{t} represents the trimming level of the \eqn{t}-largest or \eqn{t}-smallest 
values, \eqn{r} represents the order of the L-moments, \eqn{n} represents the 
sample size, and \eqn{x_{i:n}} represents the \eqn{i}th sample order statistic (\eqn{x_{1:n} \le x_{2:n} \le \dots \le x_{n:n}}). This function loops across the above equation for each \code{nmom} set in 
the argument list.
}
\usage{
TLmoms(x,nmom,trim)
}
\arguments{
  \item{x}{A vector of data values.}
  \item{nmom}{The number of moments to compute. Default is 5.}
  \item{trim}{The trimming level to use.}
}
\value{
  An R \code{list} is returned where \eqn{t} represents the \code{trim} level.

  \item{lambdas}{Vector of the TL-moments. First element is
\eqn{\lambda^{(t)}_1}, second element is \eqn{\lambda^{(t)}_2}, and so on.}
  \item{ratios}{Vector of the L-moment ratios. Second element is 
\eqn{\tau^{(t)}}, third element is \eqn{\tau^{(t)}_3} and so on.}
  \item{trim}{Level of symmetrical trimming used in the computation. 
Default is 0--the usual L-moments are returned.}
  \item{source}{An attribute identifying the computational source of the L-moments: \dQuote{TLmoms}.}
}
\note{
It is important to note that the \dQuote{L-moment object} returned by \code{TLmoms} is different in structure to that returned by \code{\link{lmom.ub}}, \code{\link{pwm2lmom}}, and similar as the TL-moments should not be confused with the ordinary L-moments. Implementation in the package might change.
}
\references{
Elamir, E.A.H., and Seheult, A.H., 2003, Trimmed L-moments: Computational statistics
and data analysis, vol. 43, pp. 299-314.
}
\author{ W.H. Asquith}

\seealso{\code{\link{TLmom}}}
\examples{
X1 <- rcauchy(30)
TL <- TLmoms(X1,nmom=6,trim=1)
}
\keyword{univar}
\keyword{distribution}
