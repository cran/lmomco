\name{TLmom}
\alias{TLmom}
\title{ A Sample Trimmed L-moment }
\description{
A sample trimmed L-moment (TL-moment) is computed for a vector. The \eqn{r \ge 1} order of the L-moment is specified
as well as the level of symmetrical trimming.  A trimmed TL-moment is 

\deqn{ \hat{\lambda}^{(t)}_r = \frac{1}{r}\sum^{n-t}_{i=t+1}
           \left[ \frac{\sum\limits^{r-1}_{k=0}{ (-1)^k {r-1 \choose k}
                                                 {i-1 \choose r+t-1-k}
                                                 {n-i \choose t+k}
                                         }}{{n \choose r+2t}}
           \right] x_{i:n} \mbox{,}} 

where \eqn{t} represents the trimming level of the \eqn{t}-largest or \eqn{t}-smallest 
values, \eqn{r} represents the order of the L-moments, \eqn{n} represents the 
sample size, and \eqn{x_{i:n}} represents the \eqn{i}th sample order statistic (\eqn{x_{1:n} \le x_{2:n} \le \dots \le x_{n:n}}).
}
\usage{
TLmom(x,order,trim=NULL,leftrim=NULL,rightrim=NULL,sortdata)
}
\arguments{
  \item{x}{A vector of data values.}
  \item{order}{L-moment order to use in the computations. 
Default is 1 (the mean).}
  \item{trim}{Level of symmetrical trimming to use in the computations. 
Although \code{NULL} in the argument list, the default is 0---the usual L-moment is returned.}
  \item{leftrim}{Level of trimming of the left-tail of the sample.}
  \item{rightrim}{Level of trimming of the right-tail of the sample.}
  \item{sortdata}{A logical switch on whether the data should be sorted.
The default is TRUE.}
}
\value{
  An R \code{list} is returned.

  \item{lambda}{The TL-moment of order=\code{order} and trimming=\code{trim}.}
  \item{order}{L-moment order computed. Default is 1 (the mean).}
  \item{trim}{Level of symmetrical trimming used in the computation, which will equal \code{NULL} if asymmetrical trimming was used.}
  \item{leftrim}{Level of left-tail trimming used in the computation.}
  \item{rightrim}{Level of right-tail trimming used in the computation.}
}
\note{
The presence of the \code{sortdata} switch can be dangerous. L-moment 
computation requires that the data be sorted into the \dQuote{order statistics}.
Thus the default behavior of \code{sortdata=TRUE} is required when the
function is called on its own. In practice, this function would
not be used as multiple trimmed L-moments would be needed. Multiple trimmed
L-moments are best computed by \code{\link{TLmoms}}, which calls \code{TLmom}
multiple times. The function \code{\link{TLmoms}} takes over the
sort operation on the data and passes \code{sortdata=FALSE} to \code{TLmom}
for efficiency. (The point of this discussion is that one does not want to
waste CPU time sorting the data more than once.)
}
\references{
Elamir, E.A.H., and Seheult, A.H., 2003, Trimmed L-moments: Computational statistics
and data analysis, vol. 43, pp. 299-314.
}
\author{ W.H. Asquith}

\seealso{\code{\link{TLmoms}}}
\examples{
X1 <- rcauchy(30)
TL <- TLmom(X1,order=2,trim=1)
}
\keyword{univar}
\keyword{distribution}
