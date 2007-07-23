\name{TLmoms}
\alias{TLmoms}
\title{ The Sample Trimmed L-moments and L-moment Ratios }
\description{
Compute the sample trimmed L-moments (TL-moments) for a vector. The level of symmetrical trimming is
specified. A TL-moment is 

\deqn{ \hat{\lambda}^{(t_1,t_2)}_r = \frac{1}{r}\sum^{n-t_2}_{i=t_1+1}
           \left[ \frac{\sum\limits^{r-1}_{k=0}{ (-1)^k {r-1 \choose k}
                                                 {i-1 \choose r+t_1-1-k}
                                                 {n-i \choose t_2+k}
                                         }}{{n \choose r+t_1+t_2}}
           \right] x_{i:n} \mbox{,}}

where \eqn{t} represents the trimming level of the \eqn{t}-largest or \eqn{t}-smallest 
values, \eqn{r} represents the order of the L-moments, \eqn{n} represents the 
sample size, and \eqn{x_{i:n}} represents the \eqn{i}th sample order statistic (\eqn{x_{1:n} \le x_{2:n} \le \dots \le x_{n:n}}). This function loops across the above equation for each \code{nmom} set in 
the argument list.
}
\usage{
TLmoms(x,nmom,trim=NULL,leftrim=NULL,rightrim=NULL)
}
\arguments{
  \item{x}{A vector of data values.}
  \item{nmom}{The number of moments to compute. Default is 5.}
  \item{trim}{Level of symmetrical trimming to use in the computations, which will equal \code{NULL} if asymmetrical trimming was used. Although \code{NULL} in the argument list, the default is 0---the usual L-moment is returned.}
  \item{leftrim}{Level of trimming of the left-tail of the sample.}
  \item{rightrim}{Level of trimming of the right-tail of the sample.}
}
\value{
  An R \code{list} is returned.

  \item{lambdas}{Vector of the TL-moments. First element is
\eqn{\hat{\lambda}^{(t_1,t_2)}_1}, second element is \eqn{\hat{\lambda}^{(t_1,t_2)}_2}, and so on.}
  \item{ratios}{Vector of the L-moment ratios. Second element is 
\eqn{\hat{\tau}^{(t_1,t_2)}}, third element is \eqn{\hat{\tau}^{(t_1,t_2)}_3} and so on.}
  \item{trim}{Level of symmetrical trimming used in the computation, which will equal \code{NULL} if asymmetrical trimming was used.}
  \item{leftrim}{Level of left-tail trimming used in the computation.}
  \item{rightrim}{Level of right-tail trimming used in the computation.}
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

\seealso{\code{\link{TLmom}}, \code{\link{lmoms}}, and \code{\link{lmorph}}}
\examples{
X1 <- rcauchy(30)
TL <- TLmoms(X1,nmom=6,trim=1)
}
\keyword{univar}
\keyword{distribution}
