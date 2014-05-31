\name{theoTLmoms}
\alias{theoTLmoms}
\title{ The Theoretical Trimmed L-moments and TL-moment Ratios using Integration of the Quantile Function }
\description{
Compute the theoretrical trimmed L-moments (TL-moments) for a vector. The level of symmetrical or asymmetrical trimming is
specified. A theoretrical TL-moment in integral form is

\deqn{ \lambda^{(t_1,t_2)}_r = \underbrace{\frac{1}{r}}_{\stackrel{\mbox{average}}{\mbox{of terms}}}
                               \sum^{r-1}_{k=0} \overbrace{(-1)^k}^{\mbox{differences}}
          \underbrace{ r-1 \choose k }_{\mbox{combinations}}
        \frac{\overbrace{(r+t_1+t_2)!}^{\mbox{sample size}}\: I^{(t_1,t_2)}_r}
     {\underbrace{(r+t_1-k-1)!}_{\mbox{left tail}}
      \underbrace{(t_2+k)!}_{\mbox{right tail}}} \mbox{, in which }}

\deqn{ I^{(t_1,t_2)}_r = \int^1_0
                              \underbrace{x(F)}_{\stackrel{\mbox{quantile}}{\mbox{function}}} \times
                              \overbrace{F^{r+t_1-k-1}}^{\mbox{left tail}}
      \overbrace{(1-F)^{t_2+k}}^{\mbox{right tail}} \,\mathrm{d}F \mbox{,}}

where \eqn{x(F)} is the quantile function of the random variable \eqn{X} for nonexceedance probability \eqn{F}, \eqn{t_1} represents the trimming level of the \eqn{t_1}-smallest, \eqn{t_2} represents the trimming level of the \eqn{t_2}-largest values, \eqn{r} represents the order of the L-moments. This function loops across the above equation for each \code{nmom} set in the argument list. The function \eqn{x(F)} is computed through the \code{\link{par2qua}} function. The distribution type is determined using the \code{type} attribute of the \code{para} argument---the parameter object.

As of version 1.5.2 of \pkg{lmomco}, there exists enhanced error trapping on integration failures in \code{\link{theoTLmoms}}. The function now abandons operations should any of the integrations for the \eqn{r}th L-moment fail for reasons such as divergent integral or round off problems. The function returns NAs for all L-moments in \code{lambdas} and \code{ratios}.
}
\usage{
theoTLmoms(para, nmom=5, trim=NULL, leftrim=NULL,
           rightrim=NULL, verbose=FALSE, minF=0, maxF=1)
}
\arguments{
  \item{para}{A distribution parameter object of this package such as by \code{\link{vec2par}}.}
  \item{nmom}{The number of moments to compute. Default is 5.}
  \item{trim}{Level of symmetrical trimming to use in the computations.
Although \code{NULL} in the argument list, the default is 0---the usual L-moment is returned.}
  \item{leftrim}{Level of trimming of the left-tail of the sample.}
  \item{rightrim}{Level of trimming of the right-tail of the sample.}
  \item{verbose}{Toggle verbose output. Because the \R function \code{integrate} is used to perform the numerical integration, it might be useful to see selected messages regarding the numerical integration.}
  \item{minF}{The end point of nonexceedance probability in which to perform the integration. Try setting to non-zero (but small) if you have a divergent integral.}
  \item{maxF}{The end point of nonexceedance probability in which to perform the integration. Try setting to non-unity (but close) if you have a divergent integral.}
}
\value{
  An \R \code{list} is returned.

  \item{lambdas}{Vector of the TL-moments. First element is
\eqn{\lambda^{(t_1,t_2)}_1}, second element is \eqn{\lambda^{(t_1,t_2)}_2}, and so on.}
  \item{ratios}{Vector of the L-moment ratios. Second element is
\eqn{\tau^{(t_1,t_2)}}, third element is \eqn{\tau^{(t_1,t_2)}_3} and so on.}
  \item{trim}{Level of symmetrical trimming used in the computation, which will equal \code{NULL} if asymmetrical trimming was used.}
  \item{leftrim}{Level of left-tail trimming used in the computation.}
  \item{rightrim}{Level of right-tail trimming used in the computation.}
  \item{source}{An attribute identifying the computational source of the L-moments: \dQuote{theoTLmoms}.}
}
\references{
Hosking, J.R.M., 1990, L-moments--Analysis and estimation of distributions using linear combinations of order statistics: Journal of the Royal Statistical Society, Series B, v. 52, pp. 105--124.

Elamir, E.A.H., and Seheult, A.H., 2003, Trimmed L-moments: Computational Statistics and Data Analysis, v. 43, pp. 299--314.
}
\author{ W.H. Asquith}
\seealso{\code{\link{theoLmoms}}, \code{\link{TLmoms}}}
\examples{
para <- vec2par(c(0,1),type='nor') # standard normal
TL00 <- theoTLmoms(para) # compute ordinary L-moments
TL30 <- theoTLmoms(para,leftrim=3,rightrim=0) # trim 3 smallest samples

# Lets look at the difference from simulation to theoretrical using
# L-kurtosis and asymmetrical trimming for generalized Lambda dist.
P <- vec2par(c(10000,10000,6,.4),type='gld')
Lkurt <- TLmoms(quagld(runif(100),P),rightrim=3,leftrim=0)$ratios[4]
theoLkurt <- theoTLmoms(P,rightrim=3,leftrim=0)$ratios[4]
Lkurt - theoLkurt # as the number for runif goes up, this
                  # difference goes to zero

# Example using the Generalized Pareto Distribution
# to verify computations from theoretical and sample stand point.
n      <- 100 # really a much larger sample should be used---for speed
P      <- vec2par(c(12,34,4),type='gpa')
theoTL <- theoTLmoms(P,rightrim=2,leftrim=4)
samTL  <- TLmoms(quagpa(runif(n),P),rightrim=2,leftrim=4)
del    <- samTL$ratios[3] - theoTL$ratios[3] # if n is large difference
                                             # is small
str(del)
}
\keyword{L-moment (theoretical)}
\keyword{L-moment (trimmed theoretical)}
