\name{lmomTLgld}
\alias{lmomTLgld}
\title{Trimmed L-moments (t=1) of the Generalized Lambda Distribution}
\description{
This function estimates the trimmed L-moments (TL-moments) for \eqn{t=1} of the Generalized Lambda distribution given the parameters (\eqn{\xi}, \eqn{\alpha}, \eqn{\kappa}, and \eqn{h}) from \code{\link{vec2par}}. The TL-moments in terms of the parameters are complicated; however, there are analytical solutions. There are no simple expressions of the parameters in terms of the L-moments. The first four TL-moments of the distribution are

\deqn{\lambda^{(1)}_1 = \xi + 6\alpha
                              \left(\frac{1}{(\kappa+3)(\kappa+2)} - 
			            \frac{1}{(h+3)(h+2)} \right) \mbox{,}}    

\deqn{\lambda^{(1)}_2 = 6\alpha \left(\frac{\kappa}{(\kappa+4)(\kappa+3)(\kappa+2)} +
                                           \frac{h}{(h+4)(h+3)(h+2)}\right) \mbox{,}}

\deqn{\lambda^{(1)}_3 =  \frac{20\alpha}{3} \left(\frac{\kappa (\kappa - 1)}
                                                {(\kappa+5)(\kappa+4)(\kappa+3)(\kappa+2)} -
					   \frac{h (h - 1)}
					        {(h+5)(h+4)(h+3)(h+2)} \right) \mbox{,}}

\deqn{\lambda^{(1)}_4 = \frac{15\alpha}{2} \left(\frac{\kappa (\kappa - 2)(\kappa - 1)}
                                                {(\kappa+6)(\kappa+5)(\kappa+4)(\kappa+3)(\kappa+2)} +
					   \frac{h (h - 2)(h - 1)}
					        {(h+6)(h+5)(h+4)(h+3)(h+2)} \right) \mbox{, and}}
\deqn{\lambda^{(1)}_5 = \frac{42\alpha}{5} \left(N1 - N2 \right) \mbox{,}}

where

\deqn{N1 = \frac{\kappa (\kappa - 3)(\kappa - 2)(\kappa - 1) }
                {(\kappa+7)(\kappa+6)(\kappa+5)(\kappa+4)(\kappa+3)(\kappa+2)} \mbox{ and}}
\deqn{N2 = \frac{h (h - 3)(h - 2)(h - 1)}{(h+7)(h+6)(h+5)(h+4)(h+3)(h+2)} \mbox{.}}

The TL-moment (\eqn{t=1}) for \eqn{\tau^{(1)}_3} is 
						
\deqn{\tau^{(1)}_3 = \frac{10}{9} \left( \frac{\kappa(\kappa-1)(h+5)(h+4)(h+3)(h+2) - 
                     h(h-1)(\kappa+5)(\kappa+4)(\kappa+3)(\kappa+2)}
		    {(\kappa+5)(h+5) \times [\kappa(h+4)(h+3)(h+2) + 
		                                h(\kappa+4)(\kappa+3)(\kappa+2)]
		    } \right)
		    \mbox{.}}

The TL-moment (\eqn{t=1}) for \eqn{\tau^{(1)}_4} is 
						
\deqn{N1 = \kappa(\kappa-2)(\kappa-1)(h+6)(h+5)(h+4)(h+3)(h+2) \mbox{,}}
\deqn{N2 = h(h-2)(h-1)(\kappa+6)(\kappa+5)(\kappa+4)(\kappa+3)(\kappa+2) \mbox{,}}
\deqn{D1 = (\kappa+6)(h+6)(\kappa+5)(h+5) \mbox{,}}
\deqn{D2 = [\kappa(h+4)(h+3)(h+2) + h(\kappa+4)(\kappa+3)(\kappa+2)] \mbox{, and}}
\deqn{\tau^{(1)}_4 = \frac{5}{4} \left( \frac{N1 +  N2}{D1 \times D2} \right) \mbox{.}}

Finally the TL-moment (\eqn{t=1}) for \eqn{\tau^{(1)}_5} is

\deqn{N1 = \kappa(\kappa-3)(\kappa-2)(\kappa-1)(h+7)(h+6)(h+5)(h+4)(h+3)(h+2) \mbox{,}}
\deqn{N2 = h(h-3)(h-2)(h-1)(\kappa+7)(\kappa+6)(\kappa+5)(\kappa+4)(\kappa+3)(\kappa+2) \mbox{,}}
\deqn{D1 = (\kappa+7)(h+7)(\kappa+6)(h+6)(\kappa+5)(h+5) \mbox{,}}
\deqn{D2 = [\kappa(h+4)(h+3)(h+2) + h(\kappa+4)(\kappa+3)(\kappa+2)] \mbox{, and}}
\deqn{\tau^{(1)}_5 = \frac{7}{5} \left( \frac{N1 -  N2}{D1 \times D2} \right)\mbox{.}}

By inspection the \eqn{\tau_r} equations are not applicable for negative integer values \eqn{k=\{-2, -3, -4, \dots \}} and \eqn{h=\{-2, -3, -4, \dots \}} as division by zero will result. There are additional, but difficult to formulate, restrictions on the parameters both to define a valid Generalized Lambda distribution as well as valid L-moments. Verification of the parameters is conducted through \code{\link{are.pargld.valid}}, and verification of the L-moment validity is conducted through \code{\link{are.lmom.valid}}.
}
\usage{
lmomTLgld(gldpara)
}
\arguments{
  \item{gldpara}{The parameters of the distribution.}
}
\value{
  An R \code{list} is returned.

  \item{lambdas}{Vector of the TL-moments. First element is
\eqn{\lambda^{(1)}_1}, second element is \eqn{\lambda^{(1)}_2}, and so on.}
  \item{ratios}{Vector of the TL-moment ratios. Second element is 
\eqn{\tau^{(1)}}, third element is \eqn{tau^{(1)}_3} and so on. }
  \item{trim}{Trim level = 1}
  \item{source}{An attribute identifying the computational source 
of the TL-moments: \dQuote{lmomTLgld}.}
}
\source{
Derivations conducted by W.H. Asquith on February 18 and 19, 2006.
}
\references{
Elamir, E.A.H., and Seheult, A.H., 2003, Trimmed L-moments: Computational statistics
and data analysis, vol. 43, pp. 299-314.

Hosking, J.R.M., 1990, L-moments--Analysis and estimation of
distributions using linear combinations of order statistics: Journal
of the Royal Statistical Society, Series B, vol. 52, p. 105-124.

Karian, Z.A., and Dudewicz, E.J., 2000, Fitting statistical distributions--The generalized lambda distribution and generalized bootstrap methods: 
CRC Press, Boca Raton, FL, 438 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{lmomgld}}, \code{\link{pargld}}, \code{\link{cdfgld}}, \code{\link{quagld}}} }
\examples{
lmomgld(vec2par(c(10,10,0.4,1.3),type='gld'))
}
\keyword{distribution}
