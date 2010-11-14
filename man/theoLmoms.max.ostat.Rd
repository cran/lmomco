\name{theoLmoms.max.ostat}
\alias{theoLmoms.max.ostat}
\title{Compute the Theoretical L-moments of a Distribution Distribution based on System of Maximum Order Statistic Expectations}
\description{
This function computes
}
\usage{
theoLmoms.max.ostat(para=NULL, cdf=NULL, pdf=NULL, nmom=5, ...)
}
\arguments{
  \item{para}{A distribution parameter list from a function such as \code{vec2par} or \code{lmom2par}.}
  \item{cdf}{CDF of the distribution for the parameters.}
  \item{pdf}{PDF of the distribution for the parameters.}
  \item{nmom}{The number of L-moments to compute.}
  \item{...}{Optional, but likely, arguments to pass to \code{expect.max.ostat}. Such arguments will likely tailor the integrations limits specific for the distribution in question.}
}
\value{
  An R \code{list} is returned.

  \item{lambdas}{Vector of the L-moments. First element is
\eqn{\lambda_1}, second element is \eqn{\lambda_2}, and so on.}
  \item{ratios}{Vector of the L-moment ratios. Second element is
\eqn{\tau}, third element is \eqn{\tau_3} and so on.}
  \item{trim}{Level of symmetrical trimming used in the computation, which will equal \code{NULL} until trimming support is made.}
  \item{leftrim}{Level of left-tail trimming used in the computation, which will equal \code{NULL} until trimming support is made.}
  \item{rightrim}{Level of right-tail trimming used in the computation, which will equal \code{NULL} until trimming support is made.}
  \item{source}{An attribute identifying the computational source of the L-moments: \dQuote{theoLmoms.max.ostat}.}
}
\author{ W.H. Asquith}
\seealso{\code{\link{theoLmoms}}, \code{\link{expect.max.ostat}}}
\examples{
para <- vec2par(c(40,20), type='nor')
theoLmoms.max.ostat(para=para, cdf=cdfnor, pdf=pdfnor)

# The Rice distribution is complex and tailoring of the integration
# limits is needed to effectively trapped errors, the limits for the
# Normal distribution above are infinite so no granular control is needed.
para <- vec2par(c(30,.5), type="rice")
theoLmoms.max.ostat(para=para, cdf=cdfrice, pdf=pdfrice,
                    lower=0, upper=.Machine$double.max)
}
\keyword{distribution}
