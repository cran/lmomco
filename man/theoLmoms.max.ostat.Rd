\name{theoLmoms.max.ostat}
\alias{theoLmoms.max.ostat}
\alias{theoLmoms.min.ostat}
\title{Compute the Theoretical L-moments of a Distribution Distribution based on System of Maximum Order Statistic Expectations}
\description{
This function computes
}
\usage{
theoLmoms.max.ostat(para=NULL, cdf=NULL, pdf=NULL, nmom=4,
                    switch2minostat=FALSE, ...)
}
\arguments{
  \item{para}{A distribution parameter list from a function such as \code{vec2par} or \code{lmom2par}.}
  \item{cdf}{CDF of the distribution for the parameters.}
  \item{pdf}{PDF of the distribution for the parameters.}
  \item{nmom}{The number of L-moments to compute.}
  \item{switch2minostat}{A logical in which a switch to the expectations of minimum order statistics will be used and \code{\link{expect.min.ostat}} will be used with tiny change in overall numerics. The function \code{\link{theoLmoms.min.ostat}} provides a direct interface for L-moment computation by minimum order statistics.}
  \item{...}{Optional, but likely, arguments to pass to \code{expect.max.ostat}. Such arguments will likely tailor the integration limits that can be specific for the distribution in question. Further these arguments might be needed for the cumulative distribution function.}
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
\dontrun{
para <- vec2par(c(40,20), type='nor')
theoLmoms.max.ostat(para=para, cdf=cdfnor, pdf=pdfnor)

# The Rice distribution is complex and tailoring of the integration
# limits is needed to effectively trapped errors, the limits for the
# Normal distribution above are infinite so no granular control is needed.
para <- vec2par(c(30,10), type="rice")
theoLmoms.max.ostat(para=para, cdf=cdfrice, pdf=pdfrice,
                    lower=0, upper=.Machine$double.max)
}

\dontrun{
para <- vec2par(c(0.6, 1.5), type="emu")
theoLmoms.min.ostat(para, cdf=cdfemu, pdf=pdfemu,
                    lower=0, upper=.Machine$double.max)
theoLmoms.min.ostat(para, cdf=cdfemu, pdf=pdfemu, yacoubsintegral = FALSE,
                    lower=0, upper=.Machine$double.max)


para <- vec2par(c(0.6, 1.5), type="kmu")
theoLmoms.min.ostat(para, cdf=cdfkmu, pdf=pdfkmu,
                    lower=0, upper=.Machine$double.max)
theoLmoms.min.ostat(para, cdf=cdfkmu, pdf=pdfkmu, marcumQ = FALSE,
                    lower=0, upper=.Machine$double.max)

}


\dontrun{
# The Normal distribution is used on the fly for the Rice for
# high signal to noise ratios (SNR=nu/alpha > some threshold). This
# application here will error out.
nu <- 30; alpha <- 0.5
para <- vec2par(c(nu,alpha), type="rice")
theoLmoms.max.ostat(para=para, cdf=cdfrice, pdf=pdfrice,
                    lower=0, upper=.Machine$double.max)
}
}
\keyword{L-moment (theoretical)}
\keyword{order statistics}
