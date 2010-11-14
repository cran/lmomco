\name{lmomrice}
\alias{lmomrice}
\title{L-moments of the Rice Distribution}
\description{
This function estimates the L-moments of the Rice distribution given
the parameters (\eqn{\nu} and \eqn{\mathrm{SNR}}) from \code{\link{parrice}}.
The L-moments in terms of the parameters are complex. They are computed here by the system of maximum order statistic expectations.
}
\usage{
lmomrice(para)
}
\arguments{
  \item{para}{The parameters of the distribution.}
}
\value{
  An R \code{list} is returned.

  \item{lambdas}{Vector of the L-moments. First element is
\eqn{\lambda_1}, second element is \eqn{\lambda_2}, and so on.}
  \item{ratios}{Vector of the L-moment ratios. Second element is
\eqn{\tau}, third element is \eqn{\tau_3} and so on.}
  \item{trim}{Level of symmetrical trimming used in the computation, which will equal \code{NULL} until trimming support is made.}
  \item{leftrim}{Level of left-tail trimming used in the computation, which will equal \code{NULL} until trimming support is made.}
  \item{rightrim}{Level of right-tail trimming used in the computation, which will equal \code{NULL} until trimming support is made.}  \item{source}{An attribute identifying the computational
source of the L-moments: \dQuote{lmomrice}.}
}
\author{ W.H. Asquith}
\seealso{\code{\link{parrice}}, \code{\link{quarice}}, \code{\link{cdfrice}}, \code{\link{theoLmoms.max.ostat}} }
\examples{
rice <- vec2par(c(65,1.9), type='rice')
lmomrice(rice)
}
\keyword{distribution}
