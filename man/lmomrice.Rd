\name{lmomrice}
\alias{lmomrice}
\title{L-moments of the Rice Distribution}
\description{
This function estimates the L-moments of the Rice distribution given
the parameters (\eqn{\nu} and \eqn{\mathrm{alpha}}) from \code{\link{parrice}}. The L-moments in terms of the parameters are complex. They are computed here by the system of maximum order statistic expectations from the \code{\link{theoLmoms.max.ostat}}, which uses the \code{\link{expect.max.ostat}}. The connection between \eqn{\tau_2} and \eqn{\nu/\alpha} and a special function (Laguerre polynomial) of \eqn{\nu^2/\alpha^2} and additional algebraic terms is tabulated in the data frame located in \env{.lmomcohash$RiceTable}. The file \file{SysDataBuilder.R} provides additional technical details.
}
\usage{
lmomrice(para, ...)
}
\arguments{
  \item{para}{The parameters of the distribution.}
  \item{...}{Additional arguments passed to the \code{\link{theoLmoms.max.ostat}}.}
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
rice <- vec2par(c(65,34), type="rice")
lmomrice(rice)

\dontrun{
# Use the additional arguments to show how to avoid
# unnecessary overhead when using the Rice, which only
# has two parameters.
  rice <- vec2par(c(15,14), type="rice")
  system.time(lmomrice(rice, nmom=2))
  system.time(lmomrice(rice, nmom=6))


  lcvs <- vector(mode="numeric"); i <- 0
  SNR  <- c(seq(7,0.25, by=-0.25), 0.1)
  for(snr in SNR) {
    i <- i + 1
    rice    <- vec2par(c(10,10/snr), type="rice")
    lcvs[i] <- lmomrice(rice, nmom=2)$ratios[2]
  }
  plot(lcvs, SNR,
       xlab="COEFFICIENT OF L-VARIATION",
       ylab="LOCAL SIGNAL TO NOISE RATIO (NU/ALPHA)")
  lines(.lmomcohash$RiceTable$LCV,
        .lmomcohash$RiceTable$SNR)
  abline(1,0, lty=2)
  mtext("Rice Distribution")
  text(0.15,0.5, "More noise than signal")
  text(0.15,1.5, "More signal than noise")
}

}
\keyword{distribution}
