\name{lmomrice}
\alias{lmomrice}
\title{L-moments of the Rice Distribution}
\description{
This function estimates the L-moments of the Rice distribution given
the parameters (\eqn{\nu} and \eqn{\alpha}) from \code{\link{parrice}}. The L-moments in terms of the parameters are complex. They are computed here by the system of maximum order statistic expectations from the \code{\link{theoLmoms.max.ostat}}, which uses the \code{\link{expect.max.ostat}}. The connection between \eqn{\tau_2} and \eqn{\nu/\alpha} and a special function (Laguerre polynomial) of \eqn{\nu^2/\alpha^2} and additional algebraic terms is tabulated in the data frame located in \env{.lmomcohash$RiceTable}. The file \file{SysDataBuilder.R} provides additional technical details.
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
  \item{rightrim}{Level of right-tail trimming used in the computation, which will equal \code{NULL} until trimming support is made.}
\item{source}{An attribute identifying the computational
source of the L-moments: \dQuote{lmomrice}, but the exact contents of the remainder of the string might vary as limiting distributions of Normal and Rayleigh can be involved for \eqn{\nu/\alpha > 52} (super high SNR, Normal) or \eqn{24 < \nu/\alpha \le 52} (high SNR, Normal) or \eqn{\nu/\alpha < 0.08} (very low SNR, Rayleigh).}
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


\dontrun{
# A polynomial expression for the relation between
# L-skew and L-kurtosis for the Rice distribution can
# be readily constructed.
T3 <- .lmomcohash$RiceTable$TAU3
T4 <- .lmomcohash$RiceTable$TAU4
LM <- lm(T4~T3+I(T3^2)+I(T3^3)+I(T3^4)+
               I(T3^5)+I(T3^6)+I(T3^7)+I(T3^8))
summary(LM)
Residuals:
       Min         1Q     Median         3Q        Max
-4.585e-05 -1.936e-05  6.640e-07  1.905e-05  1.221e-04

Coefficients:
              Estimate Std. Error   t value Pr(>|t|)
(Intercept)  1.226e-01  7.997e-07 153348.44   <2e-16
T3          -1.782e-01  6.447e-04   -276.45   <2e-16
I(T3^2)     -3.005e+01  1.174e-01   -255.95   <2e-16
I(T3^3)      1.512e+03  8.536e+00    177.18   <2e-16
I(T3^4)     -4.017e+04  3.089e+02   -130.03   <2e-16
I(T3^5)      6.365e+05  6.097e+03    104.38   <2e-16
I(T3^6)     -5.885e+06  6.672e+04    -88.21   <2e-16
I(T3^7)      2.925e+07  3.799e+05     76.98   <2e-16
I(T3^8)     -6.020e+07  8.778e+05    -68.58   <2e-16
---
Residual standard error: 2.009e-05 on 2685 degrees
of freedom. Multiple R-squared: 1,
Adjusted R-squared: 1 F-statistic: 5.37e+07 on 8
and 2685 DF, p-value: < 2.2e-16
}
}
\keyword{L-moment (distribution)}
