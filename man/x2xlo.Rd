\name{x2xlo}
\alias{x2xlo}
\title{Conversion of a Vector through a Left-Hand Threshold to Setup Conditional Probability Computations}
\description{
This function takes a vector of numerical values and subselects the values above and those equal to or less than the \code{leftout} argument and assigns plodding positions based on the \code{a} argument (passed into the \code{\link{pp}} function) and returns a list providing helpful as well as necessary results needed for conditional probability adjustment for general magnitude and frequency analysis. This function only performs very simple vector of selection of patient. The real features for conditional probability application are found in the \code{\link{f2flo}} and \code{\link{f2flo}} functions.
}
\usage{
x2xlo(x, leftout=0, a=0)
}
\arguments{
  \item{x}{A vector of values.}
  \item{leftout}{The lower threshold for which to leave out. The default of zero sets up for conditional probability adjustments for values equal (or less than) zero.}
  \item{a}{The plotting position coefficient passed to the \code{\link{pp}} function.}
}
\value{
  An R \code{list} is returned.

  \item{xin}{The subselection of values greater than the \code{leftout} threshold.}
  \item{ppin}{The plotting positions of the subselected values greater than the \code{leftout} threshold. These plotting positions correspond to those data values in \code{xin}.}
  \item{xout}{The subselection of values less than or equal to the \code{leftout} threshold.}
  \item{ppout}{The plotting positions of the subselected values  less than or equal to the \code{leftout} threshold. These plotting positions correspond to those data values in \code{xout}.}
  \item{pp}{The plotting position of the largest value left out of \code{xin}.}
  \item{source}{The source of the parameters: \dQuote{x2xlo}.}
}
\author{ W.H. Asquith}
\seealso{ \code{\link{f2flo}}, \code{\link{flo2f}} }
\examples{

\dontrun{
set.seed(13)
type <- "nor"
parent <- vec2par(c(0,1), type=type)
X <- rlmomco(100, parent); a <- 0
Fs <- seq(0.001,.999, by=.005)
PP <- pp(X, a=a); Xs <- sort(X)
par <- lmom2par(lmoms(X), type=type)
plot(PP, Xs, type="n", xlim=c(0,1),
     xlab="NONEXCEEDANCE PROBABILITY",
     ylab="RANDOM VARIATE")
points(PP, Xs, col=3, cex=2, pch=0)
Xlo <- x2xlo(X, leftout=0, a=a)
parlo <- lmom2par(lmoms(Xlo$xin), type=type)
points(Xlo$ppout, Xlo$xout, pch=4, col=4)
points(Xlo$ppin, Xlo$xin, col=4, cex=.7)
lines(Fs, qlmomco(Fs, parent), lty=2, lwd=2)
lines(Fs, qlmomco(Fs, par), col=2, lwd=4)
lines(sort(c(Xlo$ppin, 0.999)),
      qlmomco(f2flo(sort(c(Xlo$ppin, 0.999)), pp=Xlo$pp), parlo),
      col=4, lwd=3)
# Notice how in the last line plotted that the proper
# plotting positions of the data greater than the threshold
# are passed into the f2flo() function that has the
# effect of mapping conventional nonexceedance probabilities
# into the conditional probability space. These mapped
# probabilities are then passed into the quantile function.
legend(.1,2, c("Simulated random variates",
                "Values to 'leave' (condition) out",
                "Values to 'leave' in",
                "Normal parent",
                "Normal fitted to whole data set",
                "Normal fitted to left-in values"),
       bty="n", cex=.75,
       pch=c(0,  4, 1,  NA, NA, NA),
       col=c(3,  1, 4,  1,  2,  4),
       pt.lwd=c(2,1, 1,1),
       pt.cex=c(2,1, 0.7, 1),
       lwd=c(0,0,0,2,2,3),
       lty=c(0,0,0,2,1,1))
}

\dontrun{
set.seed(62)
type <- "exp"
parent <- vec2par(c(0,1), type=type)
X <- rlmomco(100, parent); a <- 0
Fs <- seq(0.001,.999, by=.005)
PP <- pp(X, a=a); Xs <- sort(X)
par <- lmom2par(lmoms(X), type=type)
plot(PP, Xs, type="n", xlim=c(0,1), log="y",
     xlab="NONEXCEEDANCE PROBABILITY",
     ylab="RANDOM VARIATE")
points(PP, Xs, col=3, cex=2, pch=0, lwd=2)
Xlo <- x2xlo(X, leftout=0.5, a=a)
parlo <- lmom2par(lmoms(Xlo$xin), type=type)
points(Xlo$ppout, Xlo$xout, pch=4, col=1)
points(Xlo$ppin, Xlo$xin, col=4, cex=.7)
lines(Fs, qlmomco(Fs, parent), lty=2, lwd=2)
lines(Fs, qlmomco(Fs, par), col=2, lwd=4)
lines(sort(c(Xlo$ppin,.999)),
      qlmomco(f2flo(sort(c(Xlo$ppin,.999)), pp=Xlo$pp), parlo),
      col=4, lwd=3)
legend(.4,.2, c("Simulated random variates",
                "Values to 'leave' (condition) out",
                "Values to 'leave' in",
                "Exponential parent",
                "Exponential fitted to whole data set",
                "Exponential fitted to left-in values"),
       bty="n", cex=.75,
       pch=c(0,  4, 1,  NA, NA, NA),
       col=c(3,  1, 4,  1,  2,  4),
       pt.lwd=c(2,1, 1,1),
       pt.cex=c(2,1, 0.7, 1),
       lwd=c(0,0,0,2,2,3),
       lty=c(0,0,0,2,1,1))
}
}
\keyword{misc}
