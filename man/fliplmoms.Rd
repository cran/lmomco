\name{fliplmoms}
\alias{fliplmoms}
\title{Flip L-moments by Flip Attribute in L-moment Vector}
\description{
This function flips the L-moments by a flip attribute in an L-moment object such as that returned by \code{lmomsRCmark}. The function will attempt to identify the L-moment object and \code{lmorph} as necessary, but this support is not guaranteed.  The flipping process is used to support left-tail censoring using the right-tail censoring alogrithms of the package. The odd order (\code{seq(3,n,by2)}) \eqn{\lambda_r} and \eqn{\tau_r} are negated. The mean \eqn{\hat\lambda_1} is computed by subtracting the \eqn{\lambda_1} from the \var{lmom} argument from the flip \var{M}: \eqn{\hat\lambda_1 = M - \lambda_1} and the \eqn{\tau_2} is subsequently adjusted by the new mean. This function is written to provide a convenient method to re-transform or back flip the L-moments computed by \code{lmomsRCmark}. Careful analysis of the example problem listed here should be made.
}
\usage{
fliplmoms(lmom, checklmom=TRUE)
}
\arguments{
  \item{lmom}{A L-moment object created by \code{\link{lmomsRCmark}}.}
  \item{checklmom}{Should the \var{lmom} be checked for validity using the \code{are.lmom.valid} function. Normally this should be left as the default and it is very unlikely that the L-moments will not be viable (particularly in the \eqn{\tau_4} and \eqn{\tau_3} inequality). However, for some circumstances or large simulation exercises then one might want to bypass this check.}
}

\value{
  An R \code{list} is returned that matches the structure of the \var{lmom} argument (unless an \code{lmorph} was attempted). The structure is intended to match that coming from \code{lmomsRCmark}.
}
\references{
Wang, Dongliang, Hutson, A.D., Miecznikowski, J.C., 2010, L-moment estimation for parametric survival models given censored data: Statistical Methodology, preprint

Helsel, D.R., 2005, Nondetects and data analysis---Statistics for censored environmental data: Hoboken, New Jersey, John Wiley, 250 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{lmomsRCmark}}
}
\examples{
# Create some data with **multiple detection limits**
# This is a left-tail censoring problem--flipping will be required.
fakedat1 <- rnorm(50, mean=16, sd=5);
fake1.left.censor.indicator <- fakedat1 < 14;
fakedat1[fake1.left.censor.indicator]   <- 14;

fakedat2 <- rnorm(50, mean=16, sd=5);
fake2.left.censor.indicator <- fakedat2 < 10;
fakedat2[fake2.left.censor.indicator]   <- 10;

# combine the data sets
fakedat <- c(fakedat1, fakedat2);
fake.left.censor.indicator <- c(fake1.left.censor.indicator,
                                fake2.left.censor.indicator);
ix <- order(fakedat);
fakedat <- fakedat[ix];
fake.left.censor.indicator <- fake.left.censor.indicator[ix];

lmr.usual       <- lmoms(fakedat);
lmr.flipped     <- lmomsRCmark(fakedat, flip=TRUE,
                               rcmark=fake.left.censor.indicator);
lmr.backflipped <- fliplmoms(lmr.flipped); # re-transform
pch <- as.numeric(fake.left.censor.indicator)*15 + 1;
F <- nonexceeds();
plot(pp(fakedat), sort(fakedat), pch=pch,
     xlab="NONEXCEEDANCE PROBABILITY",
     ylab="DATA VALUE");
lines(F, qlmomco(F, parnor(lmr.backflipped)), lwd=2)
lines(F, qlmomco(F, parnor(lmr.usual)), lty=2)
legend(0,20, c("Uncensored", "Left-tail censored"), pch=c(1,16))
# The solid line represented the Normal distribution fit by
# censoring indicator on the multiple left-tail detection limits.
}
\keyword{distribution}
