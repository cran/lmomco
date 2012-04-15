\name{paraep}
\alias{paraep}
\title{Estimate the Parameters of the Asymmetric Exponential Power Distribution}
\description{
This function estimates the parameters of the Asymmetric Exponential Power distribution given the L-moments of the data in an L-moment object such as that returned by \code{lmoms}. The relation between distribution parameters and L-moments is seen under \code{lmomaep}. Relatively straightforward, but difficult to numerically achieve, optimization is needed to extract the parameters from the L-moments.

Delicado and Goria (2008) make a compelling case for the optimization to use the following objective function

\deqn{\epsilon(\alpha, \kappa, h) = \log(1 + \sum_{r=2}^4 (\hat\lambda_r - \lambda_r)^2)}

and solve directly for \eqn{\xi}.

This objective function was chosen by those authors because the solution surface can become quite flat for away from the minimum.  The author of \pkg{lmomco} agrees with the findings of those authors from limited exploratory analysis and the development of the algorithms used here under the rubic of the \dQuote{Delicado-Goria} method with a rough algorithm to select initial guesses for the parameters. An evident drawback of their algorithm, is that precision in \eqn{\alpha} is lost according to the observation that this parameter can be analytically computed given \eqn{\lambda_2}, \eqn{\kappa}, and \eqn{h}.

It is established practice in L-moment theory of four (and similarly three) parameter distributions to see expressions for \eqn{\tau_3} and \eqn{\tau_4} used for numerical optimization to obtain the two higher parameters (\eqn{\alpha} and \eqn{h}) first and then see analytical expressions directly compute the two lower parameters (\eqn{\xi} and \eqn{\alpha}). The author made various exploratory studies by optimizing on \eqn{\tau_3} and \eqn{\tau_4} through a least squares objective function. Such a practice performed poorly when compared to that recommended by Delicado and Goria (2008). The author has decided to extend this function with a method titled \dQuote{Asquith} in which the estimates of \eqn{\kappa} and \eqn{h}  emanating from the Delicado-Goria method are used as new initial guesses to these two parameters. Subsequently, another optimization is performed on the following objective function

\deqn{\epsilon(\kappa, h) = \sqrt{\sum_{r=3}^4 (\hat\tau_r - \tau_r)^2}}

and solve directly for \eqn{\alpha} and then \eqn{\xi}. The Asquith method appears to perform slightly better in \eqn{\kappa} and \eqn{h} estimation and quite a bit better in \eqn{\alpha} and and \eqn{\xi} as seemingly expected because these last two are analytically computed. The objective function of the Asquith method defaults to use of the \eqn{\sqrt{x}} but this can be removed by setting \code{sqrt.t3t4=FALSE}.
}
\usage{
paraep(lmom, checklmom=TRUE,
       method=c("Asquith", "Delicado-Goria", "Asquith_bypass_Delicado-Goria"),
       sqrt.t3t4=TRUE, eps=1e-3,
       A.guess=NULL, K.guess=NULL, H.guess=NULL)
}
\arguments{
  \item{lmom}{A L-moment object created by \code{\link{lmom.ub}}
or \code{\link{pwm2lmom}}.}
  \item{checklmom}{Should the \code{lmom} be checked for validity using the \code{are.lmom.valid} function. Normally this should be left as the default and it is very unlikely that the L-moments will not be viable (particularly in the \eqn{\tau_4} and \eqn{\tau_3} inequality). However, for some circumstances or large simulation exercises then one might want to bypass this check.}
  \item{method}{Which method for parameter estimation should be used, the \dQuote{Delicado-Goria} method follows that outlined by Delicado and Goria (2008) or the \dQuote{Asquith} method uses Delicado-Goria to obtain refined initial guesses and proceed to optimize again with an different objective function as explained above. All minimization is based on the \code{optim} function using the Nelder-Mead method and default arguments. A rough first initial guesses on the values for the three \eqn{\lambda_{2,3,4}} are made. If \dQuote{Asquith_bypass_Delicado-Goria}, then the attempt to refine parameter guesses through their algorithm is bypassed and the optimize moves directly to the sum of square errors of \eqn{\tau_3} and \eqn{\tau_4}.}
  \item{sqrt.t3t4}{If true and the method is Asquith, then the square root of the sum of square errors in \eqn{\tau_3} and \eqn{\tau_4} are used.}
 \item{eps}{A small term or threshold for which the square root of the sum of square errors in \eqn{\tau_3} and \eqn{\tau_4} is compared to to judge \dQuote{good enough} for the alogrithm to set the \code{ifail} on return in addition to convergence flags coming from the \code{optim} function. Note that \code{eps} is only used if the \dQuote{Asquith} or \dQuote{Asquith_bypass_Delicado-Goria} methods are triggered.}
  \item{A.guess}{A user specified guess of the \eqn{\alpha} parameter to provide to the Delicado-Goria method regardless of whether the method is Asquith. This argument just superceeds the simple initial guess conditionals early in the code.}
  \item{K.guess}{A user specified guess of the \eqn{\kappa} parameter to provide to the Delicado-Goria method regardless of whether the method is Asquith. This argument just superceeds the simple initial guess conditionals early in the code.}
  \item{H.guess}{A user specified guess of the \eqn{h} parameter to provide to the Delicado-Goria method regardless of whether the method is Asquith. This argument just superceeds the simple initial guess conditionals early in the code.}

}
\value{
  An \R list is returned.

  \item{type}{The type of distribution: \code{aep}.}
  \item{para}{The parameters of the distribution.}
  \item{source}{The source of the parameters: \dQuote{paraep}.}
  \item{eachopt}{Another \R list containing the optimization details by the \dQuote{Delicado-Goria} method along with the estimated parameters in \code{para_L234}. The \dQuote{_234} is to signify that optimization is being conducted using \eqn{\lambda_2}, \eqn{\lambda_3}, and \eqn{\lambda_4}. The list also contains the optimization details (if requested) by the \dQuote{Asquith} method along with the estimated parameters in \code{para_T34}. The \dQuote{_T34} is to signify that opimization is being conducted using \eqn{\tau_3} and \eqn{\tau_4} only. The parameter values in \code{para} are those requested by the \code{method}.}
}
\references{
Delicado, P., and Goria, M.N., 2008, A small sample comparison of maximum likelihood,
moments and L-moments methods for the asymmetric exponential power distribution:
Computational Statistics and Data Analysis, v. 52, no. 3, pp. 1661-1673.
}
\author{ W.H. Asquith}
\seealso{\code{\link{lmomaep}},
\code{\link{cdfaep}}, \code{\link{quaaep}}
}
\examples{
PAR <- list(para=c(0,1000, .7, .4), type="aep");
lmr <- lmomaep(PAR)

PARdg  <- paraep(lmr, method="Delicado-Goria")
PARasq <- paraep(lmr, method="Asquith")
PARasq.nodg <- paraep(lmr, method="Asquith_bypass_Delicado-Goria")

F <- c(0.001, 0.005, seq(0.01,0.99, by=0.01), 0.995, 0.999)
qF <- qnorm(F)
plot(qF, quaaep(F, PARdg), type="n",
     xlab="STANDARD NORMAL VARIATE", ylab="QUANTILE")
lines(qF, quaaep(F, PARdg), col=1, lwd=2, lty=2)
lines(qF, quaaep(F, PARasq), col=2, lwd=4) # really the correct curve
lines(qF, quaaep(F, PARasq.nodg), col=4, lwd=4) # really the correct curve

print(PAR$para)
print(PARdg$para)
print(PARasq$para)
print(PARasq.nodg$para)

ePAR1dg <- (PAR$para[1] - PARdg$para[1])^2
ePAR2dg <- (PAR$para[2] - PARdg$para[2])^2
ePAR3dg <- (PAR$para[3] - PARdg$para[3])^2
ePAR4dg <- (PAR$para[4] - PARdg$para[4])^2

ePAR1asq <- (PAR$para[1] - PARasq$para[1])^2
ePAR2asq <- (PAR$para[2] - PARasq$para[2])^2
ePAR3asq <- (PAR$para[3] - PARasq$para[3])^2
ePAR4asq <- (PAR$para[4] - PARasq$para[4])^2

ePAR1asq.nodg <- (PAR$para[1] - PARasq$para[1])^2
ePAR2asq.nodg <- (PAR$para[2] - PARasq$para[2])^2
ePAR3asq.nodg <- (PAR$para[3] - PARasq$para[3])^2
ePAR4asq.nodg <- (PAR$para[4] - PARasq$para[4])^2

MSEdg  <- mean(ePAR1dg,  ePAR2dg,  ePAR3dg,  ePAR4dg)
MSEasq <- mean(ePAR1asq, ePAR2asq, ePAR3asq, ePAR4asq)
MSEasq.nodg <- mean(ePAR1asq.nodg, ePAR2asq.nodg, ePAR3asq.nodg, ePAR4asq.nodg)

# We see that the Asquith method performs radically better
# for the example parameters in PAR
print(MSEdg)
print(MSEasq)
print(MSEasq.nodg)

# ANOTHER EXAMPLE
PAR <- list(para=c(0,100, 1.7, 4), type="aep");
lmr <- lmomaep(PAR)

# First two error out with warnings such as
#   o precision on lgamma
#   o pgamma returning NaN
#   o integrate(delf, 0, 1/2) : non-finite function value (within lmomaep)
# These warnings are attributable to the attempt to use Alpha as well as
# Kappa and H in the optimization.
PARasq1 <- paraep(lmr, method="Asquith", sqrt.t3t4=TRUE)
PARasq2 <- paraep(lmr, method="Asquith", sqrt.t3t4=FALSE)
# These kick of no warnings.
PARasq3 <- paraep(lmr, method="Asquith_bypass_Delicado-Goria", sqrt.t3t4=TRUE)
PARasq4 <- paraep(lmr, method="Asquith_bypass_Delicado-Goria", sqrt.t3t4=FALSE)
print(PAR$para)
print(PARasq1$para)
print(PARasq2$para)
print(PARasq3$para)
print(PARasq4$para)

}
\keyword{distribution}
