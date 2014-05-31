\name{add.lmomco.axis}
\alias{add.lmomco.axis}
\title{Add an lmomco Axis to a Plot}
\description{
This function provides special support for adding probability-like axes to an existing plot. The function supports a recurrence interval (RI) axis, normal probability axis (NPP), and standard normal variate (SNV) axis.
}
\usage{
add.lmomco.axis(side=1, twoside=FALSE, side.type=c("NPP", "RI", "SNV"),
                otherside.type=c("NA", "RI", "SNV", "NPP"),
                NPP.control=NULL, RI.control=NULL, SNV.control=NULL, ...)
}
\arguments{
  \item{side}{The side of the plot (1=bottom, 2=left, 3=top, 4=right).}
  \item{twoside}{A logical triggering whether the tick marks are echoed on the opposite side. This value is forced to \code{FALSE} if \code{otherside.type} is not \code{"NA"}.}
  \item{side.type}{The axis type for the primary \code{side}.}
  \item{otherside.type}{The optional axis type for the opposite side. The default is a literal not applicable.}
  \item{NPP.control}{An optional \R \code{list} used to influence the NPP axis.}
  \item{RI.control}{An optional \R \code{list} used to influence the RI axis.}
  \item{SNV.control}{An optional \R \code{list} used to influence the SNV axis.}
   \item{...}{Additional arguments that are passed to the \R function \code{Axis}.}
}
\value{
   No value is returned. This function is used for its side effects.
}
\note{
The \code{NPP.control} provides
\tabular{ll}{
  \code{label} \tab The title for the NPP axis---be careful with value of \code{as.exceed}. \cr
  \code{probs} \tab A vector of nonexceedance probabilities \eqn{F}. \cr
  \code{probs.lab} \tab A vector of nonexceedance probabilities \eqn{F} to label. \cr
  \code{digits} \tab The digits for the \R function \code{format} to enhance appearance. \cr
  \code{line}  \tab The line for the \R function \code{mtext} to place \code{label}. \cr
  \code{as.exceed} \tab A logical triggering \eqn{S = 1 - F}.
}

The \code{RI.control} provides
\tabular{ll}{
  \code{label} \tab The title for the RI axis. \cr
  \code{Tyear} \tab A vector of \eqn{T}-year recurrence intervals. \cr
  \code{line}  \tab The line for the \R function \code{mtext} to place \code{label}.
}

The \code{SNV.control} provides
\tabular{ll}{
  \code{label} \tab The title for the SNV axis. \cr
  \code{begin} \tab The beginning \dQuote{number of standard deviations}. \cr
  \code{end}   \tab The ending \dQuote{number of standard deviations}. \cr
  \code{by}    \tab The step between \code{begin} and \code{end}. \cr
  \code{line}  \tab The line for the \R function \code{mtext} to place \code{label}.
}

The user is responsible for appropriate construction of the \code{control} lists. Very little error trapping is made to keep the code base tight. The defaults when the function definition are likely good for many types of applications. Lastly, the manipulation of the \code{mgp} parameter in the example is to show how to handle the offset between the numbers and the ticks when the ticks are moved to pointing inward, which is opposite of the default in \R.
}
\author{ W.H. Asquith}
\seealso{\code{\link{prob2T}}, \code{\link{T2prob}}}
\examples{
par(mgp=c(3,0.5,0)) # going to tick to the inside, change some parameters
X <- sort(rnorm(65)); pp <- pp(X) # generate synthetic data
plot(qnorm(pp), X, xaxt="n", xlab="", ylab="QUANTILE", xlim=c(-2,3))
add.lmomco.axis(las=2, tcl=0.5, side.type="RI", otherside.type="NPP")
par(mgp=c(3,1,0)) # restore defaults
}
\keyword{utility (probability)}
\keyword{axis (utility)}
\keyword{Graphics}
\concept{probability axis}
\concept{normal probability paper}
\concept{probability paper}
\concept{recurrence interval}
\concept{return period}
\concept{standard normal axis}
\concept{standard normal paper}
\concept{standard normal variate axis}
\concept{standard normal variate paper}
\keyword{The lmomco functions}
