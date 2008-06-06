\name{check.pdf}
\alias{check.pdf}
\title{ Check and Potentially Graph Probability Density Functions }
\description{
This convenience function checks that a given probability density function from \pkg{lmomco} appears to workout as mathematically valid. Basically a pdf function must integrate to unity. The \code{check.fs} function permits some flexibility in the limits of integration and provides a high-level interface from graphical display of the pdf.
}
\usage{
check.pdf(pdfunc,  para, lowerF=0.001, upperF=0.999, 
eps=0.02, verbose=FALSE, plot=FALSE, plotlowerF=0.001, 
plotupperF=0.999, ...)
}
\arguments{
  \item{pdfunc}{A probability density function from \pkg{lmomco}.}
  \item{lowerF}{The lower bounds of nonexceedance probability for the numerical integration.}
  \item{upperF}{The upper bounds of nonexceedance probability for the numerical integration.}
  \item{para}{The parameters of the distribution.}
  \item{eps}{An error term expressing allowable error (deviation) of the numerical integration from unity. (If that is the objective of the call to the \code{check.pdf} function.)}
  \item{verbose}{Is verbose output desired?}
  \item{plot}{Should a plot (polygon) of the pdf integration be produce?}
  \item{plotlowerF}{Alternative lower limit for the generation of the curve depicting the pdf function.}
  \item{plotupperF}{Alternative upper limit for the generation of the curve depicting the pdf function.}
  \item{...}{Additional arguments that are passed onto the \code{integration} function.}
}
\value{
An R list structure is returned
  \item{isunity}{Given the \code{eps} is \code{F} close enough.}
  \item{F}{The numerical integration of the probability density function from \code{lowerF} to \code{upperF}.}
}
\author{ W.H. Asquith}
\examples{
lmr <- vec2lmom(c(100,40,0.1)) # Arbitrary L-moments
gev <- pargev(lmr) # parameters of Generalized Extreme Value distribution
wei <- parwei(lmr) # parameters of Weibull distribution

# The Weibull is effectively a reversed GEV and the plots in the
# following examples should demonstrate this.

# Two examples that should integrate to "unity" given default parameters.
check.pdf(pdfgev,gev,plot=TRUE)
check.pdf(pdfwei,wei,plot=TRUE)

# Two examples that will not, but the integrated value on the return list
# should be very close to the median (F=0.5) and the resulting plots
# should affirm what this convenience function is actually doing.
check.pdf(pdfgev,upperF=0.5,gev,plot=TRUE)
check.pdf(pdfwei,upperF=0.5,wei,plot=TRUE)
}
\keyword{distribution}
