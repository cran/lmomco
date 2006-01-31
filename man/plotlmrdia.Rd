\name{plotlmrdia}
\alias{plotlmrdia}
\title{Plot L-moment Ratio Diagram}
\description{
Plot the L-moment ratio diagram of L-skew and L-kurtosis from an L-moment ratio
diagram object returned by \code{lmrdia}.  This diagram is useful for selecting
a distribution to model the data. The application of L-moment diagrams is well 
documented in the literature. This function is intended to function as a demonstration
of L-moment diagram plotting. It is expected that users will \dQuote{roll their own}.
}
\usage{
plotlmrdia(lmr, nopoints=FALSE, nolines=FALSE, nolimits=FALSE,
           nogev=FALSE, noglo=FALSE, nogpa=FALSE, nope3=FALSE, nogno=FALSE,
	   noexp=FALSE, nonor=FALSE, nogum=FALSE, nouni=FALSE, ...)
}
\arguments{
  \item{lmr}{L-moment diagram object from \code{lmrdia}.}
  \item{nopoints}{If \code{TRUE} then point distributions are not drawn.}
  \item{nolines}{If \code{TRUE} then line distributions are not drawn.}
  \item{nolimits}{If \code{TRUE} then theoretical limits of L-moments are not drawn.}
  \item{nogev}{If \code{TRUE} then line of Generalized Extreme Value distribution is not drawn.}
  \item{noglo}{If \code{TRUE} then line of Generalized Logistic distribution is not drawn.}
  \item{nogno}{If \code{TRUE} then line of Generalized Normal (log-Normal) distribution is not drawn.}
  \item{nogpa}{If \code{TRUE} then line of Generalized Pareto distribution is not drawn.}
  \item{nope3}{If \code{TRUE} then line of Pearson Type III distribution is not drawn.}
  \item{noexp}{If \code{TRUE} then point of Exponential distribution is not drawn.}
  \item{nonor}{If \code{TRUE} then point of Normal distribution is not drawn.}
  \item{nogum}{If \code{TRUE} then point of Gumbel distribution is not drawn.}
  \item{nouni}{If \code{TRUE} then point of Uniform distribution is not drawn.}
  \item{...}{Additional arguments passed onto the \code{plot} function.}
}
\note{
This function provides hardwired calls to \code{lines} and \code{points} to 
produce the diagram. The plot symbology for the shown distributions is summarized here. 
The Kappa (four parameter) and Wakeby (five parameter) distributions are not well 
represented on the diagram as each constitute an area (Kappa) or hyperplane (Wakeby) and 
not a line (three-parameter distributions) or a point (two-parameter distributions). However, 
the Kappa demarks the area bounded by the Generalized Logistic (\code{glo}) on the top and the 
theoretical L-moment limits on the bottom.

\tabular{cl}{
  \bold{GRAPHIC TYPE} \tab \bold{GRAPHIC NATURE} \cr
  L-moment Limits \tab line width 2 and color 8 (grey) \cr
  Generalized Extreme Value \tab line width 1, line type 2 (dash), and color 2 (red) \cr
  Generalized Logistic \tab line width 1 and color 3 (green) \cr
  Generalized Normal \tab line width 1, line type 2 (dash), and color 4 (blue) \cr
  Generalized Pareto \tab line width 1 and color 4 (blue) \cr
  Pearson Type III \tab line width 1 and color 6 (purple) \cr
  Exponential \tab symbol 16 (filled circle) and color 2 (red) \cr
  Normal \tab symbol 15 (filled square) and color 2 (red) \cr
  Gumbel \tab symbol 17 (filled triangle) and color 2 (red) \cr
  Uniform \tab symbol 18 (filled diamond) and color 2 (red) \cr
}
}
\references{
Asquith, W.H., 1998, Depth-duration frequency of precipitation for Texas: U.S.
Geological Survey Water-Resources Investigations Report 98-4044, 107 p.

Hosking, J.R.M., 1990, L-moments--Analysis and estimation of
distributions using linear combinations of order statistics: Journal
of the Royal Statistical Society, Series B, vol. 52, p. 105-124.

Hosking, J.R.M. and Wallis, J.R., 1997, Regional frequency analysis--An
approach based on L-moments: Cambridge University Press.

Vogel, R.M., and Fennessey, N.M., 1993, L moment diagrams should replace
product moment diagrams: Water Resources Research, vol. 29, no. 6, pp. 1745-1752.
}
\author{ W.H. Asquith}
\seealso{\code{\link{lmrdia}}}
\examples{
plotlmrdia(lmrdia())
}
\keyword{distribution}
