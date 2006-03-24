\name{lmrdia}
\alias{lmrdia}
\title{L-moment Ratio Diagram Components}
\description{
This function returns a list of the L-skew and L-kurtosis (\eqn{\tau_3} and \eqn{\tau_4}, 
respectively) ordinates for construction of L-moment Ratio (L-moment diagrams) 
that are useful in selecting a distribution to model the data.
}
\usage{
lmrdia()
}
\value{
  An R \code{list} is returned.

  \item{limits}{The theoretical limits of \eqn{\tau_3} and \eqn{\tau_4}--below \eqn{\tau_4} are theoretically
  not possible.}
  \item{exp}{\eqn{\tau_3} and \eqn{\tau_4} of the Exponential distribution.}
  \item{gam}{\eqn{\tau_3} and \eqn{\tau_4} of the Gamma distribution.}
  \item{gev}{\eqn{\tau_3} and \eqn{\tau_4} of the Generalized Extreme Value distribution.}
  \item{glo}{\eqn{\tau_3} and \eqn{\tau_4} of the Generalized Logistic distribution.}
  \item{gpa}{\eqn{\tau_3} and \eqn{\tau_4} of the Generalized Pareto distribution.}
  \item{gum}{\eqn{\tau_3} and \eqn{\tau_4} of the Gumbel distribution.}
  \item{lognormal}{\eqn{\tau_3} and \eqn{\tau_4} of the Lognormal distribution.}
  \item{nor}{\eqn{\tau_3} and \eqn{\tau_4} of the Normal distribution.}
  \item{pe3}{\eqn{\tau_3} and \eqn{\tau_4} of the Pearson Type III distribution.}
  \item{uniform}{\eqn{\tau_3} and \eqn{\tau_4} of the uniform distribution.}
}
\references{
Hosking, J.R.M., 1990, L-moments---Analysis and estimation of
distributions using linear combinations of order statistics: Journal
of the Royal Statistical Society, Series B, vol. 52, p. 105--124.

Hosking, J.R.M., 1996, FORTRAN routines for use with the method of L-moments:
Version 3, IBM Research Report RC20525, T.J. Watson Research Center,
Yorktown Heights, New York.

Hosking, J.R.M. and Wallis, J.R., 1997, Regional frequency analysis---An
approach based on L-moments: Cambridge University Press.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{plotlmrdia}}}
\examples{
lratios <- lmrdia()
}
\keyword{distribution}
