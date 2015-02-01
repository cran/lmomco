\name{quagam}
\alias{quagam}
\title{Quantile Function of the Gamma Distribution}
\description{
This function computes the quantiles of the Gamma distribution given
parameters (\eqn{\alpha} and \eqn{\beta}) computed by \code{\link{pargam}}.  The quantile function has no explicit form. See the \code{qgamma} function of \R and \code{\link{cdfgam}}. The parameters have the following interpretations: \eqn{\alpha} is a shape parameter and \eqn{\beta} is a scale parameter in the \R syntax of the \code{qgamma} function.
}
\usage{
quagam(f, para, paracheck=TRUE)
}
\arguments{
  \item{f}{Nonexceedance probability (\eqn{0 \le F \le 1}).}
  \item{para}{The parameters from \code{\link{pargam}} or \code{\link{vec2par}}.}
  \item{paracheck}{A logical controlling whether the parameters are checked for validity. Overriding of this check might be extremely important and needed for use of the quantile function in the context of TL-moments with nonzero trimming.}
}
\value{
  Quantile value for nonexceedance probability \eqn{F}.
}
\references{
Hosking, J.R.M., 1990, L-moments---Analysis and estimation of
distributions using linear combinations of order statistics: Journal
of the Royal Statistical Society, Series B, v. 52, pp. 105--124.

Hosking, J.R.M., 1996, FORTRAN routines for use with the method of L-moments:
Version 3, IBM Research Report RC20525, T.J. Watson Research Center,
Yorktown Heights, New York.

Hosking, J.R.M., and Wallis, J.R., 1997, Regional frequency analysis---An
approach based on L-moments: Cambridge University Press.
}
\author{ W.H. Asquith}
\seealso{\code{\link{cdfgam}}, \code{\link{pdfgam}}, \code{\link{lmomgam}}, \code{\link{pargam}}}
\examples{
  lmr <- lmoms(c(123,34,4,654,37,78))
  g <- pargam(lmr)
  quagam(0.5,g)
\dontrun{
  # generate 50 random samples from this fitted parent
  Qsim <- rlmomco(5000,g)
  # compute the apparent gamma parameter for this parent
  gsim <- pargam(lmoms(Qsim))
}
}
\keyword{distribution}
\keyword{quantile function}
\keyword{Distribution: Gamma}
