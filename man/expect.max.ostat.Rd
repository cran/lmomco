\name{expect.max.ostat}
\alias{expect.max.ostat}
\title{Compute the Expection of a Maximum Order Statistic (or others)}
\description{
This function computes
}
\usage{
expect.max.ostat(n, para=NULL, cdf=NULL, pdf=NULL,
                 j=NULL, lower=-Inf, upper=Inf)
}
\arguments{
  \item{n}{The sample size.}
  \item{para}{A distribution parameter list from a function such as \code{vec2par} or \code{lmom2par}.}
  \item{cdf}{CDF of the distribution for the parameters}
  \item{pdf}{PDF of the distribution for the parameters}
  \item{j}{The \eqn{j}th value of the order statistic, which defaults to \var{n=j} if \code{j=NULL}.}
  \item{lower}{The lower limit of the distribution for integration.}
  \item{upper}{The upper limit of the distribution for integration.}
}
\value{
  The expectation of the maximum order statistic, unless \eqn{j} is specified and then the expectation of that order statistic is returned.
}
\author{ W.H. Asquith}
\seealso{\code{\link{theoLmoms.max.ostat}}}
\examples{
para <- vec2par(c(10,100), type="nor")

# The two output values from these two lines should be
# very similar: (1) theoretical and (2) simulation
expect.max.ostat(10, para=para, cdf=cdfnor, pdf=pdfnor)
mean(sapply(1:1000, function(x) { max(rlmomco(10,para))}))
}
\keyword{distribution}

