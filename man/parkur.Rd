\name{parkur}
\alias{parkur}
\title{Estimate the Parameters of the Kumaraswamy Distribution}
\description{
This function estimates the parameters of the Kumaraswamy distribution given
the L-moments of the data in an L-moment object such as that returned by
\code{lmom.ub}. The relation between distribution parameters and L-moments
is seen under \code{lmomkur}.
}
\usage{
parkur(lmom,checklmom=TRUE)
}
\arguments{
  \item{lmom}{A L-moment object created by \code{\link{lmom.ub}}
or \code{\link{pwm2lmom}}.}
  \item{checklmom}{Should the \code{lmom} be checked for validity using the \code{are.lmom.valid} function. Normally this should be left as the default and it is very unlikely that the L-moments will not be viable (particularly in the \eqn{\tau_4} and \eqn{\tau_3} inequality). However, for some circumstances or large simulation exercises then one might want to bypass this check.}
}

\value{
  An R \code{list} is returned.

  \item{type}{The type of distribution: \code{kur}.}
  \item{para}{The parameters of the distribution.}
  \item{err}{The convergence error.}
  \item{convergence}{Logical showing whether error convergence occurred.}
  \item{source}{The source of the parameters: \dQuote{parkur}.}
}
\references{
Jones, M.C., 2009, Kumaraswamy's distribution---A beta-type distribution with
some tractability advantages: Statistical Methodology, v.6, pp. 70--81.
}
\author{ W.H. Asquith}
\seealso{\code{\link{lmom.ub}}, \code{\link{lmomkur}},
\code{\link{cdfgum}}, \code{\link{quakur}}
}
\examples{
lmr <- lmom.ub(runif(20)^2)
parkur(lmr)

kurpar <- list(para=c(1,1), type="kur");
lmr <- lmomkur(kurpar)
parkur(lmr)

kurpar <- list(para=c(0.1,1), type="kur");
lmr <- lmomkur(kurpar)
parkur(lmr)

kurpar <- list(para=c(1,0.1), type="kur");
lmr <- lmomkur(kurpar)
parkur(lmr)

kurpar <- list(para=c(0.1,0.1), type="kur");
lmr <- lmomkur(kurpar)
parkur(lmr)

}
\keyword{distribution}
