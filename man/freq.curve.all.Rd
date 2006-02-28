\name{freq.curve.all}
\alias{freq.curve.all}
\title{ Compute Frequency Curve for All Distributions}
\description{
This function is dispatcher on top of the suite of \code{freq.curve.CCC}
functions that compute frequency curves for the L-moments. Frequency curves in hydrologic science is a term typically renaming the more conventional quantile function. The notation
\code{CCC} represents the three character notation for the distribution: 
\code{exp}, \code{gam}, \code{gev}, \code{gld}, \code{glo}, \code{gno}, \code{gpa},
\code{gum}, \code{kap}, \code{nor}, \code{pe3}, and \code{wak}. The Cauchy distribution
is not called because of its dependency on trimmed L-moments and its general lack of use in applied
research problems (at least those familiar to the author).
}
\usage{
freq.curve.all(lmom,exp=TRUE,gam=TRUE,gev=TRUE,gld=TRUE,glo=TRUE,
              gno=TRUE,gpa=TRUE,gum=TRUE,kap=TRUE,nor=TRUE,pe3=TRUE,wak=TRUE,...)
}
\arguments{
  \item{lmom}{A L-moment object from \code{lmom.ub} or similar.}
  \item{exp}{A logical switch on computation of corresponding distribution--Default is \code{TRUE}.} 
  \item{gam}{A logical switch on computation of corresponding distribution--Default is \code{TRUE}.} 
  \item{gev}{A logical switch on computation of corresponding distribution--Default is \code{TRUE}.} 
  \item{gld}{A logical switch on computation of corresponding distribution--Default is \code{TRUE}.} 
  \item{glo}{A logical switch on computation of corresponding distribution--Default is \code{TRUE}.} 
  \item{gno}{A logical switch on computation of corresponding distribution--Default is \code{TRUE}.} 
  \item{gpa}{A logical switch on computation of corresponding distribution--Default is \code{TRUE}.} 
  \item{gum}{A logical switch on computation of corresponding distribution--Default is \code{TRUE}.} 
  \item{kap}{A logical switch on computation of corresponding distribution--Default is \code{TRUE}.} 
  \item{nor}{A logical switch on computation of corresponding distribution--Default is \code{TRUE}.} 
  \item{pe3}{A logical switch on computation of corresponding distribution--Default is \code{TRUE}.} 
  \item{wak}{A logical switch on computation of corresponding distribution--Default is \code{TRUE}.} 
  \item{...}{Additional parameters are passed to the parameter estimation routines such as \code{parexp}.}
}
\value{
  An extensive R \code{list} of frequency curves.
}
\author{ W.H. Asquith}
\seealso{
\code{\link{freq.curve.exp}}, 
\code{\link{freq.curve.gam}}, 
\code{\link{freq.curve.gev}}, 
\code{\link{freq.curve.gld}}, \cr
\code{\link{freq.curve.glo}}, 
\code{\link{freq.curve.gno}}, 
\code{\link{freq.curve.gpa}}, 
\code{\link{freq.curve.gum}}, \cr
\code{\link{freq.curve.kap}}, 
\code{\link{freq.curve.nor}},
\code{\link{freq.curve.pe3}}, and 
\code{\link{freq.curve.wak}}

}
\keyword{distribution}
