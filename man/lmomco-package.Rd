\name{lmomco-package}
\docType{package}
\alias{lmomco-package}
\title{L-moments, Censored L-moments, Trimmed L-moments, L-comoments, and Many Distributions}
\description{
   The \pkg{lmomco} package is a comparatively comprehensive implementation of L-moments in addition to probability-weighted moments, and parameter estimation for numerous familiar and not-so-familiar distributions. L-moments and their cousins are based on certain linear combinations of order statistic expectations. Being based on linear mathematics and thus especially robust compared to conventional moments, they are particular suitable for analysis of rare events of non-Normal data. L-moments are consistent and often have smaller sampling variances than maximum likelihood in small to moderate sample sizes. L-moments are especially useful in the context of quantile functions.

Over 330 user-level functions are implemented in \pkg{lmomco} that range from low-level utilities forming an application programming interface (API) to high-level sophisticated data analysis and visualization operators. The \dQuote{See Also} section lists recommended function entry points for new users. The nomenclature (\code{d}, \code{p}, \code{r}, \code{q})-\code{lmomco} is directly analogous to that for distributions built-in to \R.

Lastly, the \R packages \pkg{lmom} (Hosking), \pkg{lmomRFA} (Hosking), \pkg{Lmoments} (Karvanen) might also be of great interest.
}
\references{
Asquith, W.H., 2007, L-moments and TL-moments of the generalized lambda distribution: Computational Statistics and Data Analysis, v. 51, no. 9, pp. 4484--4496, \url{http://dx.doi.org/10.1016/j.csda.2006.07.016}

Asquith, W.H., 2011, Distributional analysis with L-moment statistics using the R environment for statistical computing: Createspace Independent Publishing Platform, ISBN 978--146350841--8, \url{http://www.amazon.com/dp/1463508417/}

Asquith, W.H., 2014, Parameter estimation for the 4-parameter asymmetric exponential power distribution by the method of L-moments using R: Computational Statistics and Data Analysis, v. 71, pp. 955--970, \url{http://dx.doi.org/10.1016/j.csda.2012.12.013}

Elamir, E.A.H., and Seheult, A.H., 2003, Trimmed L-moments: Computational statistics and data analysis, vol. 43, pp. 299-314,  \url{http://dx.doi.org/10.1016/S0167-9473(02)00250-5}

Hosking, J.R.M., 1990, L-moments---Analysis and estimation of
distributions using linear combinations of order statistics: Journal of the Royal Statistical Society, Series B, v. 52, pp. 105--124, \url{http://www.jstor.org/stable/2345653}

Hosking, J.R.M., and Wallis, J.R., 1997, Regional frequency analysis---An approach based on L-moments: Cambridge University Press, \url{http://www.amazon.com/dp/0521019400/}

Nair, N.U., Sankaran, P.G., and Balakrishnan, N., 2013, Quantile-based reliability analysis: Springer, New York, \url{http://www.amazon.com/dp/0817683607/}

Serfling, R., and Xiao, P., 2007, A contribution to multivariate L-moments---L-comoment matrices: Journal of Multivariate Analysis, v. 98, pp. 1765--1781, \url{http://dx.doi.org/10.1016/j.jmva.2007.01.008}
}
\author{William Asquith \email{william.asquith@ttu.edu} }
\seealso{
   \code{\link{lmoms}}, \code{\link{dlmomco}}, \code{\link{plmomco}}, \code{\link{rlmomco}}, \code{\link{qlmomco}}, \code{\link{lmom2par}},
\code{\link{plotlmrdia}}, \code{\link{lcomoms2}}
}
\keyword{distribution}
\keyword{mixture}
\keyword{L-moments}
\concept{Lmoments}
\concept{linear moments}
\concept{L moments}
