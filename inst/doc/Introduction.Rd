\name{Introduction}
\title{Introduction to R library lmoments}
\description{
This manual documents the R-software package \pkg{lmomco}. The \pkg{lmomco} package implements the \bold{statistical theory of L-moments} including L-moment estimation (\code{\link{lmom.ub}}, \code{\link{lmom2pwm}}), Probability-Weighted Moment estimation (\code{\link{pwm.ub}}, \code{\link{pwm.pp}}, \code{\link{pwm.gev}}), parameter estimation for numerous familiar and not-so-familiar distributions  (see following paragraph), and L-moment estimation for the same distributions from the parameters (\code{\link{lmom2par}}). In words, L-moments are derived from the expectations of order statistics and are linear with respect to the probability-weighted moments. The linearity between L-moments and Probability-Weighted Moments means that procedures base on one are equivalent to the other. L-moments are directly analogous to the well-known product moments; however, L-moments have many advantages including unbiasedness, robustness, and consistency with respect to the conventional product (central) moments (mean, standard deviation, skew, kurtosis, \dots). L-moment have particular internal relations to themselves and boundness (see \code{\link{are.lmom.valid}}). This package is oriented around the FORTRAN algorithms of J.R.M. Hosking, and the nomenclature for many of the functions parallels that of the Hosking library. Extensions are made. Additionally, recent developments by Robert Serfling and Peng Xiao have extended L-moments into multivariate space--\bold{the L-comoments}. The sample L-comoments (\code{\link{Lcomoment.Lk12}}) are implemented here for an unlimited number of random variables and moment order value. The L-comoments are considered experimental, but the diagonal of the L-comoment matrix (\code{\link{Lcomoment.matrix}}) produces conventional L-moments (\code{\link{lmom.ub}}) of the corresponding order.

At present (2006), 11 distributions (all univariate) are supported for parameter estimation using L-moments (\code{parCCC}, such as \code{\link{parexp}}), L-moment estimation using parameters (\code{lmomCCC}, such as \code{\link{lmomexp}}), cumulative distribution function (nonexceedance probability as a function of the variable), and quantile distribution function (variable as a function of nonexceedance probability). A dispatcher for parameter estimation from the L-moments is \code{\link{lmom2par}}. A dispatcher for L-moment estimation from the parameters is \code{\link{par2lmom}}. The cumulative distribution functions are \code{cdfCCC}, such as \code{\link{cdfexp}}; a dispatcher to the cumulative distribution functions is \code{\link{par2cdf}}. The quantile functions are \code{quaCCC}, such as \code{\link{quaexp}}; a dispatcher to the quantile functions is \code{\link{par2qua}}. The distributions supported are the Exponential, Gamma, Generalized Extreme Value, Generalized Logistic, Generalized Normal, Generalized Pareto, Gumbel, Kappa, Normal, Pearson Type III, and Wakeby. Some of these distributions (Exponential, Gamma, and Normal) have functional implementation within the standard R-package distribution. However, as this package in part mirrors existing FORTRAN libraries in wide spread use by the L-moment user-community (environmental and hydrologic sciences--at least those familiar to the author), these three distributions are implemented here in a parallel function context. It is important to note that R functions are used for these three distributions. Additional univariate distributions that are implemented are: Cauchy (quantile and cumulative distributions functions only) and Generalized Lambda (quantile function only); the L-moments to parameters and parameters to L-moments are not yet implemented. The 13 distributions are referred to by a three-character syntax (denoted as \code{CCC} in the documentation):

   \code{cau} = Cauchy distribution (two parameters)--only \code{quacau} and \code{cdfcau} are implemented.

   \code{exp} = Exponential distribution (two parameters)

   \code{gam} = Gamma distribution (two parameters)

   \code{gev} = Generalized Extreme Value distribution (three parameters)

   \code{gld} = Generalized Lambda distribution (four parameters)--only \code{quagld} is implemented.

   \code{glo} = Generalized Logistic distribution (three parameters)

   \code{gno} = Generalized Normal (log-Normal) distribution (three parameters)

   \code{gpa} = Generalized Pareto distribution (three parameters)

   \code{gum} = Gumbel distribution (two parameters)

   \code{kap} = Kappa distribution (four parameters)

   \code{nor} = Normal distribution (two parameters)

   \code{pe3} = Pearson Type III distribution (three parameters)

   \code{wak} = Wakeby distribution (five parameters)

Parameters for the distribution are placed into a particular object format (see \code{\link{vec2par}}, \code{\link{lmom2par}}, or \code{\link{parexp}}). The parameter object (simply an R \code{list}) in turn can be passed as an argument to the distribution functions. A broader intent of this package is to support modular code design when users are heavily involved in distributional analysis. Therefore, this package contains a number of ancillary functions such as \code{\link{are.par.valid}} or \code{is.CCC}, where \code{CCC} is the three character syntax as previously shown to assist users in building sophisticated tools in R.

This package also supports the construction of L-moment ratio diagrams (\code{\link{lmrdia}} and \code{\link{plotlmrdia}})--namely, the construction of the L-skew and L-kurtosis diagram. On the diagram and the theoretical trajectories of most of the aforementioned distributions. These diagrams are difficult to explain here, but are well documented in the literature (\code{\link{lmom.references}}).

Several other functions are available and might be useful in testing or other circumstances. The \code{\link{lmom.diff}} function computes the difference between the L-moments derived from a parameterized distribution and the L-moments as computed from the data. From the author's experience, construction of \dQuote{magnitude and frequency curves} (variable as function of nonexceedance probability) is commonly required. Therefore, \code{freq.curve.CCC} are available for ease of use. There also is a \code{\link{freq.curve.all}} function that computes the frequency curve for all the distributions given an L-moment object (not inclusive of the \code{gld} distribution). The curves require vectors of nonexceedance probabilities (see \code{\link{nonexceeds}}). Related to nonexceedance probabilities, note that for this documentation nonexceedance probability is shown as \eqn{0 \le F \le 1}; however, some distributions might not be valid at \eqn{F = 0} or \eqn{F = 1}. Finally, the functions \code{\link{lmom.test.all}}, which dispatches to distribution-specific functions following the \code{lmom.test.CCC} naming convention and provides user-level output to help evaluate the algorithms of this package.

The examples below demonstrate application of the package for the analysis of a sample. The L-moments of the data are computed. In turn, the Kappa and Normal distributions are each fit to the L-moments. The frequency curve (quantile as function of nonexceedance probability) for each distribution is plotted. The examples conclude with the computation of the 2nd-order L-comoment matrix of two nonindependent samples.

}
\examples{
  # One has the following peak streamflow values in cubic meters per second
  data <- c(123,2250,543,178,67,5100,248,1500,342,329,543,980,1020,4502,3406,856,297)
  # Compute the unbiased L-moments of the data--high L-skew.
  # This data is clearly not Normally distributed.
  lmr  <- lmom.ub(data)
  # One method of parameter estimation for a Kappa distribution
  Kappapar  <- lmom2par(lmr,type='kap')
  # Another method of parameter estimation for Normal distribution
  Normalpar <- parnor(lmr)
  # Vector of useful nonexceedance probabilities
  F  <- nonexceeds()
  # Generate Kappa frequency curve
  Qk <- freq.curve.kap(nonexceeds(),Kappapar)
  # Generate Normal frequeny curve
  Qn <- freq.curve.nor(nonexceeds(),Normalpar)
  # Plot them up
  plot(F,Qk,type="n",ylim=c(-6000,24000))
  lines(F,Qk)
  lines(F,Qn,col=2)
  
  X1 <- data
  # Generate some related data 
  X2 <- abs(rnorm(length(data)))*data
  L2 <- Lcomoment.matrix(data.frame(RandomVariable1=X1,AnotherRandomVariable=X2),k=2)
  L2
  # Compute the convential L-moments of variable 1 and 2
  X1lmr <- lmom.ub(X1)
  X2lmr <- lmom.ub(X2)
  # Show that the diagonal of the Lcomoment matrix equals the
  # conventional moments of same order (2nd order in this case).
  print(c(X1lmr$L2,L2$matrix[1,1]))
  print(c(X2lmr$L2,L2$matrix[2,2]))
  # Compute the L-correlation values
  Lrho <- Lcomoment.correlation(L2)
  Lrho
  # Compare the off-diagonal terms to the conventional
  # correlation coefficient. The off-diagonal terms will
  # not be equal or equal in value to the conventional
  # correlation coefficient.
  cor(X1,X2)
}
\author{ W.H. Asquith}
\keyword{distribution}
