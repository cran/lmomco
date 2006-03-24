\name{Introduction}
\title{Introduction to R library lmoments}
\description{
This manual documents the R-software package \pkg{lmomco}. The \pkg{lmomco} package implements the \bold{statistical theory of L-moments} including L-moment estimation (\code{\link{lmom.ub}}, \code{\link{lmom2pwm}}), \cr Probability-Weighted Moment estimation (\code{\link{pwm.ub}}, \code{\link{pwm.pp}}, \code{\link{pwm.gev}}), parameter estimation for numerous familiar and not-so-familiar distributions (see section \bold{Distributions}), and L-moment estimation for the same distributions from the parameters (\code{\link{lmom2par}}). In words, L-moments are derived from the expectations of order statistics and are linear with respect to the probability-weighted moments. L-moments are directly analogous to the well-known product moments; however, L-moments have many advantages including unbiasedness, robustness, and consistency with respect to the conventional product (central) moments (mean, standard deviation, skew, kurtosis, \dots). L-moments have particular internal relations to themselves and boundness (see \cr \code{\link{are.lmom.valid}}). In general, this package is oriented around the FORTRAN algorithms of J.R.M. Hosking, and the nomenclature for many of the functions parallels that of the Hosking library. Extensions are made. Additionally, recent developments by Robert Serfling and Peng Xiao have extended L-moments into multivariate space---\bold{the L-comoments}. The sample L-comoments (\code{\link{Lcomoment.Lk12}}) are implemented here for an arbitrary number of random variables and moment order value. The L-comoments are considered experimental in this package; however, for algorithm evaluation the diagonal of the L-comoment matrix (\code{\link{Lcomoment.matrix}}) produces conventional L-moments (\code{\link{lmom.ub}}) of the corresponding order. A complimentary addition to L-moment theory are the \bold{trimmed L-moments} by Elsayed Elamir and Allan Seheult. The sample trimmed L-moments are supported by \code{\link{TLmoms}} and are useful tools for parameter estimation for some distributions including the Cauchy; the trimmed L-moments have further robust properties compared to the usual or ordinary L-moments.
}

\section{L-moments}{

The theoretical L-moments for a random variable \eqn{X} having a quantle function, \eqn{X(F)}, for nonexceedance probability \eqn{F} are defined from the expectations of order statistics. The order statistics for \eqn{X} of a sample of size \eqn{n} are formed by the ascending order \eqn{X_{1:n} \le X_{2:n} \le \ldots \le X_{n:n}}. The L-moments are defined as

 \deqn{\lambda_r \equiv \frac{1}{r}\sum^{r-1}_{k=0} (-1)^k { r-1 \choose k } E[X_{r-k:r}]}

where \eqn{r \ge 1} is the order of the L-moment, and \eqn{E[X_{r-k:r}]} is the expectation of the \eqn{r-k} order statisic of a sample of size \eqn{r}. The expectation of an order statistic is 

 \deqn{E[X_{j:r}] = \frac{r!}{(j-1)!(r-j)!}\int^1_0 X(F)\times F^{j-1}(1-F)^{r-j} \,\mathrm{d}F \mbox{.}}
 
The theoretical L-moments for the supported distributions are computing from the above equations using numerical integration and the \code{theoLmom} function. However, each distribution has is own system of L-moment equations; these are used for parameter estimation by this package. The first four L-moments in terms of integrals are

 \deqn{\lambda_1 = \int^1_0 X(F) \,\mathrm{d}F \mbox{,}}
 \deqn{\lambda_2 = \int^1_0 X(F) \times (2F - 1) \,\mathrm{d}F \mbox{,}}
 \deqn{\lambda_3 = \int^1_0 X(F) \times (6F^2 - 6F + 1) \,\mathrm{d}F \mbox{, and}}
 \deqn{\lambda_4 = \int^1_0 X(F) \times (20F^3 - 30F^2 + 12F - 1) \,\mathrm{d}F \mbox{.}}

The L-moment ratios are the dimensionless quantities

 \deqn{ \tau = \mbox{LCV} = \lambda_2/\lambda_1 = \mbox{coefficient of L-variation, }}
 \deqn{ \tau_3 = \mbox{TAU3} = \lambda_3/\lambda_2 = \mbox{L-skew, }}
 \deqn{ \tau_4 = \mbox{TAU4} = \lambda_4/\lambda_2 = \mbox{L-kurtosis, and}}
 \deqn{ \tau_5 = \mbox{TAU5} = \lambda_5/\lambda_2 = \mbox{not named.}}
}

\section{Sample L-moments}{
The sample L-moments are computed through the probability-weighted moments (see \cr \code{\link{pwm2lmom}} or directly. In either case, the L-moments are computed from the sample order statistics \eqn{x_{1:n} \le x_{2:n} \le \dots x_{n:n}}. In the most compact notation, the sample L-moments are

\deqn{\hat{\lambda}_r = \frac{1}{n}\sum^n_{i=1}w^{(r)}_{i:n} x_{i:n} \mbox{,}}

where \eqn{w^{(r)}_{i:n}} are weight factors. The weight factors are well illustrated in figure 2.6 of Hosking and Wallis (1997).

 \deqn{ w^{(r)}_{i:n} = \sum_{j=0}^{min\{i-1,r-1\}} (-1)^{r-1-j}
                                    \frac{{r-1 \choose j}{r-1+j \choose j}{i-1 \choose j}}
				         {{n-1 \choose j}}
      \mbox{.}}
}

\section{TL-moments}{

The trimmed L-moments (TL-moments) have further robustness properties over the ordinary L-moments. TL-moments are computed from \dQuote{conceptual} samples some trim level larger than the actual sample. Symmetrical trimming is shown here, but asymmetrical trimming is supported (\code{theoTLmoms}). The \eqn{r \ge 1} TL-moment is defined for trim level \eqn{t \ge 0} as follows

 \deqn{\lambda^{(t)}_r = \frac{1}{r} \sum^{r-1}_{k=0} \frac{(-1)^k {r-1 \choose k}(r+2t)!}{(r+t-k-1)!(t+k)!} \int^1_0 X(F) \times F^{r+t-k-1}(1-F)^{t+k} \,\mathrm{d}F \mbox{.}}

It is important to note that when \eqn{t=0} the ordinary \eqn{r}th L-moment is formed. The first four theoretical TL-moments for \eqn{t=1} are

 \deqn{\lambda^{(1)}_1 = 6 \int^1_0 X(F) \times F \times (1-F) \,\mathrm{d}F \mbox{,}}
 \deqn{\lambda^{(1)}_2 = 6 \int^1_0 X(F)  \times F \times (1-F)(2F - 1) \,\mathrm{d}F \mbox{,}}
 \deqn{\lambda^{(1)}_3 = \frac{20}{3} \int^1_0 X(F) \times F \times (1-F)(5F^2 - 5F + 1) \,\mathrm{d}F \mbox{,}}
 \deqn{\lambda^{(1)}_4 = \frac{15}{2} \int^1_0 X(F) \times F \times (1-F)(14F^3 - 21F^2 + 9F - 1) \,\mathrm{d}F \mbox{,}}
}

\section{Sample TL-moments}{

The sample symmetrical TL-moments are computed as

\deqn{ \hat{\lambda}^{(t)}_r = \frac{1}{r}\sum^{n-t}_{i=t+1}
           \left[ \frac{\sum\limits^{r-1}_{k=0}{ (-1)^k {r-1 \choose k}
                                                 {i-1 \choose r+t-1-k}
                                                 {n-i \choose t+k}
                                         }}{{n \choose r+2t}}
           \right] x_{i:n}} 

where \eqn{t} represents the trimming level of the \eqn{t}-largest of \eqn{t}-smallest values, \eqn{r} represents the order of the L-moments, \eqn{n} represents the sample size, and \eqn{x_{i:n}} is the \eqn{i}th sample order statistics (\eqn{x_{1:n} \le x_{2:n} \le \dots x_{n:n}}). Sample asymmetrical TL-moments are supported by the \code{TLmoms} function.
}


\section{L-comoments}{

The L-comoments are extensions of the ordinary L-moments into multivariate space. The theoretical L-comoments of random variable \eqn{X1} are computed from the concomitants of \eqn{X2}. That is, \eqn{X2} is sorted in ascending order to create the order statistics of \eqn{X2}; \eqn{X1} is in turn reshuffled to the order of \eqn{X2} for form the concomitants of \eqn{X2} (denoted as \eqn{X^{(12)}}). The \eqn{k}th L-comoment of \eqn{X1} with respect to \eqn{X2} is

 \deqn{\lambda_{[12]} = \frac{1}{k} \sum^{k-1}_{j=0} (-1)^k {k-1 \choose j} E[X^{(12)}_{[k-j:k]}] \mbox{,}}
 
where \eqn{E[X^{(12)}_{[k-j:k]}]} is the expected value of the \eqn{X_{[k-j:n]}} concomitant of \eqn{X2} with respect to \eqn{X1}.

 \deqn{E[X^{(12)}_{[k-j:k]}] = r {n \choose r} \int^1_0 \int^1_0
                           X(F_1) \times 
			   (F_2)^{r-1}(1-F_2)^{n-r} \,\mathrm{d}F_1 \,\mathrm{d}F_2 \mbox{,}
      }

where \eqn{X(F_1)} is quantile function of random variable \eqn{X1} and \eqn{F_2} is nonexceedance probability of random variable \eqn{X2}.

The sample L-comoment (\code{Lcomoment.Lk12}) of random variable \eqn{X1} are computed from the concomitants of \eqn{X2}. The concomitants inturn are used in a weighted summation and expectation calculation to compute the L-comoment of \eqn{X1} with respect to \eqn{X2}.

 \deqn{ \hat{\lambda}_{k[12]} = n^{-1}\sum_{r=1}^{n}
                                     w^{(k)}_{r:n} x^{(12)}_{[r:n]}
      \mbox{,}}

where \eqn{x^{(12)}_{[r:n]}} are the sample concomitant statistics and \eqn{w^{(k)}_{r:n}} is

 \deqn{ w^{(k)}_{r:n} = \sum_{j=0}^{min\{r-1,k-1\}} (-1)^{k-1-j}
                                    \frac{{k-1 \choose j}{r-1+j \choose j}{r-1 \choose j}}
				         {{n-1 \choose j}}
      \mbox{,}}

The sample L-comoment matrix (\code{Lcomoment.matrix}) for \eqn{d}-random variables is

  \deqn{ \mbox{\boldmath $\Lambda_k$} = (\hat{\lambda}_{k[ij]})
       }

computed over the pairs (\eqn{X^{(i)},X^{(j)}}) where \eqn{1 \le i \le j \le d}.

}

\section{Distributions}{
The 13 univariate distributions supported by this package are referred to by a character syntax (denoted as \code{CCC} in the documentation). At present (2006), 12 distributions are supported by stable algorithms for parameter estimation using L-moments (\code{parCCC}, such as \code{\link{parexp}}), L-moment estimation using parameters (\code{lmomCCC}, such as \code{\link{lmomexp}}), cumulative distribution function (nonexceedance probability as a function of the variable), and quantile distribution function (variable as a function of nonexceedance probability). A dispatcher for parameter estimation from the L-moments is \code{\link{lmom2par}}. A dispatcher for L-moment estimation from the parameters is \code{\link{par2lmom}}. The cumulative distribution functions are \code{cdfCCC}, such as \code{\link{cdfexp}}; a dispatcher to the cumulative distribution functions is \code{\link{par2cdf}}. The quantile functions are \code{quaCCC}, such as \code{\link{quaexp}}; a dispatcher to the quantile functions is \code{\link{par2qua}}. For TL-moments, a \code{TL} is prepended to \code{CCC}.

The distributions supported are the Cauchy, Exponential, Gamma, Generalized Extreme Value, Generalized Lambda, Generalized Logistic, Generalized Normal, Generalized Pareto, Gumbel, Kappa, Normal, Pearson Type III, and Wakeby. Some of these distributions (Exponential, Gamma, and Normal) have functional implementation within the standard R-package distribution. As this package in part mirrors the Hosking FORTRAN libraries in wide spread use by the L-moment user-community (in particular environmental and hydrologic sciences---at least those familiar to the author), these three distributions are implemented here in a parallel function context. However, it is important to note that R functions are used for these three \dQuote{R-implemented} distributions. A summary of the implemented distributions follows:

   \code{cau} = Cauchy distribution (two parameters)---TL-moment implementation

   \code{exp} = Exponential distribution (two parameters)---ordinary L-moment implementation

   \code{gam} = Gamma distribution (two parameters)---ordinary L-moment implementation

   \code{gev} = Generalized Extreme Value distribution (three parameters)---ordinary L-moment implementation

   \code{gld} = Generalized Lambda distribution (four parameters)---ordinary L-moment implementation.

   \code{TLgld} = Generalized Lambda distribution (four parameters)---TL-moment implementation.

   \code{glo} = Generalized Logistic distribution (three parameters)---ordinary L-moment implementation

   \code{gno} = Generalized Normal (log-Normal) distribution (three parameters)---ordinary L-moment implementation

   \code{gpa} = Generalized Pareto distribution (three parameters)---ordinary L-moment implementation

   \code{TLgpa} = Generalized Pareto distribution (three parameters)---TL-moment implementation

   \code{gum} = Gumbel distribution (two parameters)---ordinary L-moment implementation
   
   \code{kap} = Kappa distribution (four parameters)---ordinary L-moment implementation

   \code{nor} = Normal distribution (two parameters)---ordinary L-moment implementation

   \code{pe3} = Pearson Type III distribution (three parameters)---ordinary L-moment implementation

   \code{wak} = Wakeby distribution (five parameters)---ordinary L-moment implementation

Parameters for the distribution are placed into a particular object format (see \code{\link{vec2par}}, \code{\link{lmom2par}}, or \code{\link{parexp}}). The parameter object (simply an R \code{list}) in turn can be passed as an argument to the distribution functions. A broader intent of this package is to support modular code design when users are heavily involved in distributional analysis. Therefore, this package contains a number of ancillary functions such as \code{\link{are.par.valid}} or \code{is.CCC}, where \code{CCC} is the three character syntax as previously shown to assist users in building sophisticated tools in R.
}

\section{L-moment Ratio Diagrams}{
This package also supports the construction of L-moment ratio diagrams (\code{\link{lmrdia}} and \cr \code{\link{plotlmrdia}})---namely, the construction of the L-skew and L-kurtosis diagram. On the diagram the theoretical trajectories of most of the aforementioned distributions are shown. These diagrams are difficult to explain here, but are well documented in the literature (\code{\link{lmom.references}}).
}

\section{Algorithm Verification}{
Several other functions are available and might be useful in testing or other circumstances. The \code{\link{lmom.diff}} function computes the difference between the L-moments derived from a parameterized distribution and the L-moments as computed from the data. From the author's experience, construction of \dQuote{magnitude and frequency curves} (variable as function of nonexceedance probability) is commonly required. Therefore, \code{freq.curve.CCC} are available for ease of use. There also is a \code{\link{freq.curve.all}} function that computes the frequency curve for all the distributions given an L-moment object. The curves require vectors of nonexceedance probabilities (see \code{\link{nonexceeds}}). Related to nonexceedance probabilities, note that for this documentation nonexceedance probability is shown as \eqn{0 \le F \le 1}; however, some distributions might not be valid at \eqn{F = 0} or \eqn{F = 1}. Finally, the functions \code{\link{lmom.test.all}}, which dispatches to distribution-specific functions following the \code{lmom.test.CCC} naming convention and provides user-level output to help evaluate the algorithms of this package.
}
\section{ Package Warranty }{
The \pkg{lmomco} package was developed for research purposes only and is intended for research applications. The author of this package makes no claim whatsoever about the accuracy, precision, suitability, reliability, editability, or usability of this product inclusive of source code and documentation. If you can use it, you are in luck, but your mileage will vary.
}
\section{Illustrative Examples}{
The examples below demonstrate application of the package for the analysis of a sample. The L-moments of the data are computed. In turn, the Kappa and Normal distributions are each fit to the L-moments. The frequency curve (quantile as function of nonexceedance probability) for each distribution is plotted. The examples conclude with the computation of the 2nd-order L-comoment matrix of two nonindependent samples.
}

\examples{
  # One has the following peak streamflow values in cubic meters per second
  data <- c(123,2250,543,178,67,5100,248,1500,342,329,
            543,980,1020,4502,3406,856,297)
  # Compute the unbiased L-moments of the data--high L-skew.
  # This data clearly is not Normally distributed.
  lmr  <- lmom.ub(data)
  # One style of parameter estimation for a Kappa distribution
  Kappapar  <- lmom2par(lmr,type='kap')
  # Another style of parameter estimation for Normal distribution
  Normalpar <- parnor(lmr)
  # Vector of useful nonexceedance probabilities
  F  <- nonexceeds()
  # Generate Kappa frequency curve
  Qk <- freq.curve.kap(nonexceeds(),Kappapar)
  # Generate Normal frequency curve
  Qn <- freq.curve.nor(nonexceeds(),Normalpar)
  # Plot them up
  plot(F,Qk,type="n",ylim=c(-6000,24000))
  lines(F,Qk)
  lines(F,Qn,col=2)
  
  X1 <- data
  # Generate some related data 
  X2 <- abs(rnorm(length(data)))*data
  L2 <- Lcomoment.matrix(data.frame(RanVar=X1,AnotherRanVar=X2),k=2)
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
}
\author{ W.H. Asquith}
\keyword{distribution}
