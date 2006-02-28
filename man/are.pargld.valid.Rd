\name{are.pargld.valid}
\alias{are.pargld.valid}
\title{Are the Distribution Parameters Consistent with the Generalized Lambda Distribution}
\description{
The distribution parameter object returned by functions of this package such as by
\code{vec2par} are consistent with the corresponding distribution, 
otherwise a list would not have been returned.  However, other
functions (\code{quagld}) require consistent parameters to ensure that the
Generalized Lambda Distribution is monotonic increasing on \eqn{0 \le F \le 1},
in which \eqn{F} is nonexceedance probability.
}
\usage{
are.pargld.valid(para,verbose=FALSE)
}
\arguments{
  \item{para}{A distribution parameter list returned by \code{vec2par}.}
  \item{verbose}{A logical switch on additional output to the user--default is \code{FALSE}.}
}
\details{
Karian and Dudewicz (2000) outline valid parameter space of the Generalized Lambda distribution. First, according to Theorem 1.3.3 the distribution is valid if and only if

\deqn{\Lambda_2(\Lambda_3 F^{\lambda_3 - 1} + \Lambda_4(1-F)^{\Lambda_4 -1 }) \ge 0 \mbox{.}}

for all \eqn{F \in [0,1]}. The \code{are.pargld.valid} function tests against this condition by incrementing through \eqn{[0,1]} by \eqn{dF = 0.0001}. This is a brute force method of course. Further, Karian and Dudewicz (2002) provide a diagrammatic representation of regions in \eqn{\Lambda_3} and \eqn{\Lambda_4} space for suitable \eqn{\Lambda_2} in which the distribution is valid. The \code{are.pargld.valid} function subsequently checks against the 6 valid regions as a secondary check on Theorem 1.3.3. The regions of the distribution are defined by

\deqn{\mbox{Region 1:  } \Lambda_3 \le -1 \mbox{ and } \Lambda_4 \ge 1 \mbox{,}}
\deqn{\mbox{Region 2:  } \Lambda_3 \ge 1 \mbox{ and } \Lambda_4 \le -1 \mbox{,}}
\deqn{\mbox{Region 3:  } \Lambda_3 \ge 0 \mbox{ and } \Lambda_4 \ge 0 \mbox{,}}
\deqn{\mbox{Region 4:  } \Lambda_3 \le 0 \mbox{ and } \Lambda_4 \le 0 \mbox{,}}
\deqn{\mbox{Region 5:  } \Lambda_4 \ge (-1/\Lambda_3) \mbox{ and } -1 \ge \Lambda_3 \le 0 \mbox{, and}}
\deqn{\mbox{Region 6:  } \Lambda_4 \le (-1/\Lambda_3) \mbox{ and } \Lambda_4 \ge -1 \mbox{ and } \Lambda_3 \ge 1 \mbox{.}}

}
\value{
  \item{TRUE}{If the parameters are \code{gld} consistent.}
  
  \item{FALSE}{If the parameters are not \code{gld} consistent.}
}
\references{
Karian, Z.A., and Dudewicz, E.J., 2000, Fitting statistical distributions--The generalized lambda distribution and generalized bootstrap methods: 
CRC Press, Boca Raton, FL, 438 p.
}
\note{
This function calls \code{is.gld} to verify consistency between 
the distribution parameter object and the intent of the user.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{is.gld}} }
\examples{
para <- vec2par(c(123,34,4,3),type='gld')
if(are.pargld.valid(para)) Q <- quagld(0.5,para)

}
\keyword{distribution}
