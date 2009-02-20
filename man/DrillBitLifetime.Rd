\name{DrillBitLifetime}
\docType{data}
\alias{DrillBitLifetime}
\title{Lifetime of Drill Bits}
\description{
Hamada (1995, table~9.3) provides a table of lifetime to breakage measured in cycles for drill bits used for producing small holes in printed circuit boards. The data are originally credited to an F. Montmarquet. The data were collected under various control and noise factors to perform reliability assessment to maximize bit reliability with minimization of hole diameter. Smaller holes permit higher density of placed circuitry, and are thus economically attractive. The testing was completed at 3,000 cycles---the right censoring threhold.
}
\usage{
data(DrillBitLifetime)
}
\format{
A data frame with
\describe{
  \item{LIFETIME}{Measured in cycles.}
}
}
\references{
Hamada, M., 1995, Analysis of experiments for reliability improvement and robust reliability: in Balakrishnan, N. (ed.) Recent Advances in Life-Testing and Reliability: Boca Raton, Fla., CRC Press, ISBN~0--8493--8972--0, pp.~155--172.
}
\examples{
data(DrillBitLifetime)
summary(DrillBitLifetime)
}
\keyword{datasets}
