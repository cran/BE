\name{scaledBound}
\alias{scaledBound}
\title{Widened Bound for Scaled Average Bioequivalence}
\description{
It calculates widened bound for scaled average bioequivalence.
}
\usage{
scaledBound(CV = 40, k = 0.76, digits = 4)
}
\arguments{
  \item{CV}{coefficient of variation in percent}
  \item{k}{0.76 is for EMA and Korea MFDS. US FDA uses 0.893. When CV is 30\%, bound becomes (0.8, 1.25). Most regulartory body does not use a more accurate value.}  
  \item{digits}{Korea MFDS use 4 digits only, while other regulatory bodies use more decimal values.}
}
\details{
  CV must be larger than 30\%. If CV is larger than 50%, this uses 50% as CV. US FDA uses this widened bound for both AUClast and Cmax, while EU EMA and Korea MFDA use this for Cmax only.
}
\value{
widened bound for scaled average bioequivalence
}
\author{Kyun-Seop Bae <k@acr.kr>}
\examples{
scaledBound(30)
scaledBound(35)
scaledBound(40)
scaledBound(45)
scaledBound(50)
}
