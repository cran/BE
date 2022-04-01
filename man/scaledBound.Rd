\name{scaledBound}
\alias{scaledBound}
\title{Widened Bound for Scaled Average Bioequivalence}
\description{
It calculates widened bound for scaled average bioequivalence.
}
\usage{
scaledBound(CV = 40, k=0.76, digits=4)
}
\arguments{
  \item{CV}{coefficient of variation in percent}
  \item{k}{0.76. When CV is 30 \%, bound becomes (0.8, 1.25). Most regulartory body does not use a more accurate value.}  
  \item{digits}{Regulatory bodies usually use four decimal rounded values.}
}
\details{
  CV must be larger than 30 \%. If CV is larger than 50%, this uses 50% as CV.
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
