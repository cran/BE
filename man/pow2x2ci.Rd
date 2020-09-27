\name{pow2x2ci}
\alias{pow2x2ci}
\title{Power using a confidence interval of previous 2x2 study}
\description{
  It calculates power for the bioequivalence test on ratio using a confidence interval of previous 2x2 study.
}
\usage{
pow2x2ci(n1, n2, LL, UL, Alpha = 0.1)
}
\arguments{
  \item{n1}{Subject count of group 1}
  \item{n2}{Subject count of group 2}  
  \item{LL}{Lower limit of the confidence interval of geometric mean ratio (Test/Reference)}
  \item{UL}{Upper limit of the confidence interval of geometric mean ratio (Test/Reference)}
  \item{Alpha}{Alpha level. This means (1 - alpha/2)*100 \% confidence interval is given}
}
\details{
  It calculates power of sample size (n per group) with \code{CV}.
}
\value{
  Returns power [0, 1)
}
\author{Kyun-Seop Bae k@acr.kr}
\examples{
pow2x2ci(12, 13, 0.85, 1.11) 
}
\keyword{Power}
