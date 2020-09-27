\name{ci2cv}
\alias{ci2cv}
\title{Coefficient of variation (CV) from a confidence interval of previous 2x2 study}
\description{
It calculates coefficient of variation (CV) from a confidence interval of previous 2x2 study.
}
\usage{
ci2cv(n1, n2, LL, UL, Alpha = 0.1)
}
\arguments{
  \item{n1}{Subject count of group 1}
  \item{n2}{Subject count of group 2}  
  \item{LL}{Lower limit of the confidence interval of geometric mean ratio (Test/Reference)}
  \item{UL}{Upper limit of the confidence interval of geometric mean ratio (Test/Reference)}
  \item{Alpha}{Alpha level. This means (1 - alpha/2)*100 \% confidence interval is given}
}
\details{
  It calculates coefficient of variation (CV) from a confidence interval of 2x2 bioequivalence study.
}
\value{
Returns coefficient of variation (CV) in percent (\%).
}
\author{Kyun-Seop Bae k@acr.kr}
\examples{
ci2cv(12, 13, 0.85, 1.11) 
}
