\name{ci2mse}
\alias{ci2mse}
\title{Mean squared error (MSE) from a confidence interval of previous 2x2 study}
\description{
It calculates mean squared error (MSE) from a confidence interval of previous 2x2 study.
}
\usage{
ci2mse(n1, n2, LL, UL, Alpha = 0.1)
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
Returns mean squared error (MSE).
}
\author{Kyun-Seop Bae k@acr.kr}
\examples{
ci2mse(12, 13, 0.85, 1.11) 
}
