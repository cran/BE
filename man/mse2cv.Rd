\name{mse2cv}
\alias{mse2cv}
\title{Coefficient of variation (CV) from mean squared error (MSE)}
\description{
It alculates coefficient of variation (CV) from mean squared error (MSE).
}
\usage{
mse2cv(mse)
}
\arguments{
  \item{mse}{Mean square error (MSE) in log scale}
}
\details{
  coefficient of variation (CV) is percent in original scale and mean squared error (MSE) is log scale.
}
\value{
Returns coefficient of variation (CV) in percent (\%).
}
\author{Kyun-Seop Bae <k@acr.kr>}
\examples{
  mse2cv(0.06062462) 
}
