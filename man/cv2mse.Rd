\name{cv2mse}
\alias{cv2mse}
\title{Mean squared error (MSE) from coefficient of variation (CV)}
\description{
It calculates mean squared error (MSE) from coefficient of variation (CV).
}
\usage{
cv2mse(cv)
}
\arguments{
  \item{cv}{Coefficient of variation (\%) in the original scale}
}
\details{
  Coefficient of variation (CV) is percent in original scale and mean squared error (MSE) is log scale.
}
\value{
Returns mean squared error (MSE) in log scale).
}
\author{Kyun-Seop Bae k@acr.kr}
\examples{
cv2mse(25) 
}
