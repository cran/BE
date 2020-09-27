\name{pow2x2mse}
\alias{pow2x2mse}
\title{Power using mean squared error (MSE) of previous 2x2 study}
\description{
  It calculates power for the bioequivalence test on ratio using mean squared error (MSE of previous 2x2 study.
}
\usage{
pow2x2mse(n1, n2, mse, True.R = 1, Alpha = 0.1, ThetaL = 0.8, ThetaU = 1.25)
}
\arguments{
  \item{n1}{Subject count of group 1}
  \item{n2}{Subject count of group 2}  
  \item{mse}{Mean squared error}
  \item{True.R}{True ratio of test/reference}
  \item{Alpha}{Alpha level. This means (1 - alpha/2)*100 \% confidence interval is given}
  \item{ThetaL}{Lower limit of equivalence criteria}
  \item{ThetaU}{Upper limit of equivalence criteria}  
}
\details{
  It calculates power of sample size (n per group) with \code{CV}.
}
\value{
  Returns power [0, 1)
}
\author{Kyun-Seop Bae k@acr.kr}
\examples{
pow2x2mse(12, 13, 0.0756530) 
}
\keyword{Power}
