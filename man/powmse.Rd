\name{powmse}
\alias{powmse}
\title{Power using mean squared error (MSE)}
\description{
It calculates power for the bioequivalence test on ratio using mean squared error (MSE).
}
\usage{
powmse(n, mse, DesignNo = 1, True.R = 1, Alpha = 0.1, ThetaL = 0.8, ThetaU = 1.25)
}
\arguments{
  \item{n}{Sample size, n per group}
  \item{mse}{Mean squared error}
  \item{DesignNo}{Crossover design number.  
\preformatted{  
 Design Number (treatment x sequence x period)
 1 2x2x2 : RT TR
 2 2x4x2 (Balaam Design) : TT RR RT TR 
 3 2x2x3 (Two-sequence Dual Design): TRR RTT 
 4 2x2x4 : TRRT RTTR
 5 2x4x4 : TTRR RRTT TRRT RTTR
 6 3x6x3 (William Design for 3 treatments) + carry-over effect 
         : RBA ARB BAR ABR BRA RAB
 7 3x6x3 (William Design for 3 treatments) - carry-over effect 
         : RBA ARB BAR ABR BRA RAB
 8 4x4x4 (William Design for 4 treatments) + carry-over effect 
         : RCAB ARBC BACR CBRA
 9 4x4x4 (William Design for 4 treatments) - carry-over effect 
         : RCAB ARBC BACR CBRA 
}  
  }
  \item{True.R}{True ratio of test/reference}
  \item{Alpha}{Alpha error level}
  \item{ThetaL}{Lower limit of equivalence criteria}
  \item{ThetaU}{Upper limit of equivalence criteria}
}
\details{
  It calculates power of sample size (n per group) with \code{mse}.
}
\value{
  Returns power [0, 1))
}
\author{Kyun-Seop Bae <k@acr.kr>}
\examples{
powmse(12, 0.06)
}
\keyword{Power}
