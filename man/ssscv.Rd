\name{ssscv}
\alias{ssscv}
\title{Sample Size for Scaled Average BE using coefficient of variation (CV)}
\description{
  It calculates sample size for the scaled average bioequivalence test on ratio using coefficient of variation (CV).
}
\usage{
ssscv(CV, DesignNo = 1, True.R = 1, Alpha = 0.1, Beta = 0.2, Region = "EU", nMax = 999999)
}
\arguments{
  \item{CV}{Coefficient of Variation (\%)}
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
  \item{Beta}{Beta error level}
  \item{Region}{US or FDA for US FDA, KR or MFDS for Korea MFDS, EU or EMA for other regions or countries}
  \item{nMax}{Maximum subject number (sample size) per group}
}
\details{
  It calculates sample size (n per group) with \code{CV}, \code{Alpha}, and \code{Beta} for scaled average bioequivalence test. US FDA uses this widened bound for both AUClast and Cmax, while EU EMA and Korea MFDA use this for Cmax only.
}
\value{
  Returns sample size (n per group) for scaled average bioequivalence test with ratio criteria.
}
\author{Kyun-Seop Bae <k@acr.kr>}
\examples{
ssscv(42.2, DesignNo=4, True.R=0.9) # 14 per group, EU EMA. This applies only for Cmax
ssscv(42.2, DesignNo=4, True.R=0.9, Region="US") # 9 per group, US FDA
ssscv(42.2, DesignNo=4, True.R=0.9, Region="KR") # 14 per group, Korea MFDS. Only for Cmax
}
\keyword{Sample Size}
