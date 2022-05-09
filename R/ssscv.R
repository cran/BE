ssscv = function(CV, DesignNo = 1, True.R = 1, Alpha = 0.1, Beta = 0.2, Region="EU", nMax = 999999)
{
  Region = toupper(trimws(Region))
  if (Region %in% c("US", "FDA")) {
    Bound = scaledBound(CV=CV, k=0.893, digits=22)
  } else if (Region %in% c("KR", "MFDS")) {
    Bound = scaledBound(CV=CV, k=0.76, digits=4)
  } else {
    Bound = scaledBound(CV=CV, k=0.76, digits=22)    
  }
  
  mse = log(1 + (CV/100)^2)
  Res = ssmse(mse, DesignNo, True.R, Alpha, Beta, Bound[1], Bound[2], nMax)
  attr(Res, "Bound") = Bound
  return(Res)
}
