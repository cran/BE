ssscv = function(CV, DesignNo=1, True.R=1, Alpha=0.1, Beta=0.2, nMax=999999)
{
  Bound = scaledBound(CV)
  mse = log(1 + (CV/100)^2)
  Res = ssmse(mse, DesignNo, True.R, Alpha, Beta, Bound[1], Bound[2], nMax)
  attr(Res, "Bound") = Bound
  return(Res)
}
