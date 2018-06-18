sscv = function(CV, DesignNo=1, True.R=1, Alpha=0.1, Beta=0.2, ThetaL=0.8, ThetaU=1.25, nMax=999999)
{
  mse = log(1 + (CV/100)^2)
  return(ssmse(mse, DesignNo, True.R, Alpha, Beta, ThetaL, ThetaU, nMax))
}
