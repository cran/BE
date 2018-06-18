ssmse = function(mse, DesignNo=1, True.R=1, Alpha=0.1, Beta=0.2, ThetaL=0.8, ThetaU=1.25, nMax=999999)
{
  if (True.R <= ThetaL | True.R >= ThetaU) return(Inf)
  for (i in 2:nMax) {
    Power = powmse(i, mse, DesignNo, True.R, Alpha, ThetaL, ThetaU)
    if (Power > 1 - Beta) return(i)
  }
  return(paste(">", nMax))
}
