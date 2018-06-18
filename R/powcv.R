powcv = function(n, CV, DesignNo=1, True.R=1, Alpha=0.1, ThetaL=0.8, ThetaU=1.25)
{
  mse = log(1 + (CV/100)^2)
  return(powmse(n, mse, DesignNo, True.R, Alpha, ThetaL, ThetaU))
}
