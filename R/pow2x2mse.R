pow2x2mse = function(n1, n2, mse, True.R=1, Alpha=0.1, ThetaL=0.8, ThetaU=1.25)
{
# n1, n2: sample size for each group
  if (True.R <= ThetaL | True.R >= ThetaU) return(0)
  t0 = qt(1 - Alpha/2, n1 + n2 - 2)
  p1 = pt(-1*t0, n1 + n2 - 2, ncp = log(ThetaL/True.R) / sqrt(mse/2*(1/n1 + 1/n2)) ) # 1st beta error: LL > 0.8 = 1 - p1
  p2 = pt(t0, n1 + n2 - 2, ncp = log(ThetaU/True.R) / sqrt(mse/2*(1/n1 + 1/n2)) )    # 2nd beta error: UL < 1.25 = p2
  Power = p1 - p2  # 1 - (1 - p1 + p2) = 1 - both beta errors
  if (Power < 0) Power = 0
  return(Power)
}
