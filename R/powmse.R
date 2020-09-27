powmse = function(n, mse, DesignNo=1, True.R=1, Alpha=0.1, ThetaL=0.8, ThetaU=1.25)
{
  if (True.R <= ThetaL | True.R >= ThetaU) return(0)

  aNu0 = c(2, 4, 4, 4, 6, 6, 12, 12, 12, 12, 12)
  aNu1 = c(2, 3, 4, 3, 5, 4, 5, 6, 4, 9, 6)
  aBx = c(1, 2, 3/4, 1/2, 11/20, 1/2, 1/4, 5/12, 1/3, 11/20, 1/2)

  nu = aNu0[DesignNo]*n - aNu1[DesignNo]
  bx = sqrt(mse*aBx[DesignNo]/n)
  t0 = qt(1 - Alpha/2, nu)
  p1 = pt(-1*t0, nu, ncp = log(ThetaL/True.R)/bx) # 1st beta error: LL > 0.8 = 1 - p1
  p2 = pt(t0, nu, ncp = log(ThetaU/True.R)/bx)    # 2nd beta error: UL < 1.25 = p2
  Power = p1 - p2  # 1 - (1 - p1 + p2) = 1 - both beta errors
  if (Power < 0) Power = 0
  return(Power)
}
