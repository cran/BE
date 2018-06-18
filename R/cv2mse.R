cv2mse = function(cv)
{
  cv = cv/100
  return(log(cv*cv + 1))
}