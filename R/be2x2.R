be2x2 = function(filename, Columns = c("AUClast", "Cmax", "Tmax"), Plot=TRUE)
{
  bedata = read.csv(filename);

  bedata = bedata[order(bedata$GRP, bedata$PRD, bedata$SUBJ),];
  if(!assert(bedata)) {
    cat("\n Subject count should be balanced!\n");
    return(NULL);
  }

  nCol = length(Columns)
  if (nCol == 0) stop("Input Error. Please, check the arguments!")

  Result = vector()
  for (i in 1:nCol) {
    if (Plot == TRUE) plot2x2(bedata, Columns[i])
    if (toupper(Columns[i]) != "TMAX"){
      cResult = test2x2(bedata, Columns[i])
    } else {
      cResult = hodges(bedata, Columns[i])
    }  
    Result = c(Result, list(cResult))
  }
  names(Result) = Columns
  return(Result)
}
