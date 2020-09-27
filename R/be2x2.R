be2x2 = function(Data, Columns = c("AUClast", "Cmax", "Tmax"), rtfName="")
{
  if ("data.frame" %in% class(Data)) {
    bedata = Data
  } else if ("character" %in% class(Data)) {
    bedata = read.csv(Data)
  } else {
    stop("Data should be data.frame or file name!")
  }

  bedata = bedata[order(bedata$GRP, bedata$PRD, bedata$SUBJ),];
  if(!assert(bedata)) {
    cat("\n Drop-outed subjects should not be included!\n");
    return(NULL);
  }

  nCol = length(Columns)
  if (nCol == 0) stop("Input Error. Please, check the arguments!")

  if (rtfName != "") {
    rtf = RTF(rtfName)
    addHeader(rtf, title="Bioequivalence Test Result")
    addNewLine(rtf)
    addHeader(rtf, "Table of Contents")
    addTOC(rtf)
  }

  Result = vector()
  for (i in 1:nCol) {
    plot2x2(bedata, Columns[i])

    if (toupper(Columns[i]) != "TMAX"){
      cResult = test2x2(bedata, Columns[i])
    } else {
      cResult = hodges(bedata, Columns[i])
    }  

    if (rtfName != "") {
      addPageBreak(rtf)
      addHeader(rtf, title=Columns[i], TOC.level=1)
      LineResult = capture.output(print(cResult, na.print=""))
      for (j in 1:length(LineResult)) addParagraph(rtf, LineResult[j])
      addPageBreak(rtf)
      addPlot(rtf, plot.fun=plot2x2a, width=6.5, height=6.5, res=300, bedata=bedata, Var=Columns[i])
      addPageBreak(rtf)
      addPlot(rtf, plot.fun=plot2x2b, width=6.5, height=6.5, res=300, bedata=bedata, Var=Columns[i])
    }

    Result = c(Result, list(cResult))
  }

  if (rtfName != "") {
    addPageBreak(rtf)
    addSessionInfo(rtf)
    done(rtf)
  }

  names(Result) = Columns
  return(Result)
}
