plot2x2b = function(bedata, Var)
{
  if(!assert(bedata)) {
    cat("\n Subject count should be balanced!\n");
    return(NULL);
  }

  Yi11 = bedata[bedata$GRP=="RT" & bedata$PRD==1, Var]
  Yi21 = bedata[bedata$GRP=="RT" & bedata$PRD==2, Var]
  Yi12 = bedata[bedata$GRP=="TR" & bedata$PRD==1, Var]
  Yi22 = bedata[bedata$GRP=="TR" & bedata$PRD==2, Var]

  par(oma=c(1,1,3,1), mfrow=c(2,2))

  boxplot(Yi11, Yi21, Yi12, Yi22, names=c("SEQ=RT\nPRD=1", "SEQ=RT\nPRD=2", "SEQ=TR\nPRD=1", "SEQ=TR\nPRD=2"), cex.axis=0.85, main="(a) By Sequence and Period")
  boxplot(c(Yi11, Yi21), c(Yi12, Yi22), names=c("Sequence=RT", "Sequence=TR"), main="(b) By Sequence")
  boxplot(c(Yi11, Yi12), c(Yi21, Yi22), names=c("Period=1", "Period=2"), main="(c) By Period")
  boxplot(c(Yi12, Yi21), c(Yi11, Yi22), names=c("Treatment=T", "Treatment=R"), main="(d) By Treatment")
  mtext(outer=TRUE, side=3, paste("Box Plots for", Var), cex=1.5)
}
