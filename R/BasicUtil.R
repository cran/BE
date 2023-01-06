assert = function(bedata)
{
  Si11 = bedata[bedata$GRP=="RT" & bedata$PRD==1, "SUBJ"]
  Si21 = bedata[bedata$GRP=="RT" & bedata$PRD==2, "SUBJ"]
  Si12 = bedata[bedata$GRP=="TR" & bedata$PRD==1, "SUBJ"]
  Si22 = bedata[bedata$GRP=="TR" & bedata$PRD==2, "SUBJ"]

  nameCheck = setdiff(c("GRP", "SUBJ", "PRD", "TRT"), colnames(bedata))
  if (length(nameCheck) > 0) stop("GRP, SUBJ, PRD, TRT columns should exist!")

  return(identical(Si11, Si21) & identical(Si12, Si22))
}

TrimData = function(bedata)
{
  bedata$GRP = trimws(bedata$GRP)
  bedata$SUBJ = trimws(bedata$SUBJ)
  bedata$PRD = trimws(bedata$PRD)
  bedata$TRT = trimws(bedata$TRT)
  return(bedata)
}

drawind = function(g1l, g1r, g2l, g2r, g1s, g2s)
{
  for (i in 1:length(g1l)) {
    x = jitter(c(1, 2), factor=0.3)
    y = c(g1l[i], g1r[i])
    lines(x, y, type="l", lty=1, col="red")
    text(x[1]-0.05, y[1], paste(g1s[i]), cex=0.6, col="red")
  }

  for (i in 1:length(g2l)) {
    x = jitter(c(1, 2), factor=0.3)
    y = c(g2l[i], g2r[i])
    lines(x, y, type="l", lty=2, col="blue")
    text(x[2]+0.05, y[2], paste(g2s[i]), cex=0.6, col="blue")
  }
}

drawmeansd = function(ma, sa, mb, sb, mc, sc, md, sd, y.max)
{
  sft = 0.03
  delta = mean(ma, mc) - mean(mb, md)
  y.RT = mean(ma, mc) + sign(delta) * y.max * 0.05
  y.TR = mean(mb, md) - sign(delta) * y.max * 0.05

  lines(c(1-sft, 2-sft), c(ma, mc), type="l", lty=1, col="red")
  text(1.5-sft, y.RT, "RT", col="red")
  if (sa > 0) arrows(1-sft, ma-sa, 1-sft, ma+sa, length=0.1, code=3, angle=90, col="red")
  if (sc > 0) arrows(2-sft, mc-sc, 2-sft, mc+sc, length=0.1, code=3, angle=90, col="red")

  lines(c(1+sft, 2+sft), c(mb, md), type="l", lty=2, col="blue")
  text(1.5+sft, y.TR, "TR", col="blue")
  if (sb > 0) arrows(1+sft, mb-sb, 1+sft, mb+sd, length=0.1, code=3, angle=90, col="blue")
  if (sd > 0) arrows(2+sft, md-sd, 2+sft, md+sd, length=0.1, code=3, angle=90, col="blue")
}
