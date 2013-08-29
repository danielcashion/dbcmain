# Start Code--------------------------------------------------------------------
source("Z:/Risk/akochetkova/R_Scripts/FnDef_Load.r")
source("Z:/Risk/akochetkova/R_Scripts/FnDef_Correlation_History.r")

# get previous non-missing S&P date from FAME
get.spx <-  GetFameAK("cash", "spx.cl", 
  format(Sys.Date() - 50, "%Y%m%d"), 
  format(Sys.Date() - 1, "%Y%m%d"))
no.hol <- rownames(get.spx)[!is.na(get.spx)]
set.date <- no.hol[length(no.hol)]
print(set.date)
# set.date <- "20110331"

set.dir <- "Z:/Risk/akochetkova/Manager Coverage"
setwd(set.dir)
bench.ref.temp <- read.csv(
  "Z:/Risk/akochetkova/R_Scripts/BenchmarkRef_FireDrills.csv", 
  colClasses = "character")
names(bench.ref.temp)
n.price.days <- 820
bench.ref0 <- bench.ref.temp[bench.ref.temp$DB_Name != "", ]
# bench.ref0 <-  bench.ref.temp[!(bench.ref0$DB_Name %in% c("", "futures", "credit")), ]

# calculate weekdays
my.alldays <- timeSequence(as.Date(set.date, "%Y%m%d") -
  ceiling(7 * n.price.days / 5),
  as.Date(set.date, "%Y%m%d"), by="day")
my.weekdays <- my.alldays[isWeekday(my.alldays)]
my.start <- format(my.weekdays[length(my.weekdays) - n.price.days +1], "%Y%m%d")
my.end <- format(my.weekdays[length(my.weekdays)], "%Y%m%d")
my.ticks <- seq(as.Date(as.yearmon(my.start, "%Y%m%d"))
  , as.Date(as.yearmon(my.end, "%Y%m%d"))
  , by = "4 months")
my.ticks
my.series <- union(paste(bench.ref0$DB_Name, bench.ref0$Component, sep="'")
  , paste(bench.ref0$DB_Name, bench.ref0$Unadjusted, sep="'"))
#-------------------------------------------------------------------------------
get.temp <- GetFameAK(
    sapply(strsplit(my.series,"'"), function(x) x[1])
  , sapply(strsplit(my.series,"'"), function(x) x[2])
  , my.start, my.end)
# match_holiday
get.spx <- GetFameAK("cash", "spx.cl", my.start, my.end)
get.temp[rownames(get.spx)[is.na(get.spx[, 1])], ] <- NA
#
roc.comp0 <- names(table(bench.ref0[bench.ref0$Result_diff_roc == "roc", "Component"]))
zroc0 <-  roc.comp0[apply(get.temp[, roc.comp0], 2, function(x) sum(x[!is.na(x)] == 0)) > 0]
for (zz in zroc0) {
  zdates <- rownames(get.temp)[!is.na(get.temp[, zz])][get.temp[!is.na(get.temp[, zz]), zz] == 0]
  print(paste("Price of 0 for:", zz, zdates))
  get.temp[zdates, zz] <- NA
}
# fill forward
my.temp <- get.temp
for (s.name in colnames(get.temp)) {
  for (x in which(is.na(get.temp[, s.name]))) {
    my.temp[x, s.name] <- ifelse(x == 1,
      my.temp[x, s.name],
      my.temp[x - 1, s.name])
  }
}
print(apply(is.na(my.temp), 2, sum))
my.price.data <- my.temp
# calculate initial returns
lag.days <- 1
comp.names <- setdiff(bench.ref0$Component, c())
my.rtn.data <- sapply(comp.names, function(xname) {
  xind <- match(xname, bench.ref0$Component)
  xtype <- bench.ref0$Result_diff_roc[xind]
  xfactor <- as.numeric(bench.ref0$Common_Factor[xind])
  res <- my.price.data[, xname]
  res.lag <- res[1:(length(res) - lag.days)]
  res.nolag <- res[(lag.days + 1):length(res)]
  switch(xtype
  , "roc" = 100 * (res.nolag / res.lag - 1)
  , "diff" = (res.nolag - res.lag) * xfactor / -1000
  , "level_diff" = (res.nolag - res.lag) * xfactor)
  })
dim(my.rtn.data)
rownames(my.rtn.data)
colnames(my.rtn.data)

my.tenors <- c(20, 60, 260)
my.colors <- c("darkgrey", "brown", "darkblue")
my.tenor.names <- paste(my.tenors, "d", sep="")
names(my.tenors) <- my.tenor.names
names(my.colors) <- my.tenor.names


SemiVol <- function(x, direction = c("lower", "upper"), target = 0) {
# square root of average squared deviations from target in a specified direction
  direction <- match.arg(direction)
  selection <- switch(direction
    , lower = x[x < target]
    , upper = x[x > target])
  res <- sqrt(mean((selection - target)^2))
  return(res)
} # End of SemiVol


# ynames <- "KOSPI2.cl"; xname <- "NKY.CL"
# ynames <- "NIFTY.cl"; xname <- "NKY.CL"
ynames <- "NDX.CL"; xname <- "SPX.CL"
# ynames <- "UKX.CL"; xname <- "NKY.CL"
# ynames <- "SPX.CL"; xname <- "EUUS#KR.CL"
# ynames <- "NKY.CL"; xname <- "USJP#KR.CL"

xind <- match(xname, bench.ref0$Component)
if (length(ynames) == 1) {
  fname <- paste("VR", set.date, gsub("[.#]", "_", ynames), gsub("[.#]", "_", xname), sep="_")
} else {
  fname <- paste("VR", set.date, "vs", gsub("[.#]", "_", xname), sep="_")
}
pdf(paste(fname, ".pdf", sep=""), 7.5, 10, paper="letter")
for (yname in ynames) {
  yind <- match(yname, bench.ref0$Component)
  par(mfrow=c(2, 1), oma=c(6, 0, 0, 0))
  out <- list()
  for (my.direction in c("upper", "lower")) {
    out.dir <- sapply(my.tenor.names, function(xtenor.name) {
      xtenor <- my.tenors[xtenor.name]
      sapply(rownames(my.price.data), function(xdate) {
        i <- match(xdate, rownames(my.price.data)) - lag.days
        res <- NA
        if (i >= xtenor) {
        y <- my.rtn.data[(i - xtenor + 1):i, yname]
        x <- my.rtn.data[(i - xtenor + 1):i, xname]
        res <- SemiVol(y, my.direction) / SemiVol(x, my.direction)
        }
        return(res)
        })})
    out[[my.direction]] <- out.dir
    out.zoo <- zoo(out.dir, as.Date(rownames(out.dir), "%Y%m%d"))
    par(mgp = c(3, 1, 0), las=1, cex.axis=0.7, tcl=0.25, lab = c(8, 8, 7)
      , mai = c(0.22, 0.82, 0.82, 0.82))
    plot(zoo(my.price.data[, bench.ref0$Unadjusted[yind]]
      , as.Date(rownames(my.price.data), "%Y%m%d"))
      , lty=3, lwd=2, col="darkred"
      , xaxt="n", yaxt="n", xlab="", ylab="")
    axis(4, hadj=.5)
    par(new=T)
    plot(out.zoo
      , plot.type="single", type="p", pch=18, xaxt="n", yaxt="n", cex=0.5
      , xlab="", ylab=""
      , col=as.list(my.colors))
    axis(2, hadj=0)
    par(mgp = c(3, 0, 0))
    axis(1, my.ticks, format(my.ticks, "%b %Y"), line=0, padj=0)
    mtext(paste("Vol Ratio when"
      , bench.ref0$Description[yind]
      , "and"
      , bench.ref0$Description[xind]
      , "are"
      , switch(my.direction, "upper" = "higher", "lower"="lower"))
      , side=3, line=0.2, cex = 0.8)
    mtext(paste("Vol Ratio: 1"
      , switch(bench.ref0$Result_diff_roc[xind], "roc"="%", "diff"=" bp", "level_diff" = " pt")
      , " Move in "
      , bench.ref0$Description[xind]
      , " = x"  
      , switch(bench.ref0$Result_diff_roc[yind], "roc"="% ", "diff"="bp ", "level_diff" = "pt ")
      , bench.ref0$Description[yind]
      , sep="")
      , side=2, line=1.5, cex = 0.8, las=0)
    text(par("usr")[2], par("usr")[4]
      , labels=paste(bench.ref0$Description[yind]
      , switch(bench.ref0$Result_diff_roc[yind], "roc"="Price", "diff"="Yield", "level_diff" = "Level")
      ), xpd=T, cex=0.8, srt=-90, pos=4, offset=2.5)
    graphics::legend("topleft"
      , paste(my.tenor.names, "vol ratio")
      , col=my.colors
      , pch=18
      , bty="n", inset=c(0, -0.15), xpd=T, cex=0.7, horiz=T)
    graphics::legend("topright"
      , bench.ref0$Description[yind]
      , col="darkred"
      , lty=3, lwd=2
      , bty="n", inset=c(0, -0.15), xpd=T, cex=0.7, horiz=T)
  } # end of loop
  mtext(paste("Run as of"
    , format(as.Date(set.date, "%Y%m%d"), "%d-%b-%Y"))
    , 1, 5, outer=T, adj=0, cex=0.7)
  fnote <- "Vol calculated as square root of average squared deviations from target=0 in a specified direction"
  mtext(fnote
    , 1, 2, outer=T, adj=0, cex=0.7, font=3)
  fnote <- paste("Data source - FAME:"
    , paste(setdiff(c(
        paste(bench.ref0$DB_Name[yind], yname, sep="'")
      , paste(bench.ref0$DB_Name[yind], bench.ref0$Unadjusted[yind], sep="'")
      , paste(bench.ref0$DB_Name[xind], xname, sep="'")), c()), collapse=", ")) 
  mtext(fnote
    , 1, 3, outer=T, adj=0, cex=0.7, font=3)
} # end of loop on yname
dev.off()

#
#  xtenor <- 20
#  sort(out$upper[, "20d"])[1]
#  sort(out$lower[, "20d"])[1]
#  # xdate <- "20101230"
#  # xdate <- "20111214"
#  # xdate <- "20110118"
#  # xdate <- "20110902"
#  # xdate <- "20120102"
#  # xdate <- "20090323"
#  xdate <- "20110113"
#  i <- match(xdate, rownames(my.price.data)) - lag.days
#  y <- my.rtn.data[(i - xtenor + 1):i, yname]
#  x <- my.rtn.data[(i - xtenor + 1):i, xname]
#  plot(x, y)
#  abline(h=0)
#  abline(v=0)
#  
#  temp <- t(sapply(rownames(my.price.data), function(xdate) {
#    i <- match(xdate, rownames(my.price.data)) - lag.days
#    res <- rep(NA, 4)
#    if (i >= xtenor) {
#    y <- my.rtn.data[(i - xtenor + 1):i, yname]
#    x <- my.rtn.data[(i - xtenor + 1):i, xname]
#    res <- c(sum(y > 0), sum(y < 0), sum(x > 0), sum(x < 0))
#    }
#    return(res)
#    }))
#  hist(temp[, 1])
#  hist(temp[, 2])
#  hist(temp[, 3])
#  hist(temp[, 4])
#

# End Code----------------------------------------------------------------------
