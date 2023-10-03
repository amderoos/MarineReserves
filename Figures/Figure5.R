if (!exists("par.defaults")) par.defaults <- par(no.readonly = TRUE) # save default, for resetting...
allenvvars <- ls()
rm(list=allenvvars[(allenvvars != "par.defaults")])
require(latex2exp)
require(vioplot)

ToPdf <- T

basedir <- "~/Projects/MarineReserves/"
figdir  <- paste0(basedir, "Figures/")
datadir <- paste0(figdir, "EBToutput/")
fname <- paste0(figdir, "maintextfigure5.pdf")
setwd(basedir)

tmp00 <- read.table(paste0(datadir, "Yield-Evodyn-RS000-ETS16.out"), colClasses = c(rep("NULL", 24), "numeric", "NULL"))
aduyield00 <- mean(tmp00[,1])
tmp00 <- read.table(paste0(datadir, "Yield-Evodyn-RS000-ETS16.out"), colClasses = c(rep("NULL", 25), "numeric"))
totyield00 <- mean(tmp00[,1])

tmp10 <- read.table(paste0(datadir, "Yield-Evodyn-RS010-ETS09.out"), colClasses = c(rep("NULL", 24), "numeric", "NULL"))
tmp30 <- read.table(paste0(datadir, "Yield-Evodyn-RS030-ETS05.out"), colClasses = c(rep("NULL", 24), "numeric", "NULL"))
aduyield <- cbind(tmp10[,1], tmp30[,1]) / aduyield00

tmp10 <- read.table(paste0(datadir, "Yield-Evodyn-RS010-ETS09.out"), colClasses = c(rep("NULL", 25), "numeric"))
tmp30 <- read.table(paste0(datadir, "Yield-Evodyn-RS030-ETS05.out"), colClasses = c(rep("NULL", 25), "numeric"))
totyield <- cbind(tmp10[,1], tmp30[,1]) / totyield00

colnames(aduyield) <- c("10%", "30%")
colnames(totyield) <- c("10%", "30%")

par(par.defaults)
if (ToPdf) pdf(file = fname, width = 4.5, height = 4.5)

xlabline <- 1.5
ylablineL <- 1.8
cexaxis <- 1.0
cexlab <- 1.2
tcllong  <- 0.35
tclshort <- 0.2

layout(matrix(1:2, nrow = 1, ncol = 2), widths = c(1.3, 1))
par(mar=c(2.75, 3, 1.25, 0.0), mgp=c(3,0.25,0), tcl = tcllong)
# boxplot(totyield, xlab = NA, ylab = NA, yaxt = "n", ylim = c(0, 0.4), outcex = 1, outlwd = 0.1, range = 0)
plot(NA, xlab = NA, ylab = NA, xaxt = "n", yaxt = "n", xlim = c(0.5, 2.5), ylim = c(0, 2))
lines(par("usr")[1:2], rep(1.0, 2), lty = 2, lwd = 1)
vioplot(totyield, col = "grey75", rectCol = "grey75", lineCol = NA, colMed = "black", range = 0, plotCentre = "line", areaEqual = T, add = T)
axis(1, at = c(1, 2), labels=c("10%", "30%"), cex.axis = cexaxis, lwd = 0, lwd.ticks = 1)
axis(2, at = (0:5) * 0.5, 
     labels = c("0", "0.5", "1.0", "1.5", "2.0", "2.5"),
     cex.axis = cexaxis, lwd = 0, lwd.ticks = 1, las=2)
axis(2, at = 0.1 + (0:18) * 0.1,  labels = F, lwd = 0, lwd.ticks = 1, tcl = tclshort)
mtext("Relative change in biomass yield", 2, line=ylablineL, cex=cexlab)
mtext("Total yield", 3, line=0.15, cex=cexlab)

par(mar=c(2.75, 0, 1.25, 0.25), mgp=c(3,0.25,0), tcl = tcllong)
# boxplot(aduyield, xlab = NA, ylab = NA, yaxt = "n", ylim = c(0, 0.008), outcex = 1, outlwd = 0.1, range = 0)
plot(NA, xlab = NA, ylab = NA, xaxt = "n", yaxt = "n", xlim = c(0.5, 2.5), ylim = c(0, 2))
lines(par("usr")[1:2], rep(1.0, 2), lty = 2, lwd = 1)
vioplot(aduyield, col = "grey75", rectCol = "grey75", lineCol = NA, colMed = "black", range = 0, plotCentre = "line", areaEqual = T, add = T)
axis(1, at = c(1, 2), labels=c("10%", "30%"), cex.axis = cexaxis, lwd = 0, lwd.ticks = 1)
axis(2, at = (0:5) * 0.5,  labels = F, lwd = 0, lwd.ticks = 1, tcl = tclshort)

mtext("Marine reserve size", 1, line=xlabline, cex=cexlab, at = 0.5)
mtext("Adult yield", 3, line=0.15, cex=cexlab)

if (ToPdf) {
  dev.off()
  system(paste("open", fname))
}
