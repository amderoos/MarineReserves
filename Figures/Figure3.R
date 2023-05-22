basedir <- "~/Projects/MarineReserves/"
datadir <- paste0(basedir, "Figures/EBToutput/")
modeldir <- paste0(basedir, "Equi/")
fname <- paste0(basedir, "Figures/maintextfigure3.pdf")
setwd(basedir)

ToPdf <- T
EBT_PSPM_cmp <- F

EBTtcol         <-  1
EBTres1col      <-  2
EBTres2col      <-  3
EBTres3col      <-  4
EBTtotnumcol    <-  5
EBTtotbiocol    <-  6
EBTh1numcol     <-  7
EBTh1biocol     <-  8
EBTh2numcol     <-  9
EBTh2biocol     <- 10
EBTh3numcol     <- 11
EBTh3biocol     <- 12
EBTh2juvnumcol  <- 13
EBTh2juvbiocol  <- 14
EBTh3juvnumcol  <- 15
EBTh3juvbiocol  <- 16
EBTh2adunumcol  <- 17
EBTh2adubiocol  <- 18
EBTh3adunumcol  <- 19
EBTh3adubiocol  <- 20
EBTsmoltsizecol <- 21   
EBTtotreprocol  <- 22
EBTnumcohcol    <- 23
EBTbifparcol    <- 24
EBTperiodcol    <- 25

PSPMbifparcol    <-  1
PSPMres1col      <-  2
PSPMres2col      <-  3
PSPMres3col      <-  4
PSPMh1biocol     <-  9
PSPMh2juvbiocol  <- 10
PSPMh3juvbiocol  <- 11
PSPMh2adubiocol  <- 12
PSPMh3adubiocol  <- 13
PSPMh1numcol     <- 14
PSPMh2juvnumcol  <- 15
PSPMh3juvnumcol  <- 16
PSPMh2adunumcol  <- 17
PSPMh3adunumcol  <- 18

library(PSPManalysis)
library(latex2exp)
library(shape)
library(igraph)
library(DescTools)

iArrows <- igraph:::igraph.Arrows
DefaultPars <- c(Rho1 = 0.5, Rho2 =    0.5, Delta = 1.0, 
                 Eta1 = 1.0, Eta2 =    0.8, Eta3  = 0.8, 
                 ETSJ = 1.5, ETSA =    1.5, WS    = 0.3735478389, 
                 Q    = 1.0, Beta = 2000.0, SR    = 0.0)

if (ToPdf) pdf(file = fname, width = (44.0 / 2.54), height = 10.0)

layout(matrix(1:8, nrow = 2, ncol = 4), widths = c(1.3, rep(1.0, 6)), heights = rep(c(0.98, 1.0), 6))

xlimc <- c(-0.05, 1.05)
xlimw <- c(0.08, 0.45)
ylim1 <- c(-0.05, 1.05)
ylim2 <- c(-0.02, 0.35)

cexlab <- 2.0
cexaxs <- 2.0
cexleg <- 1.8
cexttl <- 2.2
cexpnt <- 2.0
cexbt  <- 1.6

axislwd <- 1

par(tcl = 0.6, mgp = c(3, 1, 0))

########## Panel A

pars <- DefaultPars
init=c(0.00001, 0.1707116827,	0.3947321797,	0.5000000000, 6)
if (!exists("Equi_SR_Ws03736")) {
  Equi_SR_Ws03736 <- PSPMequi(modelname = paste0(modeldir, "HarvestWithReserve"), 
                              biftype = "EQ", startpoint = init, 
                              stepsize = 0.1, parbnds = c(11, 0.0, 1.0), 
                              parameters = pars, clean=TRUE, 
                              options = c("popEVO", "0", "parEVO", "8"))
}

dt          <- Equi_SR_Ws03736$curvepoints

EBTdt       <- read.table(paste0(datadir, "Figure3Aup.minmax.out"))
EBTmax      <- EBTdt[2 * (1:(nrow(EBTdt) / 2.0)),]
EBTmin      <- EBTdt[2 * (1:(nrow(EBTdt) / 2.0)) - 1,]

stable      <- EBTmax[, EBTperiodcol] > 1.0E-4
indx2       <- nrow(EBTmax)
indx1       <- min((1:indx2)[stable]) - 1
EBTmax_upS  <- EBTmax[(1:indx1),]
EBTmin_upS  <- EBTmin[(1:indx1),]
EBTupM      <- rbind(EBTmax[(indx2:indx1),], EBTmin[(indx1:indx2),])

EBTdt       <- read.table(paste0(datadir, "Figure3Adwn.minmax.out"))
EBTmax      <- EBTdt[2 * (1:(nrow(EBTdt) / 2.0)),]
EBTmin      <- EBTdt[2 * (1:(nrow(EBTdt) / 2.0)) - 1,]

stable      <- EBTmax[, EBTperiodcol] > 1.0E-4
indx2       <- nrow(EBTmax)
indx1       <- max((1:indx2)[stable]) + 1
EBTmax_dwnS <- EBTmax[(indx1:indx2),]
EBTmin_dwnS <- EBTmin[(indx1:indx2),]
EBTdwnM     <- rbind(EBTmax[(1:indx1),], EBTmin[(indx1:1),])
stable      <- dt[, PSPMbifparcol] < min(EBTupM[, EBTbifparcol])
  
par(mar = c(0, 10, 4.0, 1))
plot(NULL, NULL, xlim = xlimc, ylim = ylim1, 
     xlab = "", ylab = "", xaxs = "i", yaxs = "i",
     xaxt = "n", yaxt = "n")
axis(1, label = F, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd)
axis(2, label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2)
mtext("Biomass\nin nursery habitat", 2, cex = cexlab, line = 4.8)
mtext("A: no evolution", 3, cex = cexttl, line = 1.0)
# text(xlim1[2], ylimb[2], "A", cex = 4.0, xpd = T, adj = c(0.5, 0))

if (EBT_PSPM_cmp) {
  lines(EBTmax_upS[,EBTbifparcol],  EBTmax_upS[,EBTh1biocol],  lwd = 2 * lwd, col = "cyan1")
  lines(EBTmax_dwnS[,EBTbifparcol], EBTmax_dwnS[,EBTh1biocol], lwd = 2 * lwd, col = "red1")
  lines(EBTdwnM[,EBTbifparcol],     EBTdwnM[,EBTh1biocol],     lwd = 3 * lwd, col = "cyan1")
}

lines(dt[ stable,PSPMbifparcol], dt[ stable,PSPMh1biocol], lwd = 2 * lwd, col = "black")
lines(dt[!stable,PSPMbifparcol], dt[!stable,PSPMh1biocol], lwd = lwd, col = "black", lty = 2)
lines(EBTupM[,EBTbifparcol], EBTupM[,EBTh1biocol], lwd = lwd, col = "black")

par(mar = c(6, 10, 0, 1))
plot(NULL, NULL, xlim = xlimc, ylim = ylim2, 
     xlab = "", ylab = "", xaxs = "i", yaxs = "i",
     xaxt = "n", yaxt = "n")
axis(1, label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd)
axis(2, at = 0.05 + (0:3) * 0.1, label = F, 
     cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2, tcl = 0.4)
axis(2, at = (0:3) * 0.1, label = c("0.0", "0.1", "0.2", "0.3"), 
     cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2)
mtext("Biomass\nin growth habitat", 2, cex = cexlab, line = 4.8)
mtext("Marine reserve size", 1, cex = cexlab, line = 4.5)

lines(dt[stable,PSPMbifparcol], dt[stable,PSPMh2juvbiocol] + dt[stable,PSPMh2adubiocol], lwd = 2 * lwd, col = "#0072B2")
lines(dt[stable,PSPMbifparcol], dt[stable,PSPMh3juvbiocol] + dt[stable,PSPMh3adubiocol], lwd = 2 * lwd, col = "#D55E00")

lines(dt[!stable,PSPMbifparcol], dt[!stable,PSPMh2juvbiocol] + dt[!stable,PSPMh2adubiocol], lwd = lwd, lty = 2, col = "#0072B2")
lines(dt[!stable,PSPMbifparcol], dt[!stable,PSPMh3juvbiocol] + dt[!stable,PSPMh3adubiocol], lwd = lwd, lty = 2, col = "#D55E00")

lines(EBTupM[,EBTbifparcol], EBTupM[,EBTh2biocol], lwd = lwd, col = "#0072B2")
lines(EBTupM[,EBTbifparcol], EBTupM[,EBTh3biocol], lwd = lwd, col = "#D55E00")

########## Panel B

init=c(0.05, 0.4627769040, 0.3224126737, 0.5, 6)
pars <- DefaultPars
if (!exists("Equi_SR00_Ws")) {
  Equi_SR00_Ws <- PSPMequi(modelname = paste0(modeldir, "HarvestWithReserve"), 
                         biftype = "EQ", startpoint = init, 
                         stepsize = 0.01, parbnds = c(8, 0.05, 0.5), 
                         parameters = pars, clean=TRUE, 
                         options = c("popEVO", "0"))
}

dt          <- Equi_SR00_Ws$curvepoints
dtbp        <- Equi_SR00_Ws$bifpoints
dtbt        <- Equi_SR00_Ws$biftypes

EBTdt       <- read.table(paste0(datadir, "Figure3Bup.minmax.out"))
EBTmax      <- EBTdt[2 * (1:(nrow(EBTdt) / 2.0)),]
EBTmin      <- EBTdt[2 * (1:(nrow(EBTdt) / 2.0)) - 1,]

stable      <- EBTmax[, EBTperiodcol] > 1.0E-4
indx2       <- nrow(EBTmax)
indx1       <- min((1:indx2)[stable]) - 1
EBTmax_upS  <- EBTmax[(1:indx1),]
EBTmin_upS  <- EBTmin[(1:indx1),]
EBTupM      <- rbind(EBTmax[(indx2:indx1),], EBTmin[(indx1:indx2),])

EBTdt       <- read.table(paste0(datadir, "Figure3Bdwn.minmax.out"))
EBTmax      <- EBTdt[2 * (1:(nrow(EBTdt) / 2.0)),]
EBTmin      <- EBTdt[2 * (1:(nrow(EBTdt) / 2.0)) - 1,]

stable      <- EBTmax[, EBTperiodcol] > 1.0E-4
indx2       <- nrow(EBTmax)
indx1       <- max((1:indx2)[stable]) + 1
EBTmax_dwnS <- EBTmax[(indx1:indx2),]
EBTmin_dwnS <- EBTmin[(indx1:indx2),]
EBTdwnM     <- rbind(EBTmax[(1:indx1),], EBTmin[(indx1:1),])
stable      <- dt[, PSPMbifparcol] < min(EBTupM[, EBTbifparcol])

par(mar = c(0, 1, 4.0, 1))
plot(NULL, NULL, xlim = xlimw, ylim = ylim1, 
     xlab = "", ylab = "", xaxs = "i", yaxs = "i",
     xaxt = "n", yaxt = "n")
axis(1, label = F, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd)
axis(2, label = F, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2)
mtext("B: no reserve", 3, cex = cexttl, line = 1.0)
# text(xlim1[2], ylimb[2], "A", cex = 4.0, xpd = T, adj = c(0.5, 0))

if (EBT_PSPM_cmp) {
  lines(EBTmax_upS[,EBTbifparcol],  EBTmax_upS[,EBTh1biocol],  lwd = 2 * lwd, col = "cyan1")
  lines(EBTmax_dwnS[,EBTbifparcol], EBTmax_dwnS[,EBTh1biocol], lwd = 2 * lwd, col = "red1")
  lines(EBTdwnM[,EBTbifparcol],     EBTdwnM[,EBTh1biocol],     lwd = 3 * lwd, col = "cyan1")
}

lines(dt[ stable,PSPMbifparcol], dt[ stable,PSPMh1biocol], lwd = 2 * lwd, col = "black")
lines(dt[!stable,PSPMbifparcol], dt[!stable,PSPMh1biocol], lwd = lwd, col = "black", lty = 2)
lines(EBTupM[,EBTbifparcol], EBTupM[,EBTh1biocol], lwd = lwd, col = "black")

points(dtbp[,PSPMbifparcol], dtbp[,PSPMh1biocol], pch = 19, col = "#DD0000", cex = cexpnt)
text(dtbp[,PSPMbifparcol], dtbp[,PSPMh1biocol], "ESS", cex = cexbt, adj = c(1.3, 0.45))

Arrows(0.09, 0.07, 0.16, 0.07, arr.type="triangle", lwd = 5, arr.width = 0.2, col = "#A10000")
Arrows(0.44, 0.85, 0.39, 0.85, arr.type="triangle", lwd = 5, arr.width = 0.2, col = "#A10000")

par(mar = c(6, 1, 0, 1))
plot(NULL, NULL, xlim = xlimw, ylim = ylim2, 
     xlab = "", ylab = "", xaxs = "i", yaxs = "i",
     xaxt = "n", yaxt = "n")
axis(1, at = 0.15 + (0:2) * 0.1, label = F, 
     cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, tcl = 0.4)
axis(1, at = 0.1 + (0:3) * 0.1, label = c("0.1", "0.2", "0.3", "0.4"), 
     cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd)
axis(2, at = 0.05 + (0:3) * 0.1, label = F, 
     cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2, tcl = 0.4)
axis(2, at = (0:3) * 0.1, label = F, 
     cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2)
mtext("Body size at shift", 1, cex = cexlab, line = 4.5)

lines(dt[stable,PSPMbifparcol], dt[stable,PSPMh2juvbiocol] + dt[stable,PSPMh2adubiocol], lwd = 2 * lwd, col = "#0072B2")
# lines(dt[stable,PSPMbifparcol], dt[stable,PSPMh3juvbiocol] + dt[stable,PSPMh3adubiocol], lwd = 2 * lwd, col = "#D55E00")

lines(dt[!stable,PSPMbifparcol], dt[!stable,PSPMh2juvbiocol] + dt[!stable,PSPMh2adubiocol], lwd = lwd, lty = 2, col = "#0072B2")
# lines(dt[!stable,PSPMbifparcol], dt[!stable,PSPMh3juvbiocol] + dt[!stable,PSPMh3adubiocol], lwd = lwd, lty = 2, col = "#D55E00")

lines(EBTupM[,EBTbifparcol], EBTupM[,EBTh2biocol], lwd = lwd, col = "#0072B2")
# lines(EBTupM[,EBTbifparcol], EBTupM[,EBTh3biocol], lwd = lwd, col = "#D55E00")

points(dtbp[,PSPMbifparcol], dtbp[,PSPMh2juvbiocol] + dtbp[,PSPMh2adubiocol], pch = 19, col = "#DD0000", cex = cexpnt)
# text(dtbp[,PSPMbifparcol], dtbp[,PSPMh2juvbiocol] + dtbp[,PSPMh2adubiocol], "ESS", cex = cexbt, adj = c(1.3, 0.45))

########## Panel C

init=c(0.05, 0.4627769040, 0.3224126737, 0.5, 6)
pars <- DefaultPars
pars["SR"] <- 0.1
if (!exists("Equi_SR01_Ws")) {
  Equi_SR01_Ws <- PSPMequi(modelname = paste0(modeldir, "HarvestWithReserve"), 
                           biftype = "EQ", startpoint = init, 
                           stepsize = 0.01, parbnds = c(8, 0.05, 0.5), 
                           parameters = pars, clean=TRUE, 
                           options = c("popEVO", "0"))
}

dt          <- Equi_SR01_Ws$curvepoints
dtbp        <- Equi_SR01_Ws$bifpoints
dtbt        <- Equi_SR01_Ws$biftypes

EBTdt       <- read.table(paste0(datadir, "Figure3Cup.minmax.out"))
EBTmax      <- EBTdt[2 * (1:(nrow(EBTdt) / 2.0)),]
EBTmin      <- EBTdt[2 * (1:(nrow(EBTdt) / 2.0)) - 1,]

stable      <- EBTmax[, EBTperiodcol] > 1.0E-4
indx2       <- nrow(EBTmax)
indx1       <- min((1:indx2)[stable]) - 1
EBTmax_upS  <- EBTmax[(1:indx1),]
EBTmin_upS  <- EBTmin[(1:indx1),]
EBTupM      <- rbind(EBTmax[(indx2:indx1),], EBTmin[(indx1:indx2),])

EBTdt       <- read.table(paste0(datadir, "Figure3Cdwn.minmax.out"))
EBTmax      <- EBTdt[2 * (1:(nrow(EBTdt) / 2.0)),]
EBTmin      <- EBTdt[2 * (1:(nrow(EBTdt) / 2.0)) - 1,]

stable      <- EBTmax[, EBTperiodcol] > 1.0E-4
indx2       <- nrow(EBTmax)
indx1       <- max((1:indx2)[stable]) + 1
EBTmax_dwnS <- EBTmax[(indx1:indx2),]
EBTmin_dwnS <- EBTmin[(indx1:indx2),]
EBTdwnM     <- rbind(EBTmax[(1:indx1),], EBTmin[(indx1:1),])

par(mar = c(0, 1, 4.0, 1))
plot(NULL, NULL, xlim = xlimw, ylim = ylim1, 
     xlab = "", ylab = "", xaxs = "i", yaxs = "i",
     xaxt = "n", yaxt = "n")
axis(1, label = F, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd)
axis(2, label = F, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2)
mtext("C: 10% protected", 3, cex = cexttl, line = 1.0)
# text(xlim1[2], ylimb[2], "A", cex = 4.0, xpd = T, adj = c(0.5, 0))

if (EBT_PSPM_cmp) {
  lines(EBTmax_upS[,EBTbifparcol],  EBTmax_upS[,EBTh1biocol],  lwd = 2 * lwd, col = "cyan1")
  lines(EBTmax_dwnS[,EBTbifparcol], EBTmax_dwnS[,EBTh1biocol], lwd = 2 * lwd, col = "red1")
  lines(EBTdwnM[,EBTbifparcol],     EBTdwnM[,EBTh1biocol],     lwd = 3 * lwd, col = "cyan1")
}

indx1 <- which.min(abs(dt[,PSPMbifparcol] - dtbp[1,PSPMbifparcol]))
indx2 <- which.min(abs(dt[,PSPMbifparcol] - dtbp[3,PSPMbifparcol]))
indx3 <- max((indx2:nrow(dt))[dt[(indx2:nrow(dt)),PSPMbifparcol] < min(EBTupM[,EBTbifparcol])])
  
lines(dt[(1:indx1),PSPMbifparcol], dt[(1:indx1),PSPMh1biocol], lwd = 2 * lwd, col = "black")
lines(dt[(indx1:indx2),PSPMbifparcol], dt[(indx1:indx2),PSPMh1biocol], lwd = lwd, lty = 3, col = "black")
lines(dt[(indx2:indx3),PSPMbifparcol], dt[(indx2:indx3),PSPMh1biocol], lwd = 2 * lwd, col = "black")
lines(dt[(indx3:nrow(dt)),PSPMbifparcol], dt[(indx3:nrow(dt)),PSPMh1biocol], lwd = lwd, col = "black", lty = 2)

lines(EBTupM[,EBTbifparcol], EBTupM[,EBTh1biocol], lwd = lwd, col = "black")

points(dtbp[2,PSPMbifparcol], dtbp[2,PSPMh1biocol], pch = 19, col = "#DD0000", cex = cexpnt)
points(dtbp[1,PSPMbifparcol], dtbp[1,PSPMh1biocol], pch = 19, col = "#009E73", cex = cexpnt)
points(dtbp[3,PSPMbifparcol], dtbp[3,PSPMh1biocol], pch = 19, col = "#009E73", cex = cexpnt)
# text(dtbp[2,PSPMbifparcol], dtbp[2,PSPMh1biocol], "ESS", cex = cexbt, adj = c(1.3, 0.45))
text(dtbp[1,PSPMbifparcol], dtbp[1,PSPMh1biocol], "LP", cex = cexbt, adj = c(-0.5, 0.85))
text(dtbp[3,PSPMbifparcol], dtbp[3,PSPMh1biocol], "LP", cex = cexbt, adj = c(-0.5, 0.05))

Arrows(0.09, 0.08, 0.16, 0.08, arr.type="triangle", lwd = 5, arr.width = 0.2, col = "#A10000")
Arrows(0.44, 0.95, 0.37, 0.95, arr.type="triangle", lwd = 5, arr.width = 0.2, col = "#A10000")

iArrows(dtbp[1,PSPMbifparcol] + 0.012, dtbp[1,PSPMh1biocol] + 0.015, 
        dtbp[3,PSPMbifparcol] + 0.012, dtbp[3,PSPMh1biocol] - 0.015,
        h.lwd=3, sh.lwd=3, sh.col="blue",
        curve=-0.3, width=0.75, size=0.7)
iArrows(dtbp[3,PSPMbifparcol] - 0.012, dtbp[3,PSPMh1biocol] - 0.015, 
        dtbp[1,PSPMbifparcol] - 0.012, dtbp[1,PSPMh1biocol] + 0.015,
        h.lwd=3, sh.lwd=3, sh.col="blue",
        curve=-0.3, width=0.75, size=0.7)

par(mar = c(6, 1, 0, 1))
plot(NULL, NULL, xlim = xlimw, ylim = ylim2, 
     xlab = "", ylab = "", xaxs = "i", yaxs = "i",
     xaxt = "n", yaxt = "n")
axis(1, at = 0.15 + (0:2) * 0.1, label = F, 
     cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, tcl = 0.4)
axis(1, at = 0.1 + (0:3) * 0.1, label = c("0.1", "0.2", "0.3", "0.4"), 
     cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd)
axis(2, at = 0.05 + (0:3) * 0.1, label = F, 
     cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2, tcl = 0.4)
axis(2, at = (0:3) * 0.1, label = F, 
     cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2)
mtext("Body size at shift", 1, cex = cexlab, line = 4.5)

lines(dt[(1:indx1),PSPMbifparcol], dt[(1:indx1),PSPMh2juvbiocol] + dt[(1:indx1),PSPMh2adubiocol], lwd = 2 * lwd, col = "#0072B2")
lines(dt[(indx1:indx2),PSPMbifparcol], dt[(indx1:indx2),PSPMh2juvbiocol] + dt[(indx1:indx2),PSPMh2adubiocol], lwd = lwd, lty = 3, col = "#0072B2")
lines(dt[(indx2:indx3),PSPMbifparcol], dt[(indx2:indx3),PSPMh2juvbiocol] + dt[(indx2:indx3),PSPMh2adubiocol], lwd = 2 * lwd, col = "#0072B2")
lines(dt[(indx3:nrow(dt)),PSPMbifparcol], dt[(indx3:nrow(dt)),PSPMh2juvbiocol] + dt[(indx3:nrow(dt)),PSPMh2adubiocol], lwd = lwd, col = "#0072B2", lty = 2)

lines(dt[(1:indx1),PSPMbifparcol], dt[(1:indx1),PSPMh3juvbiocol] + dt[(1:indx1),PSPMh3adubiocol], lwd = 2 * lwd, col = "#D55E00")
lines(dt[(indx1:indx2),PSPMbifparcol], dt[(indx1:indx2),PSPMh3juvbiocol] + dt[(indx1:indx2),PSPMh3adubiocol], lwd = lwd, lty = 3, col = "#D55E00")
lines(dt[(indx2:indx3),PSPMbifparcol], dt[(indx2:indx3),PSPMh3juvbiocol] + dt[(indx2:indx3),PSPMh3adubiocol], lwd = 2 * lwd, col = "#D55E00")
lines(dt[(indx3:nrow(dt)),PSPMbifparcol], dt[(indx3:nrow(dt)),PSPMh3juvbiocol] + dt[(indx3:nrow(dt)),PSPMh3adubiocol], lwd = lwd, col = "#D55E00", lty = 2)

lines(EBTupM[,EBTbifparcol], EBTupM[,EBTh2biocol], lwd = lwd, col = "#0072B2")
lines(EBTupM[,EBTbifparcol], EBTupM[,EBTh3biocol], lwd = lwd, col = "#D55E00")

points(dtbp[2,PSPMbifparcol], dtbp[2,PSPMh2juvbiocol] + dtbp[2,PSPMh2adubiocol], pch = 19, col = "#DD0000", cex = cexpnt)
points(dtbp[1,PSPMbifparcol],dtbp[1,PSPMh2juvbiocol] + dtbp[1,PSPMh2adubiocol], pch = 19, col = "#009E73", cex = cexpnt)
points(dtbp[3,PSPMbifparcol], dtbp[3,PSPMh2juvbiocol] + dtbp[3,PSPMh2adubiocol], pch = 19, col = "#009E73", cex = cexpnt)
# 
# points(dtbp[2,PSPMbifparcol], dtbp[2,PSPMh3juvbiocol] + dtbp[2,PSPMh3adubiocol], pch = 19, col = "#DD0000", cex = cexpnt)
points(dtbp[1,PSPMbifparcol],dtbp[1,PSPMh3juvbiocol] + dtbp[1,PSPMh3adubiocol], pch = 19, col = "#009E73", cex = cexpnt)
points(dtbp[3,PSPMbifparcol], dtbp[3,PSPMh3juvbiocol] + dtbp[3,PSPMh3adubiocol], pch = 19, col = "#009E73", cex = cexpnt)

legend("topright", c("Harvested area", "Marine reserve"), col = c("#0072B2", "#D55E00"), lwd = lwd, cex = cexleg)

########## Panel D

init=c(0.05, 0.4627769040, 0.3224126737, 0.5, 6)
pars <- DefaultPars
pars["SR"] <- 0.3
if (!exists("Equi_SR03_Ws")) {
  Equi_SR03_Ws <- PSPMequi(modelname = paste0(modeldir, "HarvestWithReserve"), 
                           biftype = "EQ", startpoint = init, 
                           stepsize = 0.01, parbnds = c(8, 0.05, 0.5), 
                           parameters = pars, clean=TRUE, 
                           options = c("popEVO", "0"))
}

dt          <- Equi_SR03_Ws$curvepoints
dtbp        <- Equi_SR03_Ws$bifpoints
dtbt        <- Equi_SR03_Ws$biftypes

EBTdt       <- read.table(paste0(datadir, "Figure3Dup.minmax.out"))
EBTmax      <- EBTdt[2 * (1:(nrow(EBTdt) / 2.0)),]
EBTmin      <- EBTdt[2 * (1:(nrow(EBTdt) / 2.0)) - 1,]

stable      <- EBTmax[, EBTperiodcol] > 1.0E-4
indx2       <- nrow(EBTmax)
indx1       <- min((1:indx2)[stable]) - 1
EBTmax_upS  <- EBTmax[(1:indx1),]
EBTmin_upS  <- EBTmin[(1:indx1),]
EBTupM      <- rbind(EBTmax[(indx2:indx1),], EBTmin[(indx1:indx2),])

EBTdt       <- read.table(paste0(datadir, "Figure3Ddwn.minmax.out"))
EBTmax      <- EBTdt[2 * (1:(nrow(EBTdt) / 2.0)),]
EBTmin      <- EBTdt[2 * (1:(nrow(EBTdt) / 2.0)) - 1,]

stable      <- EBTmax[, EBTperiodcol] > 1.0E-4
indx2       <- nrow(EBTmax)
indx1       <- max((1:indx2)[stable]) + 1
EBTmax_dwnS <- EBTmax[(indx1:indx2),]
EBTmin_dwnS <- EBTmin[(indx1:indx2),]
EBTdwnM     <- rbind(EBTmax[(1:indx1),], EBTmin[(indx1:1),])
stable      <- dt[, PSPMbifparcol] < min(EBTupM[, EBTbifparcol])

par(mar = c(0, 1, 4.0, 1))
plot(NULL, NULL, xlim = xlimw, ylim = ylim1, 
     xlab = "", ylab = "", xaxs = "i", yaxs = "i",
     xaxt = "n", yaxt = "n")
axis(1, label = F, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd)
axis(2, label = F, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2)
mtext("D: 30% protected", 3, cex = cexttl, line = 1.0)
# text(xlim1[2], ylimb[2], "A", cex = 4.0, xpd = T, adj = c(0.5, 0))

if (EBT_PSPM_cmp) {
  lines(EBTmax_upS[,EBTbifparcol],  EBTmax_upS[,EBTh1biocol],  lwd = 2 * lwd, col = "cyan1")
  lines(EBTmax_dwnS[,EBTbifparcol], EBTmax_dwnS[,EBTh1biocol], lwd = 2 * lwd, col = "red1")
  lines(EBTdwnM[,EBTbifparcol],     EBTdwnM[,EBTh1biocol],     lwd = 3 * lwd, col = "cyan1")
}

indx1 <- which.min(abs(dt[,PSPMbifparcol] - dtbp[1,PSPMbifparcol]))
indx2 <- which.min(abs(dt[,PSPMbifparcol] - dtbp[3,PSPMbifparcol]))
indx3 <- max((indx2:nrow(dt))[dt[(indx2:nrow(dt)),PSPMbifparcol] < min(EBTdwnM[,EBTbifparcol])])

lines(dt[(1:indx1),PSPMbifparcol], dt[(1:indx1),PSPMh1biocol], lwd = 2 * lwd, col = "black")
lines(dt[(indx1:indx2),PSPMbifparcol], dt[(indx1:indx2),PSPMh1biocol], lwd = lwd, lty = 3, col = "black")
lines(dt[(indx2:indx3),PSPMbifparcol], dt[(indx2:indx3),PSPMh1biocol], lwd = 2 * lwd, col = "black")
lines(dt[(indx3:nrow(dt)),PSPMbifparcol], dt[(indx3:nrow(dt)),PSPMh1biocol], lwd = lwd, col = "black", lty = 2)

lines(EBTdwnM[,EBTbifparcol], EBTdwnM[,EBTh1biocol], lwd = lwd, col = "black")


points(dtbp[2,PSPMbifparcol], dtbp[2,PSPMh1biocol], pch = 19, col = "#DD0000", cex = cexpnt)
points(dtbp[1,PSPMbifparcol], dtbp[1,PSPMh1biocol], pch = 19, col = "#009E73", cex = cexpnt)
points(dtbp[3,PSPMbifparcol], dtbp[3,PSPMh1biocol], pch = 19, col = "#009E73", cex = cexpnt)
# text(dtbp[2,PSPMbifparcol], dtbp[2,PSPMh1biocol], "ESS", cex = cexbt, adj = c(1.3, 0.45))
text(dtbp[1,PSPMbifparcol], dtbp[1,PSPMh1biocol], "LP", cex = cexbt, adj = c(1.3, -0.25))
text(dtbp[3,PSPMbifparcol], dtbp[3,PSPMh1biocol], "LP", cex = cexbt, adj = c(1.3, -0.25))

Arrows(0.09, 0.09, 0.16, 0.09, arr.type="triangle", lwd = 5, arr.width = 0.2, col = "#A10000")
Arrows(0.44, 1.0, 0.37, 1.0, arr.type="triangle", lwd = 5, arr.width = 0.2, col = "#A10000")

Arrows(dtbp[3,PSPMbifparcol] - 0.012, 0.79, 
       dtbp[3,PSPMbifparcol] - 0.012, 0.13, 
       arr.type="triangle", lwd = 3, arr.width = 0.2, col = "blue")
Arrows(dtbp[1,PSPMbifparcol] + 0.01, 0.96, 
       dtbp[3,PSPMbifparcol] + 0.00, 0.96, 
       arr.type="triangle", lwd = 3, arr.width = 0.2, col = "blue")
Arrows(dtbp[1,PSPMbifparcol] + 0.012, 0.10, 
       dtbp[1,PSPMbifparcol] + 0.012, 0.40, 
       arr.type="triangle", lwd = 3, arr.width = 0.2, col = "blue")
Arrows(dtbp[3,PSPMbifparcol] - 0.01, 0.06, 
       dtbp[1,PSPMbifparcol] + 0.00, 0.06, 
       arr.type="triangle", lwd = 3, arr.width = 0.2, col = "blue")
DrawEllipse(dtbp[1,PSPMbifparcol] + 0.012, 0.705, radius.x = 0.008, radius.y = 0.228, col = NA, lwd = 1.5, border = "#56B4E9")
Arrows(dtbp[1,PSPMbifparcol] + 0.0047, 0.65, 
       dtbp[1,PSPMbifparcol] + 0.0047, 0.64, 
       arr.type="triangle", lwd = 3, arr.width = 0.1, col = "#56B4E9")
Arrows(dtbp[1,PSPMbifparcol] + 0.0192, 0.77, 
       dtbp[1,PSPMbifparcol] + 0.0192, 0.78, 
       arr.type="triangle", lwd = 3, arr.width = 0.1, col = "#56B4E9")

par(mar = c(6, 1, 0, 1))
plot(NULL, NULL, xlim = xlimw, ylim = ylim2, 
     xlab = "", ylab = "", xaxs = "i", yaxs = "i",
     xaxt = "n", yaxt = "n")
axis(1, at = 0.15 + (0:2) * 0.1, label = F, 
     cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, tcl = 0.4)
axis(1, at = 0.1 + (0:3) * 0.1, label = c("0.1", "0.2", "0.3", "0.4"), 
     cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd)
axis(2, at = 0.05 + (0:3) * 0.1, label = F, 
     cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2, tcl = 0.4)
axis(2, at = (0:3) * 0.1, label = F, 
     cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2)
mtext("Body size at shift", 1, cex = cexlab, line = 4.5)

lines(dt[(1:indx1),PSPMbifparcol], dt[(1:indx1),PSPMh2juvbiocol] + dt[(1:indx1),PSPMh2adubiocol], lwd = 2 * lwd, col = "#0072B2")
lines(dt[(indx1:indx2),PSPMbifparcol], dt[(indx1:indx2),PSPMh2juvbiocol] + dt[(indx1:indx2),PSPMh2adubiocol], lwd = lwd, lty = 3, col = "#0072B2")
lines(dt[(indx2:indx3),PSPMbifparcol], dt[(indx2:indx3),PSPMh2juvbiocol] + dt[(indx2:indx3),PSPMh2adubiocol], lwd = 2 * lwd, col = "#0072B2")
lines(dt[(indx3:nrow(dt)),PSPMbifparcol], dt[(indx3:nrow(dt)),PSPMh2juvbiocol] + dt[(indx3:nrow(dt)),PSPMh2adubiocol], lwd = lwd, col = "#0072B2", lty = 2)

lines(dt[(1:indx1),PSPMbifparcol], dt[(1:indx1),PSPMh3juvbiocol] + dt[(1:indx1),PSPMh3adubiocol], lwd = 2 * lwd, col = "#D55E00")
lines(dt[(indx1:indx2),PSPMbifparcol], dt[(indx1:indx2),PSPMh3juvbiocol] + dt[(indx1:indx2),PSPMh3adubiocol], lwd = lwd, lty = 3, col = "#D55E00")
lines(dt[(indx2:indx3),PSPMbifparcol], dt[(indx2:indx3),PSPMh3juvbiocol] + dt[(indx2:indx3),PSPMh3adubiocol], lwd = 2 * lwd, col = "#D55E00")
lines(dt[(indx3:nrow(dt)),PSPMbifparcol], dt[(indx3:nrow(dt)),PSPMh3juvbiocol] + dt[(indx3:nrow(dt)),PSPMh3adubiocol], lwd = lwd, col = "#D55E00", lty = 2)

lines(EBTdwnM[,EBTbifparcol], EBTdwnM[,EBTh2biocol], lwd = lwd, col = "#0072B2")
lines(EBTdwnM[,EBTbifparcol], EBTdwnM[,EBTh3biocol], lwd = lwd, col = "#D55E00")

points(dtbp[2,PSPMbifparcol], dtbp[2,PSPMh2juvbiocol] + dtbp[2,PSPMh2adubiocol], pch = 19, col = "#DD0000", cex = cexpnt)
points(dtbp[1,PSPMbifparcol],dtbp[1,PSPMh2juvbiocol] + dtbp[1,PSPMh2adubiocol], pch = 19, col = "#009E73", cex = cexpnt)
points(dtbp[3,PSPMbifparcol], dtbp[3,PSPMh2juvbiocol] + dtbp[3,PSPMh2adubiocol], pch = 19, col = "#009E73", cex = cexpnt)
# 
# points(dtbp[2,PSPMbifparcol], dtbp[2,PSPMh3juvbiocol] + dtbp[2,PSPMh3adubiocol], pch = 19, col = "#DD0000", cex = cexpnt)
points(dtbp[1,PSPMbifparcol],dtbp[1,PSPMh3juvbiocol] + dtbp[1,PSPMh3adubiocol], pch = 19, col = "#009E73", cex = cexpnt)
points(dtbp[3,PSPMbifparcol], dtbp[3,PSPMh3juvbiocol] + dtbp[3,PSPMh3adubiocol], pch = 19, col = "#009E73", cex = cexpnt)

if (ToPdf) {
  dev.off()
  system(paste("open ", fname))
}

