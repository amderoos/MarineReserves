basedir <- "~/Projects/MarineReserves/"
figdir  <- paste0(basedir, "Figures/")
modeldir <- paste0(basedir, "Equi/")
datadir <- paste0(figdir, "EBToutput/")
fname <- paste0(figdir, "appendixFigS1.pdf")
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
EBTbifjuvyield  <- 24
EBTbifaduyield  <- 25
EBTbiftotyield  <- 26
EBTbifparcol    <- 27
EBTperiodcol    <- 28

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

DefaultPars <- c(Rho1 = 0.5, Rho2 =    0.5, Delta = 1.0, 
                 Eta1 = 1.0, Eta2 =    0.8, Eta3  = 0.8, 
                 ETSJ = 1.5, ETSA =    1.5, WS    = 0.3735478389, 
                 Q    = 1.0, Beta = 2000.0, SR    = 0.0)

if (ToPdf) pdf(file = fname, width = 6, height = 8)

layout(matrix(1:2, nrow = 2, ncol = 1), heights = c(1.0, 1.1))

xlimc <- c(-0.05, 1.05)
ylim1 <- c(-0.05, 1.05)
ylim2 <- c(-0.02, 0.35)

cexlab <- 2.0
cexaxs <- 2.0
cexleg <- 1.2
cexttl <- 2.2
cexpnt <- 2.0
cexbt  <- 1.6

axislwd <- 1
baselwd <- 3
unstablelty <- 2

par(tcl = 0.6, mgp = c(3, 1, 0))

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

EBTdt       <- read.table(paste0(datadir, "Ecobif-RSup.minmax.out"))
EBTmax      <- EBTdt[2 * (1:(nrow(EBTdt) / 2.0)),]
EBTmin      <- EBTdt[2 * (1:(nrow(EBTdt) / 2.0)) - 1,]

stable      <- EBTmax[, EBTperiodcol] > 1.0E-4
indx2       <- nrow(EBTmax)
indx1       <- min((1:indx2)[stable]) - 1
EBTmax_upS  <- EBTmax[(1:indx1),]
EBTmin_upS  <- EBTmin[(1:indx1),]
EBTupM      <- rbind(EBTmax[(indx2:indx1),], EBTmin[(indx1:indx2),])

EBTdt       <- read.table(paste0(datadir, "Ecobif-RSdwn.minmax.out"))
EBTmax      <- EBTdt[2 * (1:(nrow(EBTdt) / 2.0)),]
EBTmin      <- EBTdt[2 * (1:(nrow(EBTdt) / 2.0)) - 1,]

stable      <- EBTmax[, EBTperiodcol] > 1.0E-4
indx2       <- nrow(EBTmax)
indx1       <- max((1:indx2)[stable]) + 1
EBTmax_dwnS <- EBTmax[(indx1:indx2),]
EBTmin_dwnS <- EBTmin[(indx1:indx2),]
EBTdwnM     <- rbind(EBTmax[(1:indx1),], EBTmin[(indx1:1),])
stable      <- dt[, PSPMbifparcol] < min(EBTupM[, EBTbifparcol])
  
par(mar = c(0, 8, 0.25, 0.25))
plot(NULL, NULL, xlim = xlimc, ylim = ylim1, 
     xlab = "", ylab = "", xaxs = "i", yaxs = "i",
     xaxt = "n", yaxt = "n")
axis(1, label = F, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd)
axis(2, label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2)
mtext("Biomass\nin nursery habitat", 2, cex = cexlab, line = 4.2)
# text(xlim1[2], ylimb[2], "A", cex = 4.0, xpd = T, adj = c(0.5, 0))

legend(par("usr")[2], 0.5 * (par("usr")[3] + par("usr")[4]), c("Stable equilibrium", "Unstable equilibrium", "Cycle minimum / maximum"), col = "black", lwd = c(2, 1, 1, 1) * baselwd, cex = cexleg, lty = c(1, unstablelty, 1), xjust = 1, yjust = 0)

if (EBT_PSPM_cmp) {
  lines(EBTmax_upS[,EBTbifparcol],  EBTmax_upS[,EBTh1biocol],  lwd = 2 * baselwd, col = "cyan1")
  lines(EBTmax_dwnS[,EBTbifparcol], EBTmax_dwnS[,EBTh1biocol], lwd = 2 * baselwd, col = "red1")
  lines(EBTdwnM[,EBTbifparcol],     EBTdwnM[,EBTh1biocol],     lwd = 3 * baselwd, col = "cyan1")
}

lines(dt[ stable,PSPMbifparcol], dt[ stable,PSPMh1biocol], lwd = 2 * baselwd, col = "black")
lines(dt[!stable,PSPMbifparcol], dt[!stable,PSPMh1biocol], lwd = baselwd, col = "black", lty = unstablelty)
lines(EBTupM[,EBTbifparcol], EBTupM[,EBTh1biocol], lwd = baselwd, col = "black")

par(mar = c(4.2, 8, 0, 0.25))
plot(NULL, NULL, xlim = xlimc, ylim = ylim2, 
     xlab = "", ylab = "", xaxs = "i", yaxs = "i",
     xaxt = "n", yaxt = "n")
axis(1, label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd)
axis(2, at = 0.05 + (0:3) * 0.1, label = F, 
     cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2, tcl = 0.4)
axis(2, at = (0:3) * 0.1, label = c("0.0", "0.1", "0.2", "0.3"), 
     cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2)
mtext("Biomass\nin growth habitat", 2, cex = cexlab, line = 4.2)
mtext("Marine reserve size", 1, cex = cexlab, line = 3.2)

legend("topright", c("Harvested area", "Marine reserve"), col = c("#0072B2", "#D55E00"), lwd = baselwd, cex = cexleg)

lines(dt[stable,PSPMbifparcol], dt[stable,PSPMh2juvbiocol] + dt[stable,PSPMh2adubiocol], lwd = 2 * baselwd, col = "#0072B2")
lines(dt[stable,PSPMbifparcol], dt[stable,PSPMh3juvbiocol] + dt[stable,PSPMh3adubiocol], lwd = 2 * baselwd, col = "#D55E00")

lines(dt[!stable,PSPMbifparcol], dt[!stable,PSPMh2juvbiocol] + dt[!stable,PSPMh2adubiocol], lwd = baselwd, lty = unstablelty, col = "#0072B2")
lines(dt[!stable,PSPMbifparcol], dt[!stable,PSPMh3juvbiocol] + dt[!stable,PSPMh3adubiocol], lwd = baselwd, lty = unstablelty, col = "#D55E00")

lines(EBTupM[,EBTbifparcol], EBTupM[,EBTh2biocol], lwd = baselwd, col = "#0072B2")
lines(EBTupM[,EBTbifparcol], EBTupM[,EBTh3biocol], lwd = baselwd, col = "#D55E00")

if (ToPdf) {
  dev.off()
  system(paste("open ", fname))
}

