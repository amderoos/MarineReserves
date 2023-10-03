basedir <- "~/Projects/MarineReserves/"
figdir  <- paste0(basedir, "Figures/")
datadir <- paste0(figdir, "EBToutput/")
fname <- paste0(figdir, "appendixFigS3.pdf")
setwd(basedir)

ToPdf <- T

tcol         <-  1
res1col      <-  2
res2col      <-  3
res3col      <-  4
totnumcol    <-  5
totbiocol    <-  6
h1numcol     <-  7
h1biocol     <-  8
h2numcol     <-  9
h2biocol     <- 10
h3numcol     <- 11
h3biocol     <- 12
h2juvnumcol  <- 13
h2juvbiocol  <- 14
h3juvnumcol  <- 15
h3juvbiocol  <- 16
h2adunumcol  <- 17
h2adubiocol  <- 18
h3adunumcol  <- 19
h3adubiocol  <- 20
smoltsizecol <- 21   
totreprocol  <- 22
numcohcol    <- 23
bifparcol    <- 24

if (ToPdf) pdf(file = fname, width = (44.0 / 2.54), height = 8.6)

xliml <- c(0, 300)
xlimr <- c(0, 300)
ylimb <- c(-0.04, 2.5)
ylimn <- c(-0.002, 0.08)
ylimr <- c(-0.02, 0.6)
ylims <- c(0.18, 0.45)

cexlab <- 2.0
cexaxs <- 2.0
cexttl <- 3.5
cexleg <- 1.5

axislwd <- 1
baselwd <- 3
panelsep <- 2.5
lmarl   <- 10.0
lmarr   <- 12.0
rmar    <- 2.0
tresest <- 100

par(tcl = 0.6, mgp = c(3, 0.8, 0))

library(shape)
library(igraph)

# layout(matrix(1:8, nrow = 4, ncol = 2), heights = c(1.3, 1, 1, 0.7), widths = c(1, (lmarr + rmar) / (lmarl + rmar)))
layout(matrix(1:6, nrow = 3, ncol = 2), heights = c(1.3, 1, 1.14), widths = c(1, (lmarr + rmar) / (lmarl + rmar)))

########## Left column
dt <- read.table(paste0(datadir, "Ecodyn-RS010-T0050-Eta1_05.out"))
dt <- dt[(dt[,tcol] >= tresest),]
dt <- rbind(dt[1,], dt)
dt[,tcol] <- dt[, tcol] + (50 - tresest) 
dt[1, tcol] <- 0.0

par(mar = c(panelsep, lmarl, 5.8, rmar))
plot(NULL, NULL, xlim = xliml, ylim = ylimb, 
     xlab = "", ylab = "", xaxs = "i", yaxs = "i", 
     xaxt = "n", yaxt = "n", bty = "l")
polygon(c(0, 50, 50, 0), par("usr")[c(3, 3, 4, 4)], col = "lightgrey")
lines(dt[,tcol], dt[,h1biocol], lwd = baselwd, col = "black")
lines(dt[,tcol], dt[,h2biocol], lwd = baselwd, col = "#0072B2")
lines(dt[,tcol], dt[,h3biocol], lwd = baselwd, col = "#D55E00")

axis(1, label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd)
# axis(2, at = 0.2 + (0:3) * 0.4, label = F, lwd = 0, lwd.ticks = axislwd, tcl = 0.4)
axis(2, at = (0:5) * 0.5, label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2)
mtext("Biomass", 2, cex = cexlab, line = 6)
Arrows( 5.0, 1.08 * par("usr")[4],  45.0, 1.08 * par("usr")[4], arr.type="triangle", lwd = 2, arr.width = 0.2, code = 3, xpd = T)
Arrows(55.0, 1.08 * par("usr")[4], 295.0, 1.08 * par("usr")[4], arr.type="triangle", lwd = 2, arr.width = 0.2, code = 3, xpd = T)
text(25, 1.30 * par("usr")[4], "No", cex = 2.0, xpd = T)
text(25, 1.18 * par("usr")[4], "reserve", cex = 2.0, xpd = T)
text(175, 1.25 * par("usr")[4], "10% protected", cex = cexttl, xpd = T)

legend(50, 1.015 * par("usr")[4], c("Nursery habitat", "Harvested area", "Marine reserve"), 
       col = c("black", "#0072B2", "#D55E00"), lwd = baselwd, cex = cexleg, horiz = T, bty = "n", adj = c(0, 0.5))
polygon(c(52, 300, 300, 52), c(rep(0.995, 2), rep(0.845, 2)) * par("usr")[4], col = NA, xpd = T)

par(mar = c(panelsep, lmarl, 0, rmar))
plot(NULL, NULL, xlim = xliml, ylim = ylimn, 
     xlab = "", ylab = "", xaxs = "i", yaxs = "i", 
     xaxt = "n", yaxt = "n", bty = "l")
polygon(c(0, 50, 50, 0), par("usr")[c(3, 3, 4, 4)], col = "lightgrey")
# lines(dt[,tcol], dt[,h2adunumcol] + dt[,h3adunumcol], lwd = baselwd, col = "black")
lines(dt[,tcol], dt[,h2adunumcol], lwd = baselwd, col = "#0072B2")
lines(dt[,tcol], dt[,h3adunumcol], lwd = baselwd, col = "#D55E00")

axis(1, label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd)
axis(2, at = 0.01 + (0:4) * 0.02, label = F, lwd = 0, lwd.ticks = axislwd, tcl = 0.4)
axis(2, at = (0:4) * 0.02, label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2, mgp = c(3, 0.4, 0))
mtext("Adult\nnumber", 2, cex = cexlab, line = 4.5)

par(mar = c(5.0, lmarl, 0, rmar))
plot(NULL, NULL, xlim = xliml, ylim = ylimr, 
     xlab = "", ylab = "", xaxs = "i", yaxs = "i", 
     xaxt = "n", yaxt = "n", bty = "l")
polygon(c(0, 50, 50, 0), par("usr")[c(3, 3, 4, 4)], col = "lightgrey")

lines(dt[,tcol], dt[,res1col], lwd = baselwd, col = "black")
lines(dt[,tcol], dt[,res2col], lwd = baselwd, col = "#0072B2")
lines(dt[,tcol], dt[,res3col], lwd = baselwd, col = "#D55E00")

axis(1, label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd)
axis(2, at = 0.1 + (0:3) * 0.2, label = F, lwd = 0, lwd.ticks = axislwd, tcl = 0.4)
axis(2, at = (0:3) * 0.2, label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2)
mtext("Time", 1, cex = cexlab, line = 3.75)
mtext("Resource\ndensity", 2, cex = cexlab, line = 4.5)

# par(mar = c(5.0, lmarl, 0, rmar))
# plot(NULL, NULL, xlim = xliml, ylim = ylims, 
#      xlab = "", ylab = "", xaxs = "i", yaxs = "i",
#      xaxt = "n", yaxt = "n", bty = "l")
# polygon(c(0, 50, 50, 0), par("usr")[c(3, 3, 4, 4)], col = "lightgrey")
# lines(dt[,tcol], dt[,smoltsizecol], lwd = baselwd, col = "#009E73")
# 
# axis(1, label = T,                        cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd)
# axis(2, at = c(0.2, 0.3, 0.4), label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2)
# mtext("Body size\nat shift", 2, cex = cexlab, line = 4.5)
# mtext("Time", 1, cex = cexlab, line = 3.5)

########## Right column

dt <- read.table(paste0(datadir, "Ecodyn-RS030-T0050-Eta1_05.out"))
dt <- dt[(dt[,tcol] >= tresest),]
dt <- rbind(dt[1,], dt)
dt[,tcol] <- dt[, tcol] + (50 - tresest) 
dt[1, tcol] <- 0.0

par(mar = c(panelsep, lmarr, 5.8, rmar))
plot(NULL, NULL, xlim = xliml, ylim = ylimb, 
     xlab = "", ylab = "", xaxs = "i", yaxs = "i", 
     xaxt = "n", yaxt = "n", bty = "l")
polygon(c(0, 50, 50, 0), par("usr")[c(3, 3, 4, 4)], col = "lightgrey")
lines(dt[,tcol], dt[,h1biocol], lwd = baselwd, col = "black")
lines(dt[,tcol], dt[,h2biocol], lwd = baselwd, col = "#0072B2")
lines(dt[,tcol], dt[,h3biocol], lwd = baselwd, col = "#D55E00")

axis(1, label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd)
# axis(2, at = 0.2 + (0:3) * 0.4, label = F, lwd = 0, lwd.ticks = axislwd, tcl = 0.4)
axis(2, at = (0:5) * 0.5, label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2)
mtext("Biomass", 2, cex = cexlab, line = 6)
Arrows( 5.0, 1.08 * par("usr")[4],  45.0, 1.08 * par("usr")[4], arr.type="triangle", lwd = 2, arr.width = 0.2, code = 3, xpd = T)
Arrows(55.0, 1.08 * par("usr")[4], 295.0, 1.08 * par("usr")[4], arr.type="triangle", lwd = 2, arr.width = 0.2, code = 3, xpd = T)
text(25, 1.30 * par("usr")[4], "No", cex = 2.0, xpd = T)
text(25, 1.18 * par("usr")[4], "reserve", cex = 2.0, xpd = T)
text(175, 1.25 * par("usr")[4], "30% protected", cex = cexttl, xpd = T)

par(mar = c(panelsep, lmarr, 0, rmar))
plot(NULL, NULL, xlim = xliml, ylim = ylimn, 
     xlab = "", ylab = "", xaxs = "i", yaxs = "i", 
     xaxt = "n", yaxt = "n", bty = "l")
polygon(c(0, 50, 50, 0), par("usr")[c(3, 3, 4, 4)], col = "lightgrey")
# lines(dt[,tcol], dt[,h2adunumcol] + dt[,h3adunumcol], lwd = baselwd, col = "black")
lines(dt[,tcol], dt[,h2adunumcol], lwd = baselwd, col = "#0072B2")
lines(dt[,tcol], dt[,h3adunumcol], lwd = baselwd, col = "#D55E00")

axis(1, label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd)
axis(2, at = 0.01 + (0:4) * 0.02, label = F, lwd = 0, lwd.ticks = axislwd, tcl = 0.4)
axis(2, at = (0:4) * 0.02, label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2, mgp = c(3, 0.4, 0))
mtext("Adult\nnumber", 2, cex = cexlab, line = 4.5)

par(mar = c(5.0, lmarr, 0, rmar))
plot(NULL, NULL, xlim = xliml, ylim = ylimr, 
     xlab = "", ylab = "", xaxs = "i", yaxs = "i", 
     xaxt = "n", yaxt = "n", bty = "l")
polygon(c(0, 50, 50, 0), par("usr")[c(3, 3, 4, 4)], col = "lightgrey")

lines(dt[,tcol], dt[,res1col], lwd = baselwd, col = "black")
lines(dt[,tcol], dt[,res2col], lwd = baselwd, col = "#0072B2")
lines(dt[,tcol], dt[,res3col], lwd = baselwd, col = "#D55E00")

axis(1, label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd)
axis(2, at = 0.1 + (0:3) * 0.2, label = F, lwd = 0, lwd.ticks = axislwd, tcl = 0.4)
axis(2, at = (0:3) * 0.2, label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2)
mtext("Time", 1, cex = cexlab, line = 3.75)
mtext("Resource\ndensity", 2, cex = cexlab, line = 4.5)

# par(mar = c(5.0, lmarr, 0, rmar))
# plot(NULL, NULL, xlim = xliml, ylim = ylims, 
#      xlab = "", ylab = "", xaxs = "i", yaxs = "i",
#      xaxt = "n", yaxt = "n", bty = "l")
# polygon(c(0, 50, 50, 0), par("usr")[c(3, 3, 4, 4)], col = "lightgrey")
# lines(dt[,tcol], dt[,smoltsizecol], lwd = baselwd, col = "#009E73")
# 
# axis(1, label = T,                        cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd)
# axis(2, at = c(0.2, 0.3, 0.4), label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2)
# mtext("Time", 1, cex = cexlab, line = 3.5)
# mtext("Body size\nat shift", 2, cex = cexlab, line = 4.5)

if (ToPdf) {
  dev.off()
  system(paste("open ", fname))
}
