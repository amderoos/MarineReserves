datadir <- "~/Projects/MarineReserves/Paper/EBToutput/"
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

fname <- "Figure2.pdf"

if (ToPdf) pdf(file = fname, width = (44.0 / 2.54), height = 10.0)

layout(matrix(1:8, nrow = 4, ncol = 2), heights = rep(c(1, 0.7), 4))

xliml <- c(0, 300)
xlimr <- c(0, 7000)
ylimb <- c(-0.04, 1.2)
ylims <- c(0.18, 0.45)

cexlab <- 2.0
cexaxs <- 2.0
cexleg <- 2.5

axislwd <- 1

par(tcl = 0.5, mgp = c(3, 0.8, 0))

########## Panel A
dt <- read.table(paste0(datadir, "Figure2A-GV01.out"))

par(mar = c(0, 10, 4.0, 2))
plot(NULL, NULL, xlim = xliml, ylim = ylimb, 
     xlab = "", ylab = "", xaxs = "i", yaxs = "i", 
     xaxt = "n", yaxt = "n", bty = "l")
axis(1, label = F, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd)
axis(2, at = (0:3) * 0.4, label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2)
mtext("Biomass", 2, cex = cexlab, line = 6)
mtext("No evolution", 3, cex = cexleg, line = 1.0)
text(xliml[2], ylimb[2], "A", cex = 4.0, xpd = T, adj = c(0.5, 0))

polygon(c(0, 50, 50, 0), par("usr")[c(3, 3, 4, 4)], col = "lightgrey")
lines(dt[,tcol], dt[,h1biocol], lwd = lwd, col = "black")
lines(dt[,tcol], dt[,h2biocol], lwd = lwd, col = "#0072B2")
lines(dt[,tcol], dt[,h3biocol], lwd = lwd, col = "#D55E00")

par(mar = c(3.5, 10, 0, 2))
plot(NULL, NULL, xlim = xliml, ylim = ylims, 
     xlab = "", ylab = "", xaxs = "i", yaxs = "i",
     xaxt = "n", yaxt = "n", bty = "l")
axis(1, label = T,                        cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd)
axis(2, at = c(0.2, 0.3, 0.4), label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2)
mtext("Body size\nat shift", 2, cex = cexlab, line = 4.5)

polygon(c(0, 50, 50, 0), par("usr")[c(3, 3, 4, 4)], col = "lightgrey")
lines(dt[,tcol], dt[,smoltsizecol], lwd = lwd, col = "#009E73")

########## Panel B

dt <- read.table(paste0(datadir, "Figure2B-GV01.out"))

par(mar = c(0, 10, 2.5, 2))
plot(NULL, NULL, xlim = xliml, ylim = ylimb, 
     xlab = "", ylab = "", xaxs = "i", yaxs = "i", 
     xaxt = "n", yaxt = "n", bty = "l")
axis(1, label = F, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd)
axis(2, at = (0:3) * 0.4, label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2)
mtext("Biomass", 2, cex = cexlab, line = 6)
text(xliml[2], ylimb[2], "B", cex = 4.0, xpd = T, adj = c(0.5, 0))

polygon(c(0, 50, 50, 0), par("usr")[c(3, 3, 4, 4)], col = "lightgrey")
lines(dt[,tcol], dt[,h1biocol], lwd = lwd, col = "black")
lines(dt[,tcol], dt[,h2biocol], lwd = lwd, col = "#0072B2")
lines(dt[,tcol], dt[,h3biocol], lwd = lwd, col = "#D55E00")

par(mar = c(5.0, 10, 0, 2))
plot(NULL, NULL, xlim = xliml, ylim = ylims, 
     xlab = "", ylab = "", xaxs = "i", yaxs = "i",
     xaxt = "n", yaxt = "n", bty = "l")
axis(1, label = T,                        cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd)
axis(2, at = c(0.2, 0.3, 0.4), label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2)
mtext("Time", 1, cex = cexlab, line = 4.0)
mtext("Body size\nat shift", 2, cex = cexlab, line = 4.5)

polygon(c(0, 50, 50, 0), par("usr")[c(3, 3, 4, 4)], col = "lightgrey")
lines(dt[,tcol], dt[,smoltsizecol], lwd = lwd, col = "#009E73")

########## Panel C

dt <- read.table(paste0(datadir, "Figure2C-GV01.out"))
dt <- rbind(dt[1,], dt)
dt[1,1] <- -300
dt[,1]  <- dt[,1] + 300

par(mar = c(0, 5, 4.0, 7))
plot(NULL, NULL, xlim = xlimr, ylim = ylimb, 
     xlab = "", ylab = "", xaxs = "i", yaxs = "i", 
     xaxt = "n", yaxt = "n", bty = "l")
axis(1, label = F, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd)
axis(2, at = (0:3) * 0.4, label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2)
mtext("With evolution", 3, cex = cexleg, line = 1.0)
text(xlimr[2], ylimb[2], "C", cex = 4.0, xpd = T, adj = c(0.5, 0))
mtext("10% protected", 4, cex = cexleg, line = 5, at = 0.3)

polygon(c(0, 500, 500, 0), par("usr")[c(3, 3, 4, 4)], col = "lightgrey")
lines(dt[,tcol], dt[,h1biocol], lwd = lwd, col = "black")
lines(dt[,tcol], dt[,h2biocol], lwd = lwd, col = "#0072B2")
lines(dt[,tcol], dt[,h3biocol], lwd = lwd, col = "#D55E00")

par(mar = c(3.5, 5, 0, 7))
plot(NULL, NULL, xlim = xlimr, ylim = ylims, 
     xlab = "", ylab = "", xaxs = "i", yaxs = "i",
     xaxt = "n", yaxt = "n", bty = "l")
axis(1, label = T,                        cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd)
axis(2, at = c(0.2, 0.3, 0.4), label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2)

polygon(c(0, 500, 500, 0), par("usr")[c(3, 3, 4, 4)], col = "lightgrey")
lines(dt[,tcol], dt[,smoltsizecol], lwd = lwd, col = "#009E73")

########## Panel D

dt <- read.table(paste0(datadir, "Figure2D-GV01.out"))
dt <- rbind(dt[1,], dt)
dt[1,1] <- -300
dt[,1]  <- dt[,1] + 300

par(mar = c(0, 5, 2.5, 7))
plot(NULL, NULL, xlim = xlimr, ylim = ylimb, 
     xlab = "", ylab = "", xaxs = "i", yaxs = "i", 
     xaxt = "n", yaxt = "n", bty = "l")
axis(1, label = F, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd)
axis(2, at = (0:3) * 0.4, label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2)
text(xlimr[2], ylimb[2], "D", cex = 4.0, xpd = T, adj = c(0.5, 0))
mtext("30% protected", 4, cex = cexleg, line = 5, at = 0.3)

polygon(c(0, 500, 500, 0), par("usr")[c(3, 3, 4, 4)], col = "lightgrey")
lines(dt[,tcol], dt[,h1biocol], lwd = lwd, col = "black")
lines(dt[,tcol], dt[,h2biocol], lwd = lwd, col = "#0072B2")
lines(dt[,tcol], dt[,h3biocol], lwd = lwd, col = "#D55E00")

par(mar = c(5.0, 5, 0, 7))
plot(NULL, NULL, xlim = xlimr, ylim = ylims, 
     xlab = "", ylab = "", xaxs = "i", yaxs = "i",
     xaxt = "n", yaxt = "n", bty = "l")
axis(1, label = T,                        cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd)
axis(2, at = c(0.2, 0.3, 0.4), label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2)
mtext("Time", 1, cex = cexlab, line = 4.0)

polygon(c(0, 500, 500, 0), par("usr")[c(3, 3, 4, 4)], col = "lightgrey")
lines(dt[,tcol], dt[,smoltsizecol], lwd = lwd, col = "#009E73")

if (ToPdf) {
  dev.off()
  system(paste("open ", fname))
}


# dt <- read.table("EBToutput/Figure2B.out")
# plot(dt[,tcol], dt[,h1biocol], type = "l", xlim = c(900, 1200))
