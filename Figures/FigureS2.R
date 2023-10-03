basedir <- "~/Projects/MarineReserves/"
figdir  <- paste0(basedir, "Figures/")
modeldir <- paste0(basedir, "Equi/")
datadir <- paste0(figdir, "EBToutput/")
fname <- paste0(figdir, "appendixFigS2.pdf")
setwd(basedir)

ToPdf <- T

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
EBTbifparcol    <- 31
EBTperiodcol    <- 32

if (ToPdf) pdf(file = fname, width = (44.0 / 2.54), height = 11.0)

library(colorspace)

xlim <- c(0.5, 2.0)
ylim <- c(0, 2.0)

cexlab <- 2.0
cexaxs <- 2.0
cexleg <- 1.2
cexttl <- 2.2
cexpnt <- 2.0
cexbt  <- 1.6
ttlline <- 0.7
xlabline <- 4.0
ylabline <- 4.5

tmart <- 8
tmar  <- 3
lmar  <- 7
bmar  <- 6
rmar  <- 2.5

axislwd <- 1
baselwd <- 3
unstablelty <- 2

bmortvals <- seq(0.1, 2.5, 0.05)
fmortvals <- seq(0.0, 2.5, 0.05)
selectedcol <- EBTtotnumcol

zlabs   <- (0:7) * 0.5
zconts  <- seq(0, 2.0, by = 0.5)
zclrs   <- seq(0, 2.0, by = 0.01)
drawconts <- F

zlim    <- range(zconts) 
clrs <- hcl.colors(length(zclrs) - 1, "RdYlBu", rev = TRUE)

layout(matrix(1:8, nrow = 2, ncol = 4, byrow = T), heights = c(1.13, 1), widths = c(1, 1, 1, 0.15))

par(tcl = 0.6, mgp = c(3, 1, 0))

################################################## TOP ROW ##############################

SR00_ETSbif_Bmort_var <- matrix(NA, nrow=length(bmortvals), ncol = length(fmortvals))
SR01_ETSbif_Bmort_var <- matrix(NA, nrow=length(bmortvals), ncol = length(fmortvals))
SR03_ETSbif_Bmort_var <- matrix(NA, nrow=length(bmortvals), ncol = length(fmortvals))

for (ii in (1:length(bmortvals))) {
  fn <- paste0(sub('\\.', "", paste0(datadir, "Heatmaps-Eco/SR00-ETSbif-Bmort-", sprintf("%.2f", bmortvals[ii]))), ".var.out")
  if (file.size(fn) > 0) {
    dt <- read.table(fn)
    SR00_ETSbif_Bmort_var[ii, (1:nrow(dt))] <- dt[, selectedcol]
  }
  
  fn <- paste0(sub('\\.', "", paste0(datadir, "Heatmaps-Eco/SR01-ETSbif-Bmort-", sprintf("%.2f", bmortvals[ii]))), ".var.out")
  if (file.size(fn) > 0) {
    dt <- read.table(fn)
    SR01_ETSbif_Bmort_var[ii, (1:nrow(dt))] <- dt[, selectedcol]
  }
  
  fn <- paste0(sub('\\.', "", paste0(datadir, "Heatmaps-Eco/SR03-ETSbif-Bmort-", sprintf("%.2f", bmortvals[ii]))), ".var.out")
  if (file.size(fn) > 0) {
    dt <- read.table(fn)
    SR03_ETSbif_Bmort_var[ii, (1:nrow(dt))] <- dt[, selectedcol]
  }
}

par(mar = c(bmar, lmar, tmart, rmar))
plot(NULL, NULL, xlim = xlim, ylim = ylim, 
     xlab = "", ylab = "", xaxs = "i", yaxs = "i",
     xaxt = "n", yaxt = "n")

.filled.contour(bmortvals, fmortvals, SR00_ETSbif_Bmort_var, levels = zclrs, col = clrs)
if (drawconts) {
  contour(bmortvals, fmortvals, SR00_ETSbif_Bmort_var, levels = zconts, lwd = 0.1, add = TRUE, drawlabels = F)
  contour(bmortvals, fmortvals, SR00_ETSbif_Bmort_var, levels = zlabs, lwd = 1.5, add = TRUE, drawlabels = F)
}

axis(1, at = (0:5) * 0.5, label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd)
axis(2, at = (0:5) * 0.5, label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2)
mtext("Background mortality", 1, cex = cexlab, line = xlabline)
mtext("Fishing mortality", 2, cex = cexlab, line = ylabline)
mtext("No reserve", 3, cex = cexttl, line = ttlline)

par(mar = c(bmar, lmar, tmart, rmar))
plot(NULL, NULL, xlim = xlim, ylim = ylim, 
     xlab = "", ylab = "", xaxs = "i", yaxs = "i",
     xaxt = "n", yaxt = "n")

.filled.contour(bmortvals, fmortvals, SR01_ETSbif_Bmort_var, levels = zclrs, col = clrs)
if (drawconts) {
  contour(bmortvals, fmortvals, SR01_ETSbif_Bmort_var, levels = zconts, lwd = 0.1, add = TRUE, drawlabels = F)
  contour(bmortvals, fmortvals, SR01_ETSbif_Bmort_var, levels = zlabs, lwd = 1.5, add = TRUE, drawlabels = F)
}

axis(1, at = (0:5) * 0.5, label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd)
axis(2, at = (0:5) * 0.5, label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2)
mtext("Background mortality", 1, cex = cexlab, line = xlabline)
mtext("Fishing mortality", 2, cex = cexlab, line = ylabline)
mtext("10% protected", 3, cex = cexttl, line = ttlline)

mtext("Coefficient of variation in total number of individuals", 3, cex = cexttl, line = 5.0)

par(mar = c(bmar, lmar, tmart, rmar))
plot(NULL, NULL, xlim = xlim, ylim = ylim, 
     xlab = "", ylab = "", xaxs = "i", yaxs = "i",
     xaxt = "n", yaxt = "n")

.filled.contour(bmortvals, fmortvals, SR03_ETSbif_Bmort_var, levels = zclrs, col = clrs)
if (drawconts) {
  contour(bmortvals, fmortvals, SR03_ETSbif_Bmort_var, levels = zconts, lwd = 0.1, add = TRUE, drawlabels = F)
  contour(bmortvals, fmortvals, SR03_ETSbif_Bmort_var, levels = zlabs, lwd = 1.5, add = TRUE, drawlabels = F)
}

axis(1, at = (0:5) * 0.5, label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd)
axis(2, at = (0:5) * 0.5, label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2)
mtext("Background mortality", 1, cex = cexlab, line = xlabline)
mtext("Fishing mortality", 2, cex = cexlab, line = ylabline)
mtext("30% protected", 3, cex = cexttl, line = ttlline)

par(mar=c(bmar, 0, tmart, rmar), mgp = c(3, 0.3, 0))
plot.new()
plot.window(xlim = c(0, 1), ylim = zlim, xaxs = "i", yaxs = "i")
rect(0, zclrs[-length(zclrs)], 1, zclrs[-1L], density = -1, col = clrs, lwd = 0, border = "transparent")
rect(0, zconts[-length(zconts)], 1, zconts[-1L], col = NA, lwd = 0.1)
rect(0, zlabs[-length(zlabs)], 1, zlabs[-1L], col = NA, lwd = 1.5)
axis(4, at = zlabs, labels = T, cex.axis = 0.9 * cexaxs, las = 2, lwd = 0)
mtext("CV", 3, cex = 1.1, adj = 0.5, line = 0.75)

################################################## BOTTOM ROW ##############################

bmortvals <- seq(0.5, 5.0, 0.1)
fmortvals <- seq(-0.1, 2.5, 0.1)

SR00_ETSbif_Bmort_var <- matrix(NA, nrow=length(bmortvals), ncol = length(fmortvals))
SR01_ETSbif_Bmort_var <- matrix(NA, nrow=length(bmortvals), ncol = length(fmortvals))
SR03_ETSbif_Bmort_var <- matrix(NA, nrow=length(bmortvals), ncol = length(fmortvals))

for (ii in (1:length(bmortvals))) {
  fn <- paste0(sub('\\.', "", paste0(datadir, "Heatmaps-Evo/SR00-ETSbif-Bmort-", sprintf("%.1f", bmortvals[ii]))), ".var.out")
  if (file.size(fn) > 0) {
    dt <- read.table(fn)
    SR00_ETSbif_Bmort_var[ii, (1:nrow(dt))] <- dt[, selectedcol]
  }
  
  fn <- paste0(sub('\\.', "", paste0(datadir, "Heatmaps-Evo/SR01-ETSbif-Bmort-", sprintf("%.1f", bmortvals[ii]))), ".var.out")
  if (file.size(fn) > 0) {
    dt <- read.table(fn)
    SR01_ETSbif_Bmort_var[ii, (1:nrow(dt))] <- dt[, selectedcol]
  }
  
  fn <- paste0(sub('\\.', "", paste0(datadir, "Heatmaps-Evo/SR03-ETSbif-Bmort-", sprintf("%.1f", bmortvals[ii]))), ".var.out")
  if (file.size(fn) > 0) {
    dt <- read.table(fn)
    SR03_ETSbif_Bmort_var[ii, (1:nrow(dt))] <- dt[, selectedcol]
  }
}

par(tcl = 0.6, mgp = c(3, 1, 0))
par(mar = c(bmar, lmar, tmar, rmar))
plot(NULL, NULL, xlim = xlim, ylim = ylim, 
     xlab = "", ylab = "", xaxs = "i", yaxs = "i",
     xaxt = "n", yaxt = "n")

.filled.contour(bmortvals, fmortvals, SR00_ETSbif_Bmort_var, levels = zclrs, col = clrs)
if (drawconts) {
  contour(bmortvals, fmortvals, SR00_ETSbif_Bmort_var, levels = zconts, lwd = 0.1, add = TRUE, drawlabels = F)
  contour(bmortvals, fmortvals, SR00_ETSbif_Bmort_var, levels = zlabs, lwd = 1.5, add = TRUE, drawlabels = F)
}

axis(1, at = (0:5) * 0.5, label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd)
axis(2, at = (0:5) * 0.5, label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2)
mtext("Background mortality", 1, cex = cexlab, line = xlabline)
mtext("Fishing mortality", 2, cex = cexlab, line = ylabline)

par(mar = c(bmar, lmar, tmar, rmar))
plot(NULL, NULL, xlim = xlim, ylim = ylim, 
     xlab = "", ylab = "", xaxs = "i", yaxs = "i",
     xaxt = "n", yaxt = "n")

.filled.contour(bmortvals, fmortvals, SR01_ETSbif_Bmort_var, levels = zclrs, col = clrs)
if (drawconts) {
  contour(bmortvals, fmortvals, SR01_ETSbif_Bmort_var, levels = zconts, lwd = 0.1, add = TRUE, drawlabels = F)
  contour(bmortvals, fmortvals, SR01_ETSbif_Bmort_var, levels = zlabs, lwd = 1.5, add = TRUE, drawlabels = F)
}

axis(1, at = (0:5) * 0.5, label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd)
axis(2, at = (0:5) * 0.5, label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2)
mtext("Background mortality", 1, cex = cexlab, line = xlabline)
mtext("Fishing mortality", 2, cex = cexlab, line = ylabline)

par(mar = c(bmar, lmar, tmar, rmar))
plot(NULL, NULL, xlim = xlim, ylim = ylim, 
     xlab = "", ylab = "", xaxs = "i", yaxs = "i",
     xaxt = "n", yaxt = "n")

.filled.contour(bmortvals, fmortvals, SR03_ETSbif_Bmort_var, levels = zclrs, col = clrs)
if (drawconts) {
  contour(bmortvals, fmortvals, SR03_ETSbif_Bmort_var, levels = zconts, lwd = 0.1, add = TRUE, drawlabels = F)
  contour(bmortvals, fmortvals, SR03_ETSbif_Bmort_var, levels = zlabs, lwd = 1.5, add = TRUE, drawlabels = F)
}

axis(1, at = (0:5) * 0.5, label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd)
axis(2, at = (0:5) * 0.5, label = T, cex.axis = cexaxs, lwd = 0, lwd.ticks = axislwd, las = 2)
mtext("Background mortality", 1, cex = cexlab, line = xlabline)
mtext("Fishing mortality", 2, cex = cexlab, line = ylabline)

par(mar=c(bmar, 0, tmar, rmar), mgp = c(3, 0.3, 0))
plot.new()
plot.window(xlim = c(0, 1), ylim = zlim, xaxs = "i", yaxs = "i")
rect(0, zclrs[-length(zclrs)], 1, zclrs[-1L], density = -1, col = clrs, lwd = 0, border = "transparent")
rect(0, zconts[-length(zconts)], 1, zconts[-1L], col = NA, lwd = 0.1)
rect(0, zlabs[-length(zlabs)], 1, zlabs[-1L], col = NA, lwd = 1.5)
axis(4, at = zlabs, labels = T, cex.axis = 0.9 * cexaxs, las = 2, lwd = 0)
mtext("CV", 3, cex = 1.1, adj = 0.5, line = 0.75)

if (ToPdf) {
  dev.off()
  system(paste("open ", fname))
}

