# in data ####

library(doBy); library(lubridate); library(car)
library(effects); library(sjPlot); library(plotrix)

rm(list=ls())
dev.off()
cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

setwd("C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis")

df<-read.csv("Mangrove FLvBZ long-term Q10 groups 21C.csv")
df$rep<-as.factor(df$rep)

reps<-unique(df$rep)

# start ####

par(mfrow = c(2,1), omi = c(1,0.6,0.01,0.01), mar = c(0.5,1,0.5,1))

# AG ####

dat<-subset(df, Species == "AG")
reps<-unique(dat$rep)

plot(1~0, pch = NA, xlim = c(0, 25), ylim = c(0,4.1), axes = F, xlab="",ylab=""); box()
axis(2, seq(0,4,1), las = 2, cex.axis = 1.1)
axis(1, at = dat$x+0.5, labels = F)

for (reps in unique(dat$rep)){
  dum<-subset(dat, dat$rep == reps)
  rect(dum$x, 0, dum$x+1, dum$Q)
  ablineclip(v = dum$x+0.5, y1 = dum$Q.lc, y2 = dum$Q.uc)
}

ablineclip(v = 12.5, lty = 2)
text(6.25, 3.8, expression(italic(R)[area]), cex = 1.2)
text(18.25, 3.8, expression(italic(R)[mass]), cex = 1.2)
mtext(side = 2, expression('Long-term'~italic(Q)[10]), cex = 1.2, padj = -2)
legend("topleft", bty = "n", "a) AG", adj = 0.5)

ablineclip(h = 1.5, x1=0,x2=12.25, lty = 2, col = "firebrick")
ablineclip(h = 1.5, x1=12.75,x2=25, lty = 2, col = "firebrick")

# RM ####

dat<-subset(df, Species == "RM")
reps<-unique(dat$rep)

plot(1~0, pch = NA, xlim = c(0, 25), ylim = c(0,4.1), axes = F, xlab="",ylab=""); box()
axis(2, seq(0,4,1), las = 2, cex.axis = 1.1)
axis(1, at = dat$x+0.5, labels = F)

for (reps in unique(dat$rep)){
  dum<-subset(dat, dat$rep == reps)
  rect(dum$x, 0, dum$x+1, dum$Q)
  ablineclip(v = dum$x+0.5, y1 = dum$Q.lc, y2 = dum$Q.uc)
}

ablineclip(v = 12.5, lty = 2)
text(6.25, 3.8, expression(italic(R)[area]), cex = 1.2)
text(18.25, 3.8, expression(italic(R)[mass]), cex = 1.2)
mtext(side = 2, expression('Long-term'~italic(Q)[10]), cex = 1.2, padj = -2)
legend("topleft", bty = "n", "b) RM", adj = 0.5)

ablineclip(h = 1.5, x1=0,x2=12.25, lty = 2, col = "firebrick")
ablineclip(h = 1.5, x1=12.75,x2=25, lty = 2, col = "firebrick")