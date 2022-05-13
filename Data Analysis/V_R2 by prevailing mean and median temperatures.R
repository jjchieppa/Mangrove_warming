# start ####

library(lattice);library(doBy); library(lubridate)
library(minpack.lm); library(effects); library(plotrix)
library(car); library(sjPlot); library(suncalc)
library(chron); library(reshape)

rm(list=ls())
dev.off()
cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

setwd("C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis")

# in data ####
df<-read.csv("Predictions from prevailing conditions (39 days).csv")

# start ####

tiff(file = "V_Rsq by cumulative time (days, nights, total) 39days.tiff", height = 6, width = 8, res = 600, units = "in", compression = "zip+p")


par(mfrow = c(2,2), omi = c(0.4,0.4,0.1,0.1), mar = c(1,2,1,1))

# ag Rarea ####

plot(1~0,xlim=c(0,40), ylim = c(0.1,0.3),axes=F)
axis(1, at = seq(0,40,5), labels = F)
axis(2, at = seq(0,0.3,0.05), cex.axis = 1.1, las = 2); box()

dat<-subset(df, spp == "ag" & r_calc == "rarea"
            & env == "T" & calc == "x" & tod == "t")
points(dat$r2 ~ dat$days, type = "l", col = "grey50", lty = 1, lwd = 2)
dat<-subset(df, spp == "ag" & r_calc == "rarea"
            & env == "T" & calc == "x" & tod == "n")
points(dat$r2 ~ dat$days, type = "l", col = "black", lty = 1, lwd = 2)
dat<-subset(df, spp == "ag" & r_calc == "rarea"
            & env == "T" & calc == "x" & tod == "d")
points(dat$r2 ~ dat$days, type = "l", col = "firebrick", lty = 1, lwd = 2)

dat<-subset(df, spp == "ag" & r_calc == "rarea"
            & env == "T" & calc == "m" & tod == "t")
points(dat$r2 ~ dat$days, type = "l", col = "grey50", lty = 2, lwd = 2)
dat<-subset(df, spp == "ag" & r_calc == "rarea"
            & env == "T" & calc == "m" & tod == "n")
points(dat$r2 ~ dat$days, type = "l", col = "black", lty = 2, lwd = 2)
dat<-subset(df, spp == "ag" & r_calc == "rarea"
            & env == "T" & calc == "m" & tod == "d")
points(dat$r2 ~ dat$days, type = "l", col = "firebrick", lty = 2, lwd = 2)

# rm Rarea ####

plot(1~0,xlim=c(0,40), ylim = c(0.1,0.3),axes=F)
axis(1, at = seq(0,40,5), labels = F)
axis(2, at = seq(0,0.3,0.05), cex.axis = 1.1, las = 2); box()

dat<-subset(df, spp == "rm" & r_calc == "rarea"
            & env == "T" & calc == "x" & tod == "t")
points(dat$r2 ~ dat$days, type = "l", col = "grey50", lty = 1, lwd = 2)
dat<-subset(df, spp == "rm" & r_calc == "rarea"
            & env == "T" & calc == "x" & tod == "n")
points(dat$r2 ~ dat$days, type = "l", col = "black", lty = 1, lwd = 2)
dat<-subset(df, spp == "rm" & r_calc == "rarea"
            & env == "T" & calc == "x" & tod == "d")
points(dat$r2 ~ dat$days, type = "l", col = "firebrick", lty = 1, lwd = 2)

dat<-subset(df, spp == "rm" & r_calc == "rarea"
            & env == "T" & calc == "m" & tod == "t")
points(dat$r2 ~ dat$days, type = "l", col = "grey50", lty = 2, lwd = 2)
dat<-subset(df, spp == "rm" & r_calc == "rarea"
            & env == "T" & calc == "m" & tod == "n")
points(dat$r2 ~ dat$days, type = "l", col = "black", lty = 2, lwd = 2)
dat<-subset(df, spp == "rm" & r_calc == "rarea"
            & env == "T" & calc == "m" & tod == "d")
points(dat$r2 ~ dat$days, type = "l", col = "firebrick", lty = 2, lwd = 2)
# ag Rmass ####

plot(1~0,xlim=c(0,40), ylim = c(0,0.1),axes=F)
axis(1, at = seq(0,40,5), cex.axis = 1.1)
axis(2, at = seq(0,0.3,0.025), cex.axis = 1.1, las = 2); box()

dat<-subset(df, spp == "ag" & r_calc == "rmass"
            & env == "T" & calc == "x" & tod == "t")
points(dat$r2 ~ dat$days, type = "l", col = "grey50", lty = 1, lwd = 2)
dat<-subset(df, spp == "ag" & r_calc == "rmass"
            & env == "T" & calc == "x" & tod == "n")
points(dat$r2 ~ dat$days, type = "l", col = "black", lty = 1, lwd = 2)
dat<-subset(df, spp == "ag" & r_calc == "rmass"
            & env == "T" & calc == "x" & tod == "d")
points(dat$r2 ~ dat$days, type = "l", col = "firebrick", lty = 1, lwd = 2)

dat<-subset(df, spp == "ag" & r_calc == "rmass"
            & env == "T" & calc == "m" & tod == "t")
points(dat$r2 ~ dat$days, type = "l", col = "grey50", lty = 2, lwd = 2)
dat<-subset(df, spp == "ag" & r_calc == "rmass"
            & env == "T" & calc == "m" & tod == "n")
points(dat$r2 ~ dat$days, type = "l", col = "black", lty = 2, lwd = 2)
dat<-subset(df, spp == "ag" & r_calc == "rmass"
            & env == "T" & calc == "m" & tod == "d")
points(dat$r2 ~ dat$days, type = "l", col = "firebrick", lty = 2, lwd = 2)

# rm Rmass ####

plot(1~0,xlim=c(0,40), ylim = c(0,0.15),axes=F)
axis(1, at = seq(0,40,5), cex.axis = 1.1)
axis(2, at = seq(0,0.3,0.05), cex.axis = 1.1, las = 2); box()

dat<-subset(df, spp == "rm" & r_calc == "rmass"
            & env == "T" & calc == "x" & tod == "t")
points(dat$r2 ~ dat$days, type = "l", col = "grey50", lty = 1, lwd = 2)
dat<-subset(df, spp == "rm" & r_calc == "rmass"
            & env == "T" & calc == "x" & tod == "n")
points(dat$r2 ~ dat$days, type = "l", col = "black", lty = 1, lwd = 2)
dat<-subset(df, spp == "rm" & r_calc == "rmass"
            & env == "T" & calc == "x" & tod == "d")
points(dat$r2 ~ dat$days, type = "l", col = "firebrick", lty = 1, lwd = 2)

dat<-subset(df, spp == "rm" & r_calc == "rmass"
            & env == "T" & calc == "m" & tod == "t")
points(dat$r2 ~ dat$days, type = "l", col = "grey50", lty = 2, lwd = 2)
dat<-subset(df, spp == "rm" & r_calc == "rmass"
            & env == "T" & calc == "m" & tod == "n")
points(dat$r2 ~ dat$days, type = "l", col = "black", lty = 2, lwd = 2)
dat<-subset(df, spp == "rm" & r_calc == "rmass"
            & env == "T" & calc == "m" & tod == "d")
points(dat$r2 ~ dat$days, type = "l", col = "firebrick", lty = 2, lwd = 2)
# off ####
mtext(side = 1, "Cumulative Time", outer = T, padj = 1.3, cex = 1.3)
mtext(side = 2, expression(italic(R)^2), outer = T, padj = -0.5, cex = 1.3)

dev.off()