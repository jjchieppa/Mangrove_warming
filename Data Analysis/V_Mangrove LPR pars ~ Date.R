# start ####

library(lattice);library(doBy); library(lubridate)
library(minpack.lm); library(effects); library(plotrix)
library(car); library(sjPlot)

rm(list=ls())
dev.off()
cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

home<-"C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis"
work<-"C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp" # work
raw<-"C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/raw"
hist<-"C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/hist"
lins<-"C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/lins"
deets<-"C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/deets"
env<-"C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Weather Data"
anly<-"C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/Analysis"

# data in ####

### Bring in the ref respiration data, bind area and mass for LMA

setwd(deets)
source<-read.csv("MangroveRvT_LPRmod_Rarea and Rmass at 25C.csv")
source$Date<-as.Date(source$Date, format = "%Y-%m-%d")
setwd(work)
trts<-read.csv("Mangrove Treatment Assignments.csv")
source<-merge(source, trts, by = "UserIDs_in", all = F)
rm(trts)

source$Date<-as.Date(source$Date, format = "%Y-%m-%d") # Double checks date format!
source$MoYr<-floor_date(source$Date, "month")
source$MoYr<-source$MoYr+25
source$LMA_g.m2<-source$LMA_kg.m2*100
source$Timepoint<-as.factor(source$MoYr)

start<-as.Date("2020-04-15")
end<-as.Date("2020-12-27")

# start plot ####

setwd("C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis")

tiff(file = "V_Mangrove LPR Rarea pars ~ Date.tiff", height = 8, width = 12, res = 600, units = "in", compression = "zip+p")

par(mfrow = c(3,2), omi = c(0.75,0.5,0.1,0.01), mar = c(0.8,1.7,0.8,2))
source<-subset(source, R_calc == "R.area")

# a AG ####

df<-subset(source, Species == "ag")
plot(df$CTleaf.slope ~ df$Date, pch = NA, xaxt="n",yaxt="n",xlab="",ylab="", ylim = c(-0.1,0.55))
axis(2, at = seq(-0.1,0.5,0.1), las = 2)
axis.Date(1, df$MoYr, at = seq(start, end, "month"), las = 2, format = "%m/%Y", labels = F)

legend("topleft", "a)", bty = "n", cex = 1.5)

dat<-subset(df, Treatment == "Ambient" & Source == "bz")
points(dat$CTleaf.slope ~ dat$Date, pch = 6, col = "blue")
dat<-subset(df, Treatment == "Warmed" & Source == "bz")
points(dat$CTleaf.slope ~ dat$Date, pch = 6, col = "firebrick")
dat<-subset(df, Treatment == "Ambient" & Source == "fl")
points(dat$CTleaf.slope ~ dat$Date, pch = 2, col = "blue")
dat<-subset(df, Treatment == "Warmed" & Source == "fl")
points(dat$CTleaf.slope ~ dat$Date, pch = 2, col = "firebrick")

mtext(side = 2, expression(italic(a)), cex = 1.2, padj = -4)

legend("bottomright", horiz = T, c("Ambient","Warmed","Florida","Belize"),
       pch = c(16,16,2,6), bty = "n", col = c("blue","firebrick","black","black"))

# a RM ####

df<-subset(source, Species == "rm")
plot(df$CTleaf.slope ~ df$Date, pch = NA, xaxt="n",yaxt="n",xlab="",ylab="", ylim = c(-0.1,0.4))
axis(2, at = seq(-0.1,0.4,0.1), las = 2)
axis.Date(1, df$MoYr, at = seq(start, end, "month"), las = 2, format = "%m/%Y", labels = F)

legend("topleft", "b)", bty = "n", cex = 1.5)

dat<-subset(df, Treatment == "Ambient" & Source == "bz")
points(dat$CTleaf.slope ~ dat$Date, pch = 6, col = "blue")
dat<-subset(df, Treatment == "Warmed" & Source == "bz")
points(dat$CTleaf.slope ~ dat$Date, pch = 6, col = "firebrick")
dat<-subset(df, Treatment == "Ambient" & Source == "fl")
points(dat$CTleaf.slope ~ dat$Date, pch = 2, col = "blue")
dat<-subset(df, Treatment == "Warmed" & Source == "fl")
points(dat$CTleaf.slope ~ dat$Date, pch = 2, col = "firebrick")
# b AG ####

df<-subset(source, Species == "ag")
plot(df$CTleaf2.slope ~ df$Date, pch = NA, xaxt="n",yaxt="n",xlab="",ylab="", ylim = c(-0.007,0.003))
axis(2, at = seq(-0.1,0.5,0.002), las = 2)
axis.Date(1, df$MoYr, at = seq(start, end, "month"), las = 2, format = "%m/%Y", labels = F)

legend("topleft", "c)", bty = "n", cex = 1.5)

dat<-subset(df, Treatment == "Ambient" & Source == "bz")
points(dat$CTleaf2.slope ~ dat$Date, pch = 6, col = "blue")
dat<-subset(df, Treatment == "Warmed" & Source == "bz")
points(dat$CTleaf2.slope ~ dat$Date, pch = 6, col = "firebrick")
dat<-subset(df, Treatment == "Ambient" & Source == "fl")
points(dat$CTleaf2.slope ~ dat$Date, pch = 2, col = "blue")
dat<-subset(df, Treatment == "Warmed" & Source == "fl")
points(dat$CTleaf2.slope ~ dat$Date, pch = 2, col = "firebrick")

mtext(side = 2, expression(italic(b)), cex = 1.2, padj = -3.5)

# b RM ####

df<-subset(source, Species == "rm")
plot(df$CTleaf2.slope ~ df$Date, pch = NA, xaxt="n",yaxt="n",xlab="",ylab="", ylim = c(-0.005,0.003))
axis(2, at = seq(-0.1,0.4,0.002), las = 2)
axis.Date(1, df$MoYr, at = seq(start, end, "month"), las = 2, format = "%m/%Y", labels = F)

legend("topleft", "d)", bty = "n", cex = 1.5)

dat<-subset(df, Treatment == "Ambient" & Source == "bz")
points(dat$CTleaf2.slope ~ dat$Date, pch = 6, col = "blue")
dat<-subset(df, Treatment == "Warmed" & Source == "bz")
points(dat$CTleaf2.slope ~ dat$Date, pch = 6, col = "firebrick")
dat<-subset(df, Treatment == "Ambient" & Source == "fl")
points(dat$CTleaf2.slope ~ dat$Date, pch = 2, col = "blue")
dat<-subset(df, Treatment == "Warmed" & Source == "fl")
points(dat$CTleaf2.slope ~ dat$Date, pch = 2, col = "firebrick")
# c AG ####

df<-subset(source, Species == "ag")
plot(df$intercept ~ df$Date, pch = NA, xaxt="n",yaxt="n",xlab="",ylab="", ylim = c(-5.5,2))
axis(2, at = seq(-6,2,1), las = 2)
axis.Date(1, df$MoYr, at = seq(start, end, "month"), las = 2, format = "%m/%d")

legend("topleft", "e)", bty = "n", cex = 1.5)

dat<-subset(df, Treatment == "Ambient" & Source == "bz")
points(dat$intercept ~ dat$Date, pch = 6, col = "blue")
dat<-subset(df, Treatment == "Warmed" & Source == "bz")
points(dat$intercept ~ dat$Date, pch = 6, col = "firebrick")
dat<-subset(df, Treatment == "Ambient" & Source == "fl")
points(dat$intercept ~ dat$Date, pch = 2, col = "blue")
dat<-subset(df, Treatment == "Warmed" & Source == "fl")
points(dat$intercept ~ dat$Date, pch = 2, col = "firebrick")

mtext(side = 2, expression(italic(c)), cex = 1.2, padj = -3.5)

# c RM ####

df<-subset(source, Species == "rm")
plot(df$intercept ~ df$Date, pch = NA, xaxt="n",yaxt="n",xlab="",ylab="", ylim = c(-6,2))
axis(2, at = seq(-6,2,1), las = 2)
axis.Date(1, df$MoYr, at = seq(start, end, "month"), las = 2, format = "%m/%d")

legend("topleft", "f)", bty = "n", cex = 1.5)

dat<-subset(df, Treatment == "Ambient" & Source == "bz")
points(dat$intercept ~ dat$Date, pch = 6, col = "blue")
dat<-subset(df, Treatment == "Warmed" & Source == "bz")
points(dat$intercept ~ dat$Date, pch = 6, col = "firebrick")
dat<-subset(df, Treatment == "Ambient" & Source == "fl")
points(dat$intercept ~ dat$Date, pch = 2, col = "blue")
dat<-subset(df, Treatment == "Warmed" & Source == "fl")
points(dat$intercept ~ dat$Date, pch = 2, col = "firebrick")

# off ####

mtext(side = 1, "Date (mm/dd)", outer = T, cex = 1.5, padj = 3)

dev.off()