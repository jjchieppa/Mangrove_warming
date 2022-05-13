library(lattice);library(doBy); library(lubridate)
library(minpack.lm); library(effects); library(plotrix)

rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

setwd("C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis")

df<-read.csv("Mangrove morphology data_error2.csv")
df<-subset(df, Dead != "Y")
df$dia_mm<-(df$dia1_mm+df$dia2_mm)/2
df$ht_mm<-df$ht_cm*10
df$vol_mm3<-df$dia_mm*(df$ht_mm)
df$vol_cm3<-df$vol_mm3/100

df<-summaryBy(dia_mm + ht_mm + leaf.no + vol_cm3 ~
                Date + Species + Source + Treatment,
              FUN = c(mean, std.error), na.rm = T, df)
df$Date<-as.Date(df$Date, format = "%m/%d/%Y")
df<-df[order(df$Date),]

########################################################################################

tiff(file = "V_Mangrove Morphology Performance.tiff", height = 7, width = 10, res = 600, units = "in", compression = "zip+p")

par(mfrow = c(4,2), omi = c(0.4,0.6,0.2,0.01), mar = c(0.5,1.25,0.5,1))

########################################################################################
########################################################################################

xl<-100
yl<-800

plotCI(df$Date, df$ht_mm.mean*0, df$ht_mm.mean*0, ylim = c(xl,yl), sfrac = 0, xaxt="n",yaxt="n",xlab="",ylab="",pch=NA)
axis(2, at = seq(xl,yl,100), las = 2)
legend("topleft", "a)", bty = "n")

dat<-subset(df, Species == "AG" & Source == "FL" & Treatment == "Ambient")
par(new=T)
plotCI(dat$Date, dat$ht_mm.mean, dat$ht_mm.std.error, ylim = c(xl,yl), pch = 2, col = "blue", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$ht_mm.mean ~ dat$Date, type = "l", col = "blue", lty = 2)
dat<-subset(df, Species == "AG" & Source == "FL" & Treatment == "Warmed")
par(new=T)
plotCI(dat$Date, dat$ht_mm.mean, dat$ht_mm.std.error, ylim = c(xl,yl), pch = 2, col = "firebrick", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$ht_mm.mean ~ dat$Date, type = "l", col = "firebrick", lty = 2)

dat<-subset(df, Species == "AG" & Source == "BZ" & Treatment == "Ambient")
par(new=T)
plotCI(dat$Date, dat$ht_mm.mean, dat$ht_mm.std.error, ylim = c(xl,yl), pch = 6, col = "blue", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$ht_mm.mean ~ dat$Date, type = "l", col = "blue", lty = 1)
dat<-subset(df, Species == "AG" & Source == "BZ" & Treatment == "Warmed")
par(new=T)
plotCI(dat$Date, dat$ht_mm.mean, dat$ht_mm.std.error, ylim = c(xl,yl), pch = 6, col = "firebrick", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$ht_mm.mean ~ dat$Date, type = "l", col = "firebrick", lty = 1)

axis.Date(1, df$Date, at = seq(min(df$Date), max(df$Date), "month"), las = 2, format = "%m/%d", labels = F)

mtext(side = 3, expression(italic(A.~germinans)))

mtext(side = 2, "HT (mm)", padj = -3.8)

legend("bottom", horiz = T, pch = c(2,6,16,16), c("FL","BZ", "Ambient", "Warmed"), bty = "n",
       col = c("black","black","blue","firebrick"), lty = c(2,1,NA,NA), pt.cex = 1.2)


########################################################################################

xl<-100
yl<-800

plotCI(df$Date, df$ht_mm.mean*0, df$ht_mm.mean*0, ylim = c(xl,yl), sfrac = 0, xaxt="n",yaxt="n",xlab="",ylab="",pch=NA)
axis(2, at = seq(xl,yl,100), labels = F)
legend("topleft", "b)", bty = "n")

dat<-subset(df, Species == "RM" & Source == "FL" & Treatment == "Ambient")
par(new=T)
plotCI(dat$Date, dat$ht_mm.mean, dat$ht_mm.std.error, ylim = c(xl,yl), pch = 2, col = "blue", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$ht_mm.mean ~ dat$Date, type = "l", col = "blue", lty = 2)
dat<-subset(df, Species == "RM" & Source == "FL" & Treatment == "Warmed")
par(new=T)
plotCI(dat$Date, dat$ht_mm.mean, dat$ht_mm.std.error, ylim = c(xl,yl), pch = 2, col = "firebrick", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$ht_mm.mean ~ dat$Date, type = "l", col = "firebrick", lty = 2)

dat<-subset(df, Species == "RM" & Source == "BZ" & Treatment == "Ambient")
par(new=T)
plotCI(dat$Date, dat$ht_mm.mean, dat$ht_mm.std.error, ylim = c(xl,yl), pch = 6, col = "blue", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$ht_mm.mean ~ dat$Date, type = "l", col = "blue", lty = 1)
dat<-subset(df, Species == "RM" & Source == "BZ" & Treatment == "Warmed")
par(new=T)
plotCI(dat$Date, dat$ht_mm.mean, dat$ht_mm.std.error, ylim = c(xl,yl), pch = 6, col = "firebrick", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$ht_mm.mean ~ dat$Date, type = "l", col = "firebrick", lty = 1)

axis.Date(1, df$Date, at = seq(min(df$Date), max(df$Date), "month"), las = 2, format = "%m/%d", labels = F)

mtext(side = 3, expression(italic(R.~mangle)))



########################################################################################
########################################################################################

xl<-3
yl<-15

plotCI(df$Date, df$dia_mm.mean*0, df$dia_mm.std.error*0, ylim = c(xl,yl), sfrac = 0, xaxt="n",yaxt="n",xlab="",ylab="",pch=NA)
axis(2, at = seq(xl,yl,3), las = 2)
legend("topleft", "c)", bty = "n")

dat<-subset(df, Species == "AG" & Source == "FL" & Treatment == "Ambient")
par(new=T)
plotCI(dat$Date, dat$dia_mm.mean, dat$dia_mm.std.error, ylim = c(xl,yl), pch = 2, col = "blue", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$dia_mm.mean ~ dat$Date, type = "l", col = "blue", lty = 2)
dat<-subset(df, Species == "AG" & Source == "FL" & Treatment == "Warmed")
par(new=T)
plotCI(dat$Date, dat$dia_mm.mean, dat$dia_mm.std.error, ylim = c(xl,yl), pch = 2, col = "firebrick", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$dia_mm.mean ~ dat$Date, type = "l", col = "firebrick", lty = 2)

dat<-subset(df, Species == "AG" & Source == "BZ" & Treatment == "Ambient")
par(new=T)
plotCI(dat$Date, dat$dia_mm.mean, dat$dia_mm.std.error, ylim = c(xl,yl), pch = 6, col = "blue", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$dia_mm.mean ~ dat$Date, type = "l", col = "blue", lty = 1)
dat<-subset(df, Species == "AG" & Source == "BZ" & Treatment == "Warmed")
par(new=T)
plotCI(dat$Date, dat$dia_mm.mean, dat$dia_mm.std.error, ylim = c(xl,yl), pch = 6, col = "firebrick", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$dia_mm.mean ~ dat$Date, type = "l", col = "firebrick", lty = 1)

axis.Date(1, df$Date, at = seq(min(df$Date), max(df$Date), "month"), las = 2, format = "%m/%d", labels = F)

mtext(side = 2, "DIA (mm)", padj = -3.8)

########################################################################################

xl<-3
yl<-15

plotCI(df$Date, df$dia_mm.mean*0, df$dia_mm.mean*0, ylim = c(xl,yl), sfrac = 0, xaxt="n",yaxt="n",xlab="",ylab="",pch=NA)
axis(2, at = seq(xl,yl,3), labels = F)
legend("topleft", "d)", bty = "n")

dat<-subset(df, Species == "RM" & Source == "FL" & Treatment == "Ambient")
par(new=T)
plotCI(dat$Date, dat$dia_mm.mean, dat$dia_mm.std.error, ylim = c(xl,yl), pch = 2, col = "blue", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$dia_mm.mean ~ dat$Date, type = "l", col = "blue", lty = 2)
dat<-subset(df, Species == "RM" & Source == "FL" & Treatment == "Warmed")
par(new=T)
plotCI(dat$Date, dat$dia_mm.mean, dat$dia_mm.std.error, ylim = c(xl,yl), pch = 2, col = "firebrick", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$dia_mm.mean ~ dat$Date, type = "l", col = "firebrick", lty = 2)

dat<-subset(df, Species == "RM" & Source == "BZ" & Treatment == "Ambient")
par(new=T)
plotCI(dat$Date, dat$dia_mm.mean, dat$dia_mm.std.error, ylim = c(xl,yl), pch = 6, col = "blue", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$dia_mm.mean ~ dat$Date, type = "l", col = "blue", lty = 1)
dat<-subset(df, Species == "RM" & Source == "BZ" & Treatment == "Warmed")
par(new=T)
plotCI(dat$Date, dat$dia_mm.mean, dat$dia_mm.std.error, ylim = c(xl,yl), pch = 6, col = "firebrick", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$dia_mm.mean ~ dat$Date, type = "l", col = "firebrick", lty = 1)

axis.Date(1, df$Date, at = seq(min(df$Date), max(df$Date), "month"), las = 2, format = "%m/%d", labels = F)









########################################################################################
########################################################################################

xl<-0
yl<-110

plotCI(df$Date, df$vol_cm3.mean*0, df$vol_cm3.std.error*0, ylim = c(xl,yl), sfrac = 0, xaxt="n",yaxt="n",xlab="",ylab="",pch=NA)
axis(2, at = seq(xl,yl,20), las = 2)
legend("topleft", "e)", bty = "n")

dat<-subset(df, Species == "AG" & Source == "FL" & Treatment == "Ambient")
par(new=T)
plotCI(dat$Date, dat$vol_cm3.mean, dat$vol_cm3.std.error, ylim = c(xl,yl), pch = 2, col = "blue", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$vol_cm3.mean ~ dat$Date, type = "l", col = "blue", lty = 2)
dat<-subset(df, Species == "AG" & Source == "FL" & Treatment == "Warmed")
par(new=T)
plotCI(dat$Date, dat$vol_cm3.mean, dat$vol_cm3.std.error, ylim = c(xl,yl), pch = 2, col = "firebrick", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$vol_cm3.mean ~ dat$Date, type = "l", col = "firebrick", lty = 2)

dat<-subset(df, Species == "AG" & Source == "BZ" & Treatment == "Ambient")
par(new=T)
plotCI(dat$Date, dat$vol_cm3.mean, dat$vol_cm3.std.error, ylim = c(xl,yl), pch = 6, col = "blue", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$vol_cm3.mean ~ dat$Date, type = "l", col = "blue", lty = 1)
dat<-subset(df, Species == "AG" & Source == "BZ" & Treatment == "Warmed")
par(new=T)
plotCI(dat$Date, dat$vol_cm3.mean, dat$vol_cm3.std.error, ylim = c(xl,yl), pch = 6, col = "firebrick", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$vol_cm3.mean ~ dat$Date, type = "l", col = "firebrick", lty = 1)

axis.Date(1, df$Date, at = seq(min(df$Date), max(df$Date), "month"), las = 2, format = "%m/%d", labels = F)

mtext(side = 2, expression(Stem~Volume~(cm^3)), padj = -2.1)

########################################################################################

xl<-0
yl<-110

plotCI(df$Date, df$vol_cm3.mean*0, df$vol_cm3.mean*0, ylim = c(xl,yl), sfrac = 0, xaxt="n",yaxt="n",xlab="",ylab="",pch=NA)
axis(2, at = seq(xl,yl,20), labels = F)
legend("topleft", "f)", bty = "n")

dat<-subset(df, Species == "RM" & Source == "FL" & Treatment == "Ambient")
par(new=T)
plotCI(dat$Date, dat$vol_cm3.mean, dat$vol_cm3.std.error, ylim = c(xl,yl), pch = 2, col = "blue", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$vol_cm3.mean ~ dat$Date, type = "l", col = "blue", lty = 2)
dat<-subset(df, Species == "RM" & Source == "FL" & Treatment == "Warmed")
par(new=T)
plotCI(dat$Date, dat$vol_cm3.mean, dat$vol_cm3.std.error, ylim = c(xl,yl), pch = 2, col = "firebrick", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$vol_cm3.mean ~ dat$Date, type = "l", col = "firebrick", lty = 2)

dat<-subset(df, Species == "RM" & Source == "BZ" & Treatment == "Ambient")
par(new=T)
plotCI(dat$Date, dat$vol_cm3.mean, dat$vol_cm3.std.error, ylim = c(xl,yl), pch = 6, col = "blue", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$vol_cm3.mean ~ dat$Date, type = "l", col = "blue", lty = 1)
dat<-subset(df, Species == "RM" & Source == "BZ" & Treatment == "Warmed")
par(new=T)
plotCI(dat$Date, dat$vol_cm3.mean, dat$vol_cm3.std.error, ylim = c(xl,yl), pch = 6, col = "firebrick", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$vol_cm3.mean ~ dat$Date, type = "l", col = "firebrick", lty = 1)

axis.Date(1, df$Date, at = seq(min(df$Date), max(df$Date), "month"), las = 2, format = "%m/%d", labels = F)








########################################################################################
########################################################################################

xl<-10
yl<-120

plotCI(df$Date, df$leaf.no.mean*0, df$leaf.no.std.error*0, ylim = c(xl,yl), sfrac = 0, xaxt="n",yaxt="n",xlab="",ylab="",pch=NA)
axis(2, at = seq(xl,yl,20), las = 2)
legend("topleft", "g)", bty = "n")

dat<-subset(df, Species == "AG" & Source == "FL" & Treatment == "Ambient")
par(new=T)
plotCI(dat$Date, dat$leaf.no.mean, dat$leaf.no.std.error, ylim = c(xl,yl), pch = 2, col = "blue", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$leaf.no.mean ~ dat$Date, type = "l", col = "blue", lty = 2)
dat<-subset(df, Species == "AG" & Source == "FL" & Treatment == "Warmed")
par(new=T)
plotCI(dat$Date, dat$leaf.no.mean, dat$leaf.no.std.error, ylim = c(xl,yl), pch = 2, col = "firebrick", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$leaf.no.mean ~ dat$Date, type = "l", col = "firebrick", lty = 2)

dat<-subset(df, Species == "AG" & Source == "BZ" & Treatment == "Ambient")
par(new=T)
plotCI(dat$Date, dat$leaf.no.mean, dat$leaf.no.std.error, ylim = c(xl,yl), pch = 6, col = "blue", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$leaf.no.mean ~ dat$Date, type = "l", col = "blue", lty = 1)
dat<-subset(df, Species == "AG" & Source == "BZ" & Treatment == "Warmed")
par(new=T)
plotCI(dat$Date, dat$leaf.no.mean, dat$leaf.no.std.error, ylim = c(xl,yl), pch = 6, col = "firebrick", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$leaf.no.mean ~ dat$Date, type = "l", col = "firebrick", lty = 1)

axis.Date(1, df$Date, at = seq(min(df$Date), max(df$Date), "month"), las = 2, format = "%m/%d")

mtext(side = 2, "Leaf Count", padj = -3.8)

########################################################################################

xl<-3
yl<-20

plotCI(df$Date, df$leaf.no.mean*0, df$leaf.no.mean*0, ylim = c(xl,yl), sfrac = 0, xaxt="n",yaxt="n",xlab="",ylab="",pch=NA)
axis(2, at = seq(xl,yl,3), las = 2)
legend("topleft", "h)", bty = "n")

dat<-subset(df, Species == "RM" & Source == "FL" & Treatment == "Ambient")
par(new=T)
plotCI(dat$Date, dat$leaf.no.mean, dat$leaf.no.std.error, ylim = c(xl,yl), pch = 2, col = "blue", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$leaf.no.mean ~ dat$Date, type = "l", col = "blue", lty = 2)
dat<-subset(df, Species == "RM" & Source == "FL" & Treatment == "Warmed")
par(new=T)
plotCI(dat$Date, dat$leaf.no.mean, dat$leaf.no.std.error, ylim = c(xl,yl), pch = 2, col = "firebrick", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$leaf.no.mean ~ dat$Date, type = "l", col = "firebrick", lty = 2)

dat<-subset(df, Species == "RM" & Source == "BZ" & Treatment == "Ambient")
par(new=T)
plotCI(dat$Date, dat$leaf.no.mean, dat$leaf.no.std.error, ylim = c(xl,yl), pch = 6, col = "blue", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$leaf.no.mean ~ dat$Date, type = "l", col = "blue", lty = 1)
dat<-subset(df, Species == "RM" & Source == "BZ" & Treatment == "Warmed")
par(new=T)
plotCI(dat$Date, dat$leaf.no.mean, dat$leaf.no.std.error, ylim = c(xl,yl), pch = 6, col = "firebrick", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$leaf.no.mean ~ dat$Date, type = "l", col = "firebrick", lty = 1)

axis.Date(1, df$Date, at = seq(min(df$Date), max(df$Date), "month"), las = 2, format = "%m/%d")


dev.off()
