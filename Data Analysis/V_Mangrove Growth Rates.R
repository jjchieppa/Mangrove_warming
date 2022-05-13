# in data ####

library(lattice);library(doBy); library(lubridate)
library(minpack.lm); library(effects); library(plotrix)

rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

setwd("C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis")

df<-read.csv("Mangrove morphology data_error2.csv")
df<-subset(df, Dead != "Y")

df<-summaryBy(RGR_cm3.day + leaf.no + vol_cm3 ~
                Date + Species + Source + Treatment,
              FUN = c(mean, std.error), na.rm = T, df)
df$Date<-as.Date(df$Date, format = "%m/%d/%Y")
df<-df[order(df$Date),]

setwd("C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/deets")
lma<-read.csv("MangroveRvT_LPRmod_Rarea and Rmass at 25C.csv")
lma<-subset(lma, KEEP == "Y")
lma$Date<-as.Date(lma$Date, format = "%m/%d/%Y")

setwd("C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp")
trts<-read.csv("Mangrove Treatment Assignments.csv")
lma<-merge(lma, trts, by = "UserIDs_in", all = F)
rm(trts)
lma$Date<-as.Date(lma$Date, format = "%Y-%m-%d") # Double checks date format!
lma$MoYr<-floor_date(lma$Date, "month")
lma$MoYr<-lma$MoYr+25
lma$LMA_g.m2<-lma$LMA_kg.m2*1000
lma$Timepoint<-as.factor(lma$MoYr)
lma<-summaryBy(LMA_g.m2 ~ MoYr + Species + Source + trt, FUN = c(mean, std.error), na.rm = T, lma)
lma$MoYr<-ifelse(lma$MoYr == "2020-04-26", "2020-04-28",
                 ifelse(lma$MoYr == "2020-05-26", "2020-05-18",
                        ifelse(lma$MoYr == "2020-06-26", "2020-06-11",
                               ifelse(lma$MoYr == "2020-07-26", "2020-07-11",
                                      ifelse(lma$MoYr == "2020-08-26", "2020-08-20",
                                             ifelse(lma$MoYr == "2020-10-26", "2020-10-13",
                                                    ifelse(lma$MoYr == "2020-12-26", "2020-12-08", "NA")))))))
lma$MoYr<-as.Date(lma$MoYr, format = "%Y-%m-%d")


# Start image ####

setwd("C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis")

tiff(file = "V_Mangrove Growth Rates.tiff", height = 7, width = 10, res = 600, units = "in", compression = "zip+p")

par(mfrow = c(4,2), omi = c(0.7,0.6,0.2,0.01), mar = c(1,1.5,1,2))

# a) AG vol ####

xl<-0
yl<-110

plotCI(df$Date, df$vol_cm3.mean*0, df$vol_cm3.std.error*0, ylim = c(xl,yl), sfrac = 0, xaxt="n",yaxt="n",xlab="",ylab="",pch=NA)
axis(2, at = seq(xl,yl,20), las = 2)
legend("topleft", "a)", bty = "n")

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
mtext(side = 3, expression(italic(Avicennia)))
mtext(side = 2, expression(Stem~Volume~(cm^3)), padj = -2.5, cex = .7)

legend("top", horiz = T, pch = c(2,6,15,15), c("Subtropical","Tropical", "Ambient", "Warmed"), bty = "n",
       col = c("black","black","blue","firebrick"), lty = c(2,1,NA,NA), pt.cex = 1.5)

text(as.Date("2020-10-13"), 65, "*", cex = 2.5)
text(as.Date("2020-12-08"), 105, "*", cex = 2.5)
text(as.Date("2020-06-20"), 60, expression('Date x Population, '*italic(F)~'= 6.01'), cex = 1.3)
text(as.Date("2020-06-20"), 50, expression(italic(P)~"< 0.001"), cex = 1.3)


# b) RM vol ####

xl<-0
yl<-50

plotCI(df$Date, df$vol_cm3.mean*0, df$vol_cm3.std.error*0, ylim = c(xl,yl), sfrac = 0, xaxt="n",yaxt="n",xlab="",ylab="",pch=NA)
axis(2, at = seq(xl,yl,10), las = 2)
legend("topleft", "b)", bty = "n")

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
mtext(side = 3, expression(italic(Rhizophora)))

text(as.Date("2020-10-13"), 30, "*", cex = 2.5)
text(as.Date("2020-12-07"), 45, "*", cex = 2.5)
text(as.Date("2020-06-20"), 30, expression('Date x Population, '*italic(F)~'= 2.80'), cex = 1.3)
text(as.Date("2020-06-20"), 25, expression(italic(P)~"< 0.05"), cex = 1.3)

# c) AG RGR ####

xl<-0
yl<-0.6

plotCI(df$Date, df$RGR_cm3.day.mean*0, df$RGR_cm3.day.std.error*0, ylim = c(xl,yl), sfrac = 0, xaxt="n",yaxt="n",xlab="",ylab="",pch=NA)
axis(2, at = seq(xl,yl,0.1), las = 2)
legend("topleft", "c)", bty = "n")

dat<-subset(df, Species == "AG" & Source == "FL" & Treatment == "Ambient")
par(new=T)
plotCI(dat$Date, dat$RGR_cm3.day.mean, dat$RGR_cm3.day.std.error, ylim = c(xl,yl), pch = 2, col = "blue", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$RGR_cm3.day.mean ~ dat$Date, type = "l", col = "blue", lty = 2)
dat<-subset(df, Species == "AG" & Source == "FL" & Treatment == "Warmed")
par(new=T)
plotCI(dat$Date, dat$RGR_cm3.day.mean, dat$RGR_cm3.day.std.error, ylim = c(xl,yl), pch = 2, col = "firebrick", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$RGR_cm3.day.mean ~ dat$Date, type = "l", col = "firebrick", lty = 2)

dat<-subset(df, Species == "AG" & Source == "BZ" & Treatment == "Ambient")
par(new=T)
plotCI(dat$Date, dat$RGR_cm3.day.mean, dat$RGR_cm3.day.std.error, ylim = c(xl,yl), pch = 6, col = "blue", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$RGR_cm3.day.mean ~ dat$Date, type = "l", col = "blue", lty = 1)
dat<-subset(df, Species == "AG" & Source == "BZ" & Treatment == "Warmed")
par(new=T)
plotCI(dat$Date, dat$RGR_cm3.day.mean, dat$RGR_cm3.day.std.error, ylim = c(xl,yl), pch = 6, col = "firebrick", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$RGR_cm3.day.mean ~ dat$Date, type = "l", col = "firebrick", lty = 1)

axis.Date(1, df$Date, at = seq(min(df$Date), max(df$Date), "month"), las = 2, format = "%m/%d", labels = F)
mtext(side = 2, expression(Growth~Rate~(cm^3~day^-1)), padj = -2.5, cex = .7)



# d) RM RGR ####

xl<-0
yl<-0.3

plotCI(df$Date, df$RGR_cm3.day.mean*0, df$RGR_cm3.day.std.error*0, ylim = c(xl,yl), sfrac = 0, xaxt="n",yaxt="n",xlab="",ylab="",pch=NA)
axis(2, at = seq(xl,yl,0.05), las = 2)
legend("topleft", "d)", bty = "n")

dat<-subset(df, Species == "RM" & Source == "FL" & Treatment == "Ambient")
par(new=T)
plotCI(dat$Date, dat$RGR_cm3.day.mean, dat$RGR_cm3.day.std.error, ylim = c(xl,yl), pch = 2, col = "blue", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$RGR_cm3.day.mean ~ dat$Date, type = "l", col = "blue", lty = 2)
dat<-subset(df, Species == "RM" & Source == "FL" & Treatment == "Warmed")
par(new=T)
plotCI(dat$Date, dat$RGR_cm3.day.mean, dat$RGR_cm3.day.std.error, ylim = c(xl,yl), pch = 2, col = "firebrick", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$RGR_cm3.day.mean ~ dat$Date, type = "l", col = "firebrick", lty = 2)

dat<-subset(df, Species == "RM" & Source == "BZ" & Treatment == "Ambient")
par(new=T)
plotCI(dat$Date, dat$RGR_cm3.day.mean, dat$RGR_cm3.day.std.error, ylim = c(xl,yl), pch = 6, col = "blue", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$RGR_cm3.day.mean ~ dat$Date, type = "l", col = "blue", lty = 1)
dat<-subset(df, Species == "RM" & Source == "BZ" & Treatment == "Warmed")
par(new=T)
plotCI(dat$Date, dat$RGR_cm3.day.mean, dat$RGR_cm3.day.std.error, ylim = c(xl,yl), pch = 6, col = "firebrick", sfrac = 0,xaxt="n",yaxt="n",xlab="",ylab="")
points(dat$RGR_cm3.day.mean ~ dat$Date, type = "l", col = "firebrick", lty = 1)

axis.Date(1, df$Date, at = seq(min(df$Date), max(df$Date), "month"), las = 2, format = "%m/%d", labels = F)



# e) AG leafno ####

xl<-0
yl<-130

plotCI(df$Date, df$leaf.no.mean*0, df$leaf.no.std.error*0, ylim = c(xl,yl), sfrac = 0, xaxt="n",yaxt="n",xlab="",ylab="",pch=NA)
axis(2, at = seq(xl,yl,20), las = 2)
legend("topleft", "e)", bty = "n", adj = 1.2)

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

axis.Date(1, df$Date, at = seq(min(df$Date), max(df$Date), "month"), labels = F)
mtext(side = 2, expression(Total~Leaf~Count), padj = -4.3, cex = .7)

text(as.Date("2020-08-20"), 80, "*", cex = 2.5)
text(as.Date("2020-10-13"), 110, "*", cex = 2.5)
text(as.Date("2020-12-08"), 120, "*", cex = 2.5)
text(as.Date("2020-06-20"), 80, expression('Date x Population, '*italic(F)~'= 10.68'), cex = 1.3)
text(as.Date("2020-06-20"), 65, expression(italic(P)~"< 0.001"), cex = 1.3)

# f) RM leafno ####

xl<-0
yl<-20

plotCI(df$Date, df$leaf.no.mean*0, df$leaf.no.std.error*0, ylim = c(xl,yl), sfrac = 0, xaxt="n",yaxt="n",xlab="",ylab="",pch=NA)
axis(2, at = seq(xl,yl,4), las = 2)
legend("topleft", "f)", bty = "n")

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

axis.Date(1, df$Date, at = seq(min(df$Date), max(df$Date), "month"), labels = F)


# g) AG LMA ####

plotCI(lma$MoYr, lma$LMA_g.m2.mean, lma$LMA_g.m2.std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(75,200))
axis(2, at = seq(0,200,25), las = 2, cex.axis = 1.2)
axis.Date(1, df$Date, at = seq(min(df$Date), max(df$Date), "month"), las = 2, format = "%m/%d")


par(new=T)
dat<-subset(lma, Species == "ag" & Source == "fl" & trt == "a")
plotCI(dat$MoYr, dat$LMA_g.m2.mean, dat$LMA_g.m2.std.error, pch=2, col = "blue", ylim = c(75,200),
       sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(LMA_g.m2.mean ~ dat$MoYr, type="l", col = "blue", dat, lty = 1)

par(new=T)
dat<-subset(lma, Species == "ag" & Source == "fl" & trt == "w")
plotCI(dat$MoYr, dat$LMA_g.m2.mean, dat$LMA_g.m2.std.error, pch=2, col = "firebrick", ylim = c(75,200),
       sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(LMA_g.m2.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 1)

par(new=T)
dat<-subset(lma, Species == "ag" & Source == "bz" & trt == "a")
plotCI(dat$MoYr, dat$LMA_g.m2.mean, dat$LMA_g.m2.std.error, pch=6, col = "blue", ylim = c(75,200),
       sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(LMA_g.m2.mean ~ dat$MoYr, type="l", col = "blue", dat, lty = 2)

par(new=T)
dat<-subset(lma, Species == "ag" & Source == "bz" & trt == "w")
plotCI(dat$MoYr, dat$LMA_g.m2.mean, dat$LMA_g.m2.std.error, pch=6, col = "firebrick", ylim = c(75,200),
       sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(LMA_g.m2.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 2)

mtext(side = 2, expression(LMA~(g~m^-2)), padj = -2.8, cex = .7)

legend("topleft", "g)", bty = "n")
# h) AG LMA ####

plotCI(lma$MoYr, lma$LMA_g.m2.mean, lma$LMA_g.m2.std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(75,200))
axis(2, at = seq(0,200,25), las = 2, cex.axis = 1.2)
axis.Date(1, df$Date, at = seq(min(df$Date), max(df$Date), "month"), las = 2, format = "%m/%d")


par(new=T)
dat<-subset(lma, Species == "rm" & Source == "fl" & trt == "a")
plotCI(dat$MoYr, dat$LMA_g.m2.mean, dat$LMA_g.m2.std.error, pch=2, col = "blue", ylim = c(75,200),
       sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(LMA_g.m2.mean ~ dat$MoYr, type="l", col = "blue", dat, lty = 1)

par(new=T)
dat<-subset(lma, Species == "rm" & Source == "fl" & trt == "w")
plotCI(dat$MoYr, dat$LMA_g.m2.mean, dat$LMA_g.m2.std.error, pch=2, col = "firebrick", ylim = c(75,200),
       sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(LMA_g.m2.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 1)

par(new=T)
dat<-subset(lma, Species == "rm" & Source == "bz" & trt == "a")
plotCI(dat$MoYr, dat$LMA_g.m2.mean, dat$LMA_g.m2.std.error, pch=6, col = "blue", ylim = c(75,200),
       sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(LMA_g.m2.mean ~ dat$MoYr, type="l", col = "blue", dat, lty = 2)

par(new=T)
dat<-subset(lma, Species == "rm" & Source == "bz" & trt == "w")
plotCI(dat$MoYr, dat$LMA_g.m2.mean, dat$LMA_g.m2.std.error, pch=6, col = "firebrick", ylim = c(75,200),
       sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(LMA_g.m2.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 2)

legend("topleft", "h)", bty = "n")
# dev.off ####
mtext(side = 1, "Date (month/day)", cex = 1.5, padj = 2.7, outer = T)
dev.off()