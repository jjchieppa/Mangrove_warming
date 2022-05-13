# start ####

library(lattice);library(doBy); library(lubridate)
library(minpack.lm); library(effects); library(plotrix)
library(car); library(sjPlot)

rm(list=ls())
dev.off()
cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

home<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis"
work<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp" # work
raw<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/raw"
hist<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/hist"
lins<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/lins"
deets<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/deets"
env<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Weather Data"
anly<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/Analysis"

# data in ####

### Bring in the ref respiration data, bind area and mass for LMA

setwd(deets)
source<-read.csv("MangroveRvT_LPRmod_Rarea and Rmass at 25C.csv")
source<-subset(source, KEEP == "Y")
source$Date<-as.Date(source$Date, format = "%m/%d/%Y")
setwd(work)
trts<-read.csv("Mangrove Treatment Assignments.csv")
source<-merge(source, trts, by = "UserIDs_in", all = F)
rm(trts)

source$Date<-as.Date(source$Date, format = "%Y-%m-%d") # Double checks date format!
source$MoYr<-floor_date(source$Date, "month")
source$MoYr<-source$MoYr+25
source$LMA_g.m2<-source$LMA_kg.m2*1000
source$Timepoint<-as.factor(source$MoYr)

# env data in ####
# Bring in temperature data

setwd(env)

env<-read.csv("Mangrove RVT weather data_all.csv")
env$Date<-as.Date(env$Date, format = "%m/%d/%Y")
df<-source
df$sDate<-as.factor(df$Date)
env$sDate<-as.factor(env$Date)
reps<-unique(df$sDate)
new_env<-subset(env, Block == "")
for (reps in unique(df$sDate)){
  dat<-subset(env, env$sDate == reps)
  mnd<-min(dat$Date, na.rm = T)
  
  dat1<-subset(env, env$Date == mnd-1)
  dat2<-subset(env, env$Date == mnd-2)
  dat3<-subset(env, env$Date == mnd-3)
  dat4<-subset(env, env$Date == mnd-4)
  dat5<-subset(env, env$Date == mnd-5)
  
  newdat<-rbind(dat1,dat2,dat3,dat4,dat5)
  newdat$Date<-max(newdat$Date, na.rm = T)+1
  
  new_env<-rbind(newdat, new_env)
  rm(dat, mnd, dat1, dat2, dat3, dat4, dat5)
}
rm(newdat)

warm<-subset(new_env, Treatment == "Warm")
ambi<-subset(new_env, Treatment == "Ambi")
rm(new_env, env)

warm<-summaryBy(Temp_C ~ Date, FUN = mean, na.rm = T, warm)
ambi<-summaryBy(Temp_C ~ Date, FUN = mean, na.rm = T, ambi)
warm$Treatment<-"Warmed"
ambi$Treatment<-"Ambient"

temp<-rbind(warm, ambi)
rm(warm, ambi)

source<-merge(temp, source, by = c("Date","Treatment"))
rm(temp, df)

start<-as.Date("2020-04-15")
end<-as.Date("2020-12-27")



# start plot ####

setwd("C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis")

# tiff(file = "V_Mangrove Physiology ~ Date (both species).tiff", height = 8, width = 12, res = 600, units = "in", compression = "zip+p")

par(mfrow = c(3,2), omi = c(0.75,0.5,0.1,0.01), mar = c(0.8,1.45,0.8,1.45))

# Rarea AG ####

df<-subset(source, R_calc == "R.area")
df<-summaryBy(rdref ~ MoYr + Species + Source + trt, FUN = c(mean, std.error), na.rm = T, df)

plotCI(df$MoYr, df$rdref.mean, df$rdref.std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(0.35,2.5), xlim = c(start, end))
axis(2, at = seq(0.5,6.5,0.5), las = 2)
axis.Date(1, sdf$MoYr, at = seq(start, end, "month"), las = 2, format = "%m/%Y", labels = F)

par(new=T)
dat<-subset(df, Species == "ag" & Source == "fl" & trt == "a")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=2, col = "dodgerblue4", ylim = c(0.35,2.5), 
       xlim = c(start,end), sfrac=0,cex=1.500,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "dodgerblue4", dat, lty = 1)

par(new=T)
dat<-subset(df, Species == "ag" & Source == "fl" & trt == "w")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=2, col = "firebrick", ylim = c(0.35,2.5), 
       xlim = c(start,end), sfrac=0,cex=1.500,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 1)

par(new=T)
dat<-subset(df, Species == "ag" & Source == "bz" & trt == "a")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=6, col = "dodgerblue4", ylim = c(0.35,2.5), 
       xlim = c(start,end), sfrac=0,cex=1.500,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "dodgerblue4", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "ag" & Source == "bz" & trt == "w")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=6, col = "firebrick", ylim = c(0.35,2.5), 
       xlim = c(start,end), sfrac=0,cex=1.500,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 2)

mtext(side = 2, expression(italic(R)[area]*''^25~(mu*mol~m^-2~s^-1)), cex = 0.9, padj = -1.7)

legend("topleft", "a)", bty = "n")
mtext(side = 3, expression(italic(Avicennia)), cex = 1.2)

legend("bottomleft", horiz = F, pch = c(2,6), c("Subtropical", "Tropical"), bty = "n",
       col = c("black","black"), cex = 1.5, lty = c(1,2))
legend("bottomright", horiz = F, pch = c(15,15), c("Ambient", "Warmed"), bty = "n",
       col = c("dodgerblue4","firebrick"), cex = 1.5)



# Rarea RM ####

df<-subset(source, R_calc == "R.area")
df<-summaryBy(rdref ~ MoYr + Species + Source + trt, FUN = c(mean, std.error), na.rm = T, df)

plotCI(df$MoYr, df$rdref.mean, df$rdref.std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(0.35,2.5), xlim = c(start, end))
axis(2, at = seq(0.5,6.5,0.5), las = 2, cex.axis = 1.2)
axis.Date(1, sdf$MoYr, at = seq(start, end, "month"), las = 2, format = "%m/%Y", labels = F)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "fl" & trt == "a")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=2, col = "dodgerblue4", ylim = c(0.35,2.5), 
       xlim = c(start,end), sfrac=0,cex=1.500,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "dodgerblue4", dat, lty = 1)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "fl" & trt == "w")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=2, col = "firebrick", ylim = c(0.35,2.5), 
       xlim = c(start,end), sfrac=0,cex=1.500,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 1)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "bz" & trt == "a")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=6, col = "dodgerblue4", ylim = c(0.35,2.5), 
       xlim = c(start,end), sfrac=0,cex=1.500,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "dodgerblue4", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "bz" & trt == "w")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=6, col = "firebrick", ylim = c(0.35,2.5), 
       xlim = c(start,end), sfrac=0,cex=1.500,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 2)

legend("topleft", "b)", bty = "n")
mtext(side = 3, expression(italic(Rhizophora)), cex = 1.2)

# Rmass AG ####

df<-subset(source, R_calc == "R.mass")
df<-summaryBy(rdref ~ MoYr + Species + Source + trt, FUN = c(mean, std.error), na.rm = T, df)

plotCI(df$MoYr, df$rdref.mean, df$rdref.std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(0,30), xlim = c(start, end))
axis(2, at = seq(0,30,5), las = 2, cex.axis = 1.2)
axis.Date(1, sdf$MoYr, at = seq(start, end, "month"), las = 2, format = "%m/%Y", labels = F)

par(new=T)
dat<-subset(df, Species == "ag" & Source == "fl" & trt == "a")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=2, col = "dodgerblue4", ylim = c(0,30), 
       xlim = c(start,end), sfrac=0,cex=1.500,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "dodgerblue4", dat, lty = 1)

par(new=T)
dat<-subset(df, Species == "ag" & Source == "fl" & trt == "w")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=2, col = "firebrick", ylim = c(0,30), 
       xlim = c(start,end), sfrac=0,cex=1.500,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 1)

par(new=T)
dat<-subset(df, Species == "ag" & Source == "bz" & trt == "a")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=6, col = "dodgerblue4", ylim = c(0,30), 
       xlim = c(start,end), sfrac=0,cex=1.500,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "dodgerblue4", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "ag" & Source == "bz" & trt == "w")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=6, col = "firebrick", ylim = c(0,30), 
       xlim = c(start,end), sfrac=0,cex=1.500,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 2)

mtext(side = 2, expression(italic(R)[mass]*''^25~(nmol~g^-1~s^-1)), cex = 0.9, padj = -1.7)

legend("topleft", "c)", bty = "n")


# Rmass RM ####

df<-subset(source, R_calc == "R.mass")
df<-summaryBy(rdref ~ MoYr + Species + Source + trt, FUN = c(mean, std.error), na.rm = T, df)

plotCI(df$MoYr, df$rdref.mean, df$rdref.std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(0,30), xlim = c(start, end))
axis(2, at = seq(0,30,5), las = 2, cex.axis = 1.2)
axis.Date(1, sdf$MoYr, at = seq(start, end, "month"), las = 2, format = "%m/%Y", labels = F)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "fl" & trt == "a")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=2, col = "dodgerblue4", ylim = c(0,30), 
       xlim = c(start,end), sfrac=0,cex=1.500,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "dodgerblue4", dat, lty = 1)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "fl" & trt == "w")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=2, col = "firebrick", ylim = c(0,30), 
       xlim = c(start,end), sfrac=0,cex=1.500,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 1)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "bz" & trt == "a")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=6, col = "dodgerblue4", ylim = c(0,30), 
       xlim = c(start,end), sfrac=0,cex=1.500,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "dodgerblue4", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "bz" & trt == "w")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=6, col = "firebrick", ylim = c(0,30), 
       xlim = c(start,end), sfrac=0,cex=1.500,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 2)

legend("topleft", "d)", bty = "n")


# Q10 AG ####

df<-subset(source, R_calc == "R.area")
df<-summaryBy(q10 ~ MoYr + Species + Source + trt, FUN = c(mean, std.error), na.rm = T, df)

plotCI(df$MoYr, df$q10.mean, df$q10.std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(1.5,5.2), xlim = c(start, end))
axis(2, at = seq(0.5,6.5,1), las = 2, cex.axis = 1.2)
axis.Date(1, sdf$MoYr, at = seq(start, end, "month"), las = 2, format = "%m/%y")

par(new=T)
dat<-subset(df, Species == "ag" & Source == "fl" & trt == "a")
plotCI(dat$MoYr, dat$q10.mean, dat$q10.std.error, pch=2, col = "dodgerblue4", ylim = c(1.5,5.2), 
       xlim = c(start,end), sfrac=0,cex=1.500,xaxt="n",yaxt="n",xlab="",ylab="")
points(q10.mean ~ dat$MoYr, type="l", col = "dodgerblue4", dat, lty = 1)

par(new=T)
dat<-subset(df, Species == "ag" & Source == "fl" & trt == "w")
plotCI(dat$MoYr, dat$q10.mean, dat$q10.std.error, pch=2, col = "firebrick", ylim = c(1.5,5.2), 
       xlim = c(start,end), sfrac=0,cex=1.500,xaxt="n",yaxt="n",xlab="",ylab="")
points(q10.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 1)

par(new=T)
dat<-subset(df, Species == "ag" & Source == "bz" & trt == "a")
plotCI(dat$MoYr, dat$q10.mean, dat$q10.std.error, pch=6, col = "dodgerblue4", ylim = c(1.5,5.2), 
       xlim = c(start,end), sfrac=0,cex=1.500,xaxt="n",yaxt="n",xlab="",ylab="")
points(q10.mean ~ dat$MoYr, type="l", col = "dodgerblue4", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "ag" & Source == "bz" & trt == "w")
plotCI(dat$MoYr, dat$q10.mean, dat$q10.std.error, pch=6, col = "firebrick", ylim = c(1.5,5.2), 
       xlim = c(start,end), sfrac=0,cex=1.500,xaxt="n",yaxt="n",xlab="",ylab="")
points(q10.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 2)

mtext(side = 2, expression(italic(Q)[10]*''^25), cex = 0.9, padj = -1.9)
text(as.Date("2020-08-26"), 4.9, "*", cex = 2)

legend("topleft", "e)", bty = "n")

# Q10 RM ####

df<-subset(source, R_calc == "R.area")
df<-summaryBy(q10 ~ MoYr + Species + Source + trt, FUN = c(mean, std.error), na.rm = T, df)

plotCI(df$MoYr, df$q10.mean, df$q10.std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(1.5,5.2), xlim = c(start, end))
axis(2, at = seq(0.5,6.5,1), las = 2, cex.axis = 1.2)
axis.Date(1, sdf$MoYr, at = seq(start, end, "month"), las = 2, format = "%m/%y")

par(new=T)
dat<-subset(df, Species == "rm" & Source == "fl" & trt == "a")
plotCI(dat$MoYr, dat$q10.mean, dat$q10.std.error, pch=2, col = "dodgerblue4", ylim = c(1.5,5.2), 
       xlim = c(start,end), sfrac=0,cex=1.500,xaxt="n",yaxt="n",xlab="",ylab="")
points(q10.mean ~ dat$MoYr, type="l", col = "dodgerblue4", dat, lty = 1)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "fl" & trt == "w")
plotCI(dat$MoYr, dat$q10.mean, dat$q10.std.error, pch=2, col = "firebrick", ylim = c(1.5,5.2), 
       xlim = c(start,end), sfrac=0,cex=1.500,xaxt="n",yaxt="n",xlab="",ylab="")
points(q10.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 1)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "bz" & trt == "a")
plotCI(dat$MoYr, dat$q10.mean, dat$q10.std.error, pch=6, col = "dodgerblue4", ylim = c(1.5,5.2), 
       xlim = c(start,end), sfrac=0,cex=1.500,xaxt="n",yaxt="n",xlab="",ylab="")
points(q10.mean ~ dat$MoYr, type="l", col = "dodgerblue4", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "bz" & trt == "w")
plotCI(dat$MoYr, dat$q10.mean, dat$q10.std.error, pch=6, col = "firebrick", ylim = c(1.5,5.2), 
       xlim = c(start,end), sfrac=0,cex=1.500,xaxt="n",yaxt="n",xlab="",ylab="")
points(q10.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 2)

legend("topleft", "f)", bty = "n")

# LMA AG ####

# df<-subset(source, R_calc == "R.area")
# df<-summaryBy(LMA_g.m2 ~ MoYr + Species + Source + trt, FUN = c(mean, std.error), na.rm = T, df)
# 
# plotCI(df$MoYr, df$LMA_g.m2.mean, df$LMA_g.m2.std.error*0, sfrac = 0,
#        xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(75,200), xlim = c(start, end))
# axis(2, at = seq(0,200,25), las = 2, cex.axis = 1.2)
# 
# par(new=T)
# dat<-subset(df, Species == "ag" & Source == "fl" & trt == "a")
# plotCI(dat$MoYr, dat$LMA_g.m2.mean, dat$LMA_g.m2.std.error, pch=2, col = "dodgerblue4", ylim = c(75,200), 
#        xlim = c(start,end), sfrac=0,cex=1.500,xaxt="n",yaxt="n",xlab="",ylab="")
# points(LMA_g.m2.mean ~ dat$MoYr, type="l", col = "dodgerblue4", dat, lty = 1)
# 
# par(new=T)
# dat<-subset(df, Species == "ag" & Source == "fl" & trt == "w")
# plotCI(dat$MoYr, dat$LMA_g.m2.mean, dat$LMA_g.m2.std.error, pch=2, col = "firebrick", ylim = c(75,200), 
#        xlim = c(start,end), sfrac=0,cex=1.500,xaxt="n",yaxt="n",xlab="",ylab="")
# points(LMA_g.m2.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 1)
# 
# par(new=T)
# dat<-subset(df, Species == "ag" & Source == "bz" & trt == "a")
# plotCI(dat$MoYr, dat$LMA_g.m2.mean, dat$LMA_g.m2.std.error, pch=6, col = "dodgerblue4", ylim = c(75,200), 
#        xlim = c(start,end), sfrac=0,cex=1.500,xaxt="n",yaxt="n",xlab="",ylab="")
# points(LMA_g.m2.mean ~ dat$MoYr, type="l", col = "dodgerblue4", dat, lty = 2)
# 
# par(new=T)
# dat<-subset(df, Species == "ag" & Source == "bz" & trt == "w")
# plotCI(dat$MoYr, dat$LMA_g.m2.mean, dat$LMA_g.m2.std.error, pch=6, col = "firebrick", ylim = c(75,200), 
#        xlim = c(start,end), sfrac=0,cex=1.500,xaxt="n",yaxt="n",xlab="",ylab="")
# points(LMA_g.m2.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 2)
# 
# axis.Date(1, sdf$MoYr, at = seq(start, end, "month"), las = 2, format = "%m/%y")
# 
# mtext(side = 2, expression(LMA~(g~m^-2)), cex = 0.9, padj = -1.7)
# 
# legend("topleft", "g)", bty = "n")



# LMA RM ####
# df<-subset(source, R_calc == "R.area")
# df<-summaryBy(LMA_g.m2 ~ MoYr + Species + Source + trt, FUN = c(mean, std.error), na.rm = T, df)
# 
# plotCI(df$MoYr, df$LMA_g.m2.mean, df$LMA_g.m2.std.error*0, sfrac = 0,
#        xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(75,200), xlim = c(start, end))
# axis(2, at = seq(0,200,25), las = 2, cex.axis = 1.2)
# 
# par(new=T)
# dat<-subset(df, Species == "ag" & Source == "fl" & trt == "a")
# plotCI(dat$MoYr, dat$LMA_g.m2.mean, dat$LMA_g.m2.std.error, pch=2, col = "dodgerblue4", ylim = c(75,200), 
#        xlim = c(start,end), sfrac=0,cex=1.500,xaxt="n",yaxt="n",xlab="",ylab="")
# points(LMA_g.m2.mean ~ dat$MoYr, type="l", col = "dodgerblue4", dat, lty = 1)
# 
# par(new=T)
# dat<-subset(df, Species == "ag" & Source == "fl" & trt == "w")
# plotCI(dat$MoYr, dat$LMA_g.m2.mean, dat$LMA_g.m2.std.error, pch=2, col = "firebrick", ylim = c(75,200), 
#        xlim = c(start,end), sfrac=0,cex=1.500,xaxt="n",yaxt="n",xlab="",ylab="")
# points(LMA_g.m2.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 1)
# 
# par(new=T)
# dat<-subset(df, Species == "ag" & Source == "bz" & trt == "a")
# plotCI(dat$MoYr, dat$LMA_g.m2.mean, dat$LMA_g.m2.std.error, pch=6, col = "dodgerblue4", ylim = c(75,200), 
#        xlim = c(start,end), sfrac=0,cex=1.500,xaxt="n",yaxt="n",xlab="",ylab="")
# points(LMA_g.m2.mean ~ dat$MoYr, type="l", col = "dodgerblue4", dat, lty = 2)
# 
# par(new=T)
# dat<-subset(df, Species == "ag" & Source == "bz" & trt == "w")
# plotCI(dat$MoYr, dat$LMA_g.m2.mean, dat$LMA_g.m2.std.error, pch=6, col = "firebrick", ylim = c(75,200), 
#        xlim = c(start,end), sfrac=0,cex=1.500,xaxt="n",yaxt="n",xlab="",ylab="")
# points(LMA_g.m2.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 2)
# 
# axis.Date(1, sdf$MoYr, at = seq(start, end, "month"), las = 2, format = "%m/%y")
# 
# 
# legend("topleft", "h)", bty = "n")

# off ####
mtext(side = 1, outer = T, padj = 3.3, "Date (mm-yy)", cex = 1.2)
# dev.off(); 1+1
