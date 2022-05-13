library(lattice);library(doBy); library(lubridate)
library(minpack.lm); library(effects); library(plotrix)
library(car); library(sjPlot)

rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

#########################################################
#########################################################

home<-"C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis"
work<-"C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp" # work
raw<-"C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/raw"
hist<-"C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/hist"
lins<-"C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/lins"
deets<-"C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/deets"
env<-"C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Weather Data"
anly<-"C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/Analysis"

#########################################################
#########################################################

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

#########################################################
#########################################################

# Bring in temperature data

setwd(env)

env<-read.csv("Mangrove RVT weather data_all.csv")
env$Date<-as.Date(env$Date, format = "%Y-%m-%d")

#########################################################
#########################################################


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

#########################################################
#########################################################

setwd("C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis")

tiff(file = "V_Mangrove Physiology Performance.tiff", height = 7, width = 11, res = 600, units = "in", compression = "zip+p")

par(mfrow = c(4,4), omi = c(0.75,0.5,0.1,0.01), mar = c(0.8,1.45,0.8,1.45))

#########################################################

df<-subset(source, R_calc == "R.area")
df<-summaryBy(rdref ~ MoYr + Species + Source + trt, FUN = c(mean, std.error), na.rm = T, df)

plotCI(df$MoYr, df$rdref.mean, df$rdref.std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(0.35,3.5), xlim = c(start, end))
axis(2, at = seq(0.5,6.5,1), las = 2)
axis.Date(1, sdf$MoYr, at = seq(start, end, "month"), las = 2, format = "%m/%Y", labels = F)

par(new=T)
dat<-subset(df, Species == "ag" & Source == "fl" & trt == "a")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=2, col = "blue", ylim = c(0.35,3.5), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "blue", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "ag" & Source == "fl" & trt == "w")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=2, col = "firebrick", ylim = c(0.35,3.5), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "ag" & Source == "bz" & trt == "a")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=6, col = "blue", ylim = c(0.35,3.5), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "blue", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "ag" & Source == "bz" & trt == "w")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=6, col = "firebrick", ylim = c(0.35,3.5), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 2)

mtext(side=3, expression(italic(A.~germinans)))
mtext(side = 2, expression(italic(R)[area]*''^25~(mu*mol~m^-2~s^-1)), cex = 0.9, padj = -1.7)

legend("topleft", "a)", bty = "n")

legend("bottomleft", horiz = F, pch = c(2,6), c("FL","BZ"), bty = "n",
       col = c("black","black"), cex = 0.9)
legend("bottom", horiz = F, pch = c(16,16), c("Ambient", "Warmed"), bty = "n",
       col = c("blue","firebrick"), cex = 0.9)

dum<-subset(source, Species == "ag" & R_calc == "R.area")
leveneTest(rdref ~ Treatment * Source * Timepoint, dum)
m1<-lm(rdref ~ Treatment * Source * Timepoint, dum)
Anova(m1); shapiro.test(m1$residuals)
legend("top", bty="n", c("Treatment*", "Origin","Date***"), cex = 0.9)
legend("topright", bty="n", c("Tr x O","Tr x D","O x D","Tr x O x D"), cex = 0.9)

######################

df<-subset(source, R_calc == "R.area")
df<-summaryBy(rdref ~ MoYr + Species + Source + trt, FUN = c(mean, std.error), na.rm = T, df)

plotCI(df$MoYr, df$rdref.mean, df$rdref.std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(0.35,3.5), xlim = c(start, end))
axis(2, at = seq(0.5,6.5,1), las = 2)
axis.Date(1, sdf$MoYr, at = seq(start, end, "month"), las = 2, format = "%m/%Y", labels = F)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "fl" & trt == "a")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=2, col = "blue", ylim = c(0.35,3.5), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "blue", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "fl" & trt == "w")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=2, col = "firebrick", ylim = c(0.35,3.5), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "bz" & trt == "a")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=6, col = "blue", ylim = c(0.35,3.5), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "blue", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "bz" & trt == "w")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=6, col = "firebrick", ylim = c(0.35,3.5), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 2)

mtext(side=3, expression(italic(R.~mangle)))

legend("topleft", "b)", bty = "n")

dum<-subset(source, Species == "rm" & R_calc == "R.area")
leveneTest(rdref ~ Treatment * Source * Timepoint, dum)
m1<-lm(rdref ~ Treatment * Source * Timepoint, dum)
Anova(m1); shapiro.test(m1$residuals)
legend("top", bty="n", c("Treatment", "Origin","Date***"), cex = 0.9)
legend("topright", bty="n", c("Tr x O","Tr x D","O x D*","Tr x O x D"), cex = 0.9)

######################

df<-subset(source, R_calc == "R.area")

dat<-subset(df, Treatment == "Ambient" & Species == "ag" & Source == "fl")
plot(dat$rdref ~ dat$Temp_C.mean, xlim = c(10,30), ylim = c(0.35,3.5), pch = 2, col = "blue",
     xaxt="n",yaxt="n",xlab="",ylab="")
dat<-subset(df, Treatment == "Warmed" & Species == "ag" & Source == "fl")
points(dat$rdref ~ dat$Temp_C.mean, pch = 2, col = "firebrick")
dat<-subset(df, Treatment == "Warmed" & Species == "ag" & Source == "bz")
points(dat$rdref ~ dat$Temp_C.mean, pch = 6, col = "firebrick")
dat<-subset(df, Treatment == "Ambient" & Species == "ag" & Source == "bz")
points(dat$rdref ~ dat$Temp_C.mean, pch = 6, col = "blue")
axis(1, at = seq(0,100,5), labels = F)
axis(2, at = seq(0.5,6.5,1), las = 2)

mtext(side=3, expression(italic(A.~germinans)))

legend("topleft", "c)", bty = "n")

dum<-subset(source, Species == "ag" & R_calc == "R.area")
m1<-lm(rdref ~ Treatment * Source * Temp_C.mean, dum)
Anova(m1); shapiro.test(m1$residuals)
x<-plot_model(m1, type = c("pred"), terms = c("Temp_C.mean"))
newdata<-as.data.frame(x$data)
points(predicted ~ x, newdata, lty = 1, type = "l")
legend("top", bty="n", c("Treatment", "Origin","Temperature***"), cex = 0.9)
legend("topright", bty="n", c("Tr x O","Tr x Te","O x Te","Tr x O x Te"), cex = 0.9)


######################

dat<-subset(df, Treatment == "Ambient" & Species == "rm" & Source == "fl")
plot(dat$rdref ~ dat$Temp_C.mean, xlim = c(10,30), ylim = c(0.35,3.5), pch = 2, col = "blue",
     xaxt="n",yaxt="n",xlab="",ylab="")
dat<-subset(df, Treatment == "Warmed" & Species == "rm" & Source == "fl")
points(dat$rdref ~ dat$Temp_C.mean, pch = 2, col = "firebrick")
dat<-subset(df, Treatment == "Warmed" & Species == "rm" & Source == "bz")
points(dat$rdref ~ dat$Temp_C.mean, pch = 6, col = "firebrick")
dat<-subset(df, Treatment == "Ambient" & Species == "rm" & Source == "bz")
points(dat$rdref ~ dat$Temp_C.mean, pch = 6, col = "blue")
axis(1, at = seq(0,100,5), labels = F)
axis(2, at = seq(0.5,6.5,1), las = 2)

mtext(side=3, expression(italic(R.~mangle)))

legend("topleft", "d)", bty = "n")

dum<-subset(source, Species == "rm" & R_calc == "R.area")
m1<-lm(rdref ~ Treatment * Source * Temp_C.mean, dum)
Anova(m1); shapiro.test(m1$residuals)
x<-plot_model(m1, type = c("pred"), terms = c("Temp_C.mean"))
newdata<-as.data.frame(x$data)
points(predicted ~ x, newdata, lty = 1, type = "l")
legend("top", bty="n", c("Treatment", "Origin","Temperature***"), cex = 0.9)
legend("topright", bty="n", c("Tr x O","Tr x Te","O x Te","Tr x O x Te"), cex = 0.9)

#########################################################
#########################################################


#########################################################
#########################################################

df<-subset(source, R_calc == "R.mass")
df<-summaryBy(rdref ~ MoYr + Species + Source + trt, FUN = c(mean, std.error), na.rm = T, df)

plotCI(df$MoYr, df$rdref.mean, df$rdref.std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(0,30), xlim = c(start, end))
axis(2, at = seq(0,30,5), las = 2)
axis.Date(1, sdf$MoYr, at = seq(start, end, "month"), las = 2, format = "%m/%Y", labels = F)

par(new=T)
dat<-subset(df, Species == "ag" & Source == "fl" & trt == "a")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=2, col = "blue", ylim = c(0,30), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "blue", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "ag" & Source == "fl" & trt == "w")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=2, col = "firebrick", ylim = c(0,30), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "ag" & Source == "bz" & trt == "a")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=6, col = "blue", ylim = c(0,30), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "blue", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "ag" & Source == "bz" & trt == "w")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=6, col = "firebrick", ylim = c(0,30), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 2)

mtext(side = 2, expression(italic(R)[mass]*''^25~(nmol~g^-1~s^-1)), cex = 0.9, padj = -1.7)

legend("topleft", "e)", bty = "n")

dum<-subset(source, Species == "ag" & R_calc == "R.mass")
m1<-lm(rdref ~ Treatment * Source * Timepoint, dum)
Anova(m1); shapiro.test(m1$residuals)

legend("top", bty="n", c("Treatment", "Origin","Date***"), cex = 0.9)
legend("topright", bty="n", c("Tr x O","Tr x D","O x D*","Tr x O x D"), cex = 0.9)

######################

df<-subset(source, R_calc == "R.mass")
df<-summaryBy(rdref ~ MoYr + Species + Source + trt, FUN = c(mean, std.error), na.rm = T, df)

plotCI(df$MoYr, df$rdref.mean, df$rdref.std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(0,30), xlim = c(start, end))
axis(2, at = seq(0,30,5), las = 2)
axis.Date(1, sdf$MoYr, at = seq(start, end, "month"), las = 2, format = "%m/%Y", labels = F)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "fl" & trt == "a")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=2, col = "blue", ylim = c(0,30), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "blue", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "fl" & trt == "w")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=2, col = "firebrick", ylim = c(0,30), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "bz" & trt == "a")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=6, col = "blue", ylim = c(0,30), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "blue", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "bz" & trt == "w")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=6, col = "firebrick", ylim = c(0,30), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 2)

legend("topleft", "f)", bty = "n")

dum<-subset(source, Species == "rm" & R_calc == "R.mass")
m1<-lm(rdref ~ Treatment * Source * Timepoint, dum)
Anova(m1); shapiro.test(m1$residuals)

legend("top", bty="n", c("Treatment", "Origin","Date***"), cex = 0.9)
legend("topright", bty="n", c("Tr x O","Tr x D","O x D","Tr x O x D"), cex = 0.9)

######################

df<-subset(source, R_calc == "R.mass")

dat<-subset(df, Treatment == "Ambient" & Species == "ag" & Source == "fl")
plot(dat$rdref ~ dat$Temp_C.mean, xlim = c(10,30), ylim = c(0,30), pch = 2, col = "blue",
     xaxt="n",yaxt="n",xlab="",ylab="")
dat<-subset(df, Treatment == "Warmed" & Species == "ag" & Source == "fl")
points(dat$rdref ~ dat$Temp_C.mean, pch = 2, col = "firebrick")
dat<-subset(df, Treatment == "Warmed" & Species == "ag" & Source == "bz")
points(dat$rdref ~ dat$Temp_C.mean, pch = 6, col = "firebrick")
dat<-subset(df, Treatment == "Ambient" & Species == "ag" & Source == "bz")
points(dat$rdref ~ dat$Temp_C.mean, pch = 6, col = "blue")
axis(1, at = seq(0,100,5), labels = F)
axis(2, at = seq(0,30,5), las = 2)

legend("topleft", "g)", bty = "n")

dum<-subset(source, Species == "ag" & R_calc == "R.mass")
m1<-lm(rdref ~ Treatment * Source * Temp_C.mean, dum)
Anova(m1); shapiro.test(m1$residuals)
# x<-plot_model(m1, type = c("pred"), terms = c("Temp_C.mean"))
# newdata<-as.data.frame(x$data)
# points(predicted ~ x, newdata, lty = 1, type = "l")
legend("top", bty="n", c("Treatment", "Origin","Temperature"), cex = 0.9)
legend("topright", bty="n", c("Tr x O","Tr x Te","O x Te","Tr x O x Te"), cex = 0.9)

######################

dat<-subset(df, Treatment == "Ambient" & Species == "rm" & Source == "fl")
plot(dat$rdref ~ dat$Temp_C.mean, xlim = c(10,30), ylim = c(0,30), pch = 2, col = "blue",
     xaxt="n",yaxt="n",xlab="",ylab="")
dat<-subset(df, Treatment == "Warmed" & Species == "rm" & Source == "fl")
points(dat$rdref ~ dat$Temp_C.mean, pch = 2, col = "firebrick")
dat<-subset(df, Treatment == "Warmed" & Species == "rm" & Source == "bz")
points(dat$rdref ~ dat$Temp_C.mean, pch = 6, col = "firebrick")
dat<-subset(df, Treatment == "Ambient" & Species == "rm" & Source == "bz")
points(dat$rdref ~ dat$Temp_C.mean, pch = 6, col = "blue")
axis(1, at = seq(0,100,5), labels = F)
axis(2, at = seq(0,30,5), las = 2)

legend("topleft", "h)", bty = "n")

dum<-subset(source, Species == "rm" & R_calc == "R.mass")
m1<-lm(rdref ~ Treatment * Source * Temp_C.mean, dum)
Anova(m1); shapiro.test(m1$residuals)
x<-plot_model(m1, type = c("pred"), terms = c("Temp_C.mean"))
newdata<-as.data.frame(x$data)
points(predicted ~ x, newdata, lty = 1, type = "l")
legend("top", bty="n", c("Treatment", "Origin","Temperature**"), cex = 0.9)
legend("topright", bty="n", c("Tr x O","Tr x Te","O x Te","Tr x O x Te"), cex = 0.9)

#########################################################
#########################################################





#########################################################
#########################################################

df<-subset(source, R_calc == "R.area")
df<-summaryBy(q10 ~ MoYr + Species + Source + trt, FUN = c(mean, std.error), na.rm = T, df)

plotCI(df$MoYr, df$q10.mean, df$q10.std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(1,5.5), xlim = c(start, end))
axis(2, at = seq(0.5,6.5,1), las = 2)
axis.Date(1, sdf$MoYr, at = seq(start, end, "month"), las = 2, format = "%m/%Y", labels = F)

par(new=T)
dat<-subset(df, Species == "ag" & Source == "fl" & trt == "a")
plotCI(dat$MoYr, dat$q10.mean, dat$q10.std.error, pch=2, col = "blue", ylim = c(1,5.5), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(q10.mean ~ dat$MoYr, type="l", col = "blue", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "ag" & Source == "fl" & trt == "w")
plotCI(dat$MoYr, dat$q10.mean, dat$q10.std.error, pch=2, col = "firebrick", ylim = c(1,5.5), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(q10.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "ag" & Source == "bz" & trt == "a")
plotCI(dat$MoYr, dat$q10.mean, dat$q10.std.error, pch=6, col = "blue", ylim = c(1,5.5), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(q10.mean ~ dat$MoYr, type="l", col = "blue", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "ag" & Source == "bz" & trt == "w")
plotCI(dat$MoYr, dat$q10.mean, dat$q10.std.error, pch=6, col = "firebrick", ylim = c(1,5.5), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(q10.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 2)

mtext(side = 2, expression(italic(Q)[10]), cex = 0.9, padj = -2.6)

legend("topleft", "i)", bty = "n")

dum<-subset(source, Species == "ag" & R_calc == "R.area")
m1<-lm(q10 ~ Treatment * Source * Timepoint, dum)
Anova(m1); shapiro.test(m1$residuals)
legend("top", bty="n", c("Treatment", "Origin*","Date***"), cex = 0.9)
legend("topright", bty="n", c("Tr x O*","Tr x D","O x D***","Tr x O x D"), cex = 0.9)

######################

df<-subset(source, R_calc == "R.area")
df<-summaryBy(q10 ~ MoYr + Species + Source + trt, FUN = c(mean, std.error), na.rm = T, df)

plotCI(df$MoYr, df$q10.mean, df$q10.std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(1,5.5), xlim = c(start, end))
axis(2, at = seq(0.5,6.5,1), las = 2)
axis.Date(1, sdf$MoYr, at = seq(start, end, "month"), las = 2, format = "%m/%Y", labels = F)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "fl" & trt == "a")
plotCI(dat$MoYr, dat$q10.mean, dat$q10.std.error, pch=2, col = "blue", ylim = c(1,5.5), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(q10.mean ~ dat$MoYr, type="l", col = "blue", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "fl" & trt == "w")
plotCI(dat$MoYr, dat$q10.mean, dat$q10.std.error, pch=2, col = "firebrick", ylim = c(1,5.5), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(q10.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "bz" & trt == "a")
plotCI(dat$MoYr, dat$q10.mean, dat$q10.std.error, pch=6, col = "blue", ylim = c(1,5.5), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(q10.mean ~ dat$MoYr, type="l", col = "blue", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "bz" & trt == "w")
plotCI(dat$MoYr, dat$q10.mean, dat$q10.std.error, pch=6, col = "firebrick", ylim = c(1,5.5), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(q10.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 2)

legend("topleft", "j)", bty = "n")

dum<-subset(source, Species == "rm" & R_calc == "R.mass")
m1<-lm(q10 ~ Treatment * Source * Timepoint, dum)
Anova(m1); shapiro.test(m1$residuals)
legend("top", bty="n", c("Treatment", "Origin","Date"), cex = 0.9)
legend("topright", bty="n", c("Tr x O*","Tr x D","O x D","Tr x O x D"), cex = 0.9)


######################

df<-subset(source, R_calc == "R.mass")

dat<-subset(df, Treatment == "Ambient" & Species == "ag" & Source == "fl")
plot(dat$q10 ~ dat$Temp_C.mean, xlim = c(10,30), ylim = c(1,5.5), pch = 2, col = "blue",
     xaxt="n",yaxt="n",xlab="",ylab="")
dat<-subset(df, Treatment == "Warmed" & Species == "ag" & Source == "fl")
points(dat$q10 ~ dat$Temp_C.mean, pch = 2, col = "firebrick")
dat<-subset(df, Treatment == "Warmed" & Species == "ag" & Source == "bz")
points(dat$q10 ~ dat$Temp_C.mean, pch = 6, col = "firebrick")
dat<-subset(df, Treatment == "Ambient" & Species == "ag" & Source == "bz")
points(dat$q10 ~ dat$Temp_C.mean, pch = 6, col = "blue")
axis(1, at = seq(0,100,5), labels = F)
axis(2, at = seq(0.5,6.5,1), las = 2)

legend("topleft", "k)", bty = "n")

dum<-subset(source, Species == "ag" & R_calc == "R.mass")
m1<-lm(q10 ~ Treatment * Source * Temp_C.mean, dum)
Anova(m1); shapiro.test(m1$residuals)
x<-plot_model(m1, type = c("pred"), terms = c("Temp_C.mean","Source"))
newdata<-as.data.frame(x$data)

# dum<-subset(newdata, group == "bz")
# points(predicted ~ x, dum, lty = 2, type = "l")
# dum<-subset(newdata, group == "fl")
# points(predicted ~ x, dum, lty = 3, type = "l")
# legend("bottomleft", horiz = T, c("BZ","FL"), lty = c(2,3), bty = "n", cex = 0.9)

# rm(dum1)

legend("top", bty="n", c("Treatment", "Origin","Temperature"), cex = 0.9)
legend("topright", bty="n", c("Tr x O*","Tr x Te","O x Te","Tr x O x Te"), cex = 0.9)

######################

dat<-subset(df, Treatment == "Ambient" & Species == "rm" & Source == "fl")
plot(dat$q10 ~ dat$Temp_C.mean, xlim = c(10,30), ylim = c(1,5.5), pch = 2, col = "blue",
     xaxt="n",yaxt="n",xlab="",ylab="")
dat<-subset(df, Treatment == "Warmed" & Species == "rm" & Source == "fl")
points(dat$q10 ~ dat$Temp_C.mean, pch = 2, col = "firebrick")
dat<-subset(df, Treatment == "Warmed" & Species == "rm" & Source == "bz")
points(dat$q10 ~ dat$Temp_C.mean, pch = 6, col = "firebrick")
dat<-subset(df, Treatment == "Ambient" & Species == "rm" & Source == "bz")
points(dat$q10 ~ dat$Temp_C.mean, pch = 6, col = "blue")
axis(1, at = seq(0,100,5), labels = F)
axis(2, at = seq(0.5,6.5,1), las = 2)

legend("topleft", "l)", bty = "n")

dum<-subset(source, Species == "rm" & R_calc == "R.mass")
m1<-lm(q10 ~ Treatment * Source * Temp_C.mean, dum)
Anova(m1); shapiro.test(m1$residuals)
legend("top", bty="n", c("Treatment", "Origin","Temperature*"), cex = 0.9)
legend("topright", bty="n", c("Tr x O*","Tr x Te","O x Te*","Tr x O x Te"), cex = 0.9)


#########################################################
#########################################################








#########################################################
#########################################################

df<-subset(source, R_calc == "R.area")
df<-summaryBy(LMA_g.m2 ~ MoYr + Species + Source + trt, FUN = c(mean, std.error), na.rm = T, df)

plotCI(df$MoYr, df$LMA_g.m2.mean, df$LMA_g.m2.std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(8,20), xlim = c(start, end))
axis(2, at = seq(0,20,5), las = 2)

par(new=T)
dat<-subset(df, Species == "ag" & Source == "fl" & trt == "a")
plotCI(dat$MoYr, dat$LMA_g.m2.mean, dat$LMA_g.m2.std.error, pch=2, col = "blue", ylim = c(8,20), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(LMA_g.m2.mean ~ dat$MoYr, type="l", col = "blue", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "ag" & Source == "fl" & trt == "w")
plotCI(dat$MoYr, dat$LMA_g.m2.mean, dat$LMA_g.m2.std.error, pch=2, col = "firebrick", ylim = c(8,20), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(LMA_g.m2.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "ag" & Source == "bz" & trt == "a")
plotCI(dat$MoYr, dat$LMA_g.m2.mean, dat$LMA_g.m2.std.error, pch=6, col = "blue", ylim = c(8,20), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(LMA_g.m2.mean ~ dat$MoYr, type="l", col = "blue", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "ag" & Source == "bz" & trt == "w")
plotCI(dat$MoYr, dat$LMA_g.m2.mean, dat$LMA_g.m2.std.error, pch=6, col = "firebrick", ylim = c(8,20), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(LMA_g.m2.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 2)

axis.Date(1, sdf$MoYr, at = seq(start, end, "month"), las = 2, format = "%m/%y")

mtext(side = 2, expression(LMA~(g~m^-2)), cex = 0.9, padj = -1.6)

legend("topleft", "m)", bty = "n")


dum<-subset(source, Species == "ag" & R_calc == "R.mass")
m1<-lm(LMA_g.m2 ~ Treatment * Source * Timepoint, dum)
Anova(m1); shapiro.test(m1$residuals)

legend("top", bty="n", c("Treatment", "Origin","Date***"), cex = 0.9)
legend("topright", bty="n", c("Tr x O","Tr x D","O x D","Tr x O x D"), cex = 0.9)

######################

df<-subset(source, R_calc == "R.area")
df<-summaryBy(LMA_g.m2 ~ MoYr + Species + Source + trt, FUN = c(mean, std.error), na.rm = T, df)

plotCI(df$MoYr, df$LMA_g.m2.mean, df$LMA_g.m2.std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(8,20), xlim = c(start, end))
axis(2, at = seq(0,20,5), las = 2)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "fl" & trt == "a")
plotCI(dat$MoYr, dat$LMA_g.m2.mean, dat$LMA_g.m2.std.error, pch=2, col = "blue", ylim = c(8,20), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(LMA_g.m2.mean ~ dat$MoYr, type="l", col = "blue", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "fl" & trt == "w")
plotCI(dat$MoYr, dat$LMA_g.m2.mean, dat$LMA_g.m2.std.error, pch=2, col = "firebrick", ylim = c(8,20), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(LMA_g.m2.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "bz" & trt == "a")
plotCI(dat$MoYr, dat$LMA_g.m2.mean, dat$LMA_g.m2.std.error, pch=6, col = "blue", ylim = c(8,20), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(LMA_g.m2.mean ~ dat$MoYr, type="l", col = "blue", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "bz" & trt == "w")
plotCI(dat$MoYr, dat$LMA_g.m2.mean, dat$LMA_g.m2.std.error, pch=6, col = "firebrick", ylim = c(8,20), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(LMA_g.m2.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 2)

axis.Date(1, sdf$MoYr, at = seq(start, end, "month"), las = 2, format = "%m/%y")

legend("topleft", "n)", bty = "n")


dum<-subset(source, Species == "rm" & R_calc == "R.mass")
m1<-lm(LMA_g.m2 ~ Treatment * Source * Timepoint, dum)
Anova(m1); shapiro.test(m1$residuals)

legend("top", bty="n", c("Treatment", "Origin","Date***"), cex = 0.9)
legend("topright", bty="n", c("Tr x O","Tr x D","O x D","Tr x O x D"), cex = 0.9)


######################

df<-subset(source, R_calc == "R.area")

dat<-subset(df, Treatment == "Ambient" & Species == "ag" & Source == "fl")
plot(dat$LMA_g.m2 ~ dat$Temp_C.mean, xlim = c(10,30), ylim = c(5,25), pch = 2, col = "blue",
     xaxt="n",yaxt="n",xlab="",ylab="")
dat<-subset(df, Treatment == "Warmed" & Species == "ag" & Source == "fl")
points(dat$LMA_g.m2 ~ dat$Temp_C.mean, pch = 2, col = "firebrick")
dat<-subset(df, Treatment == "Warmed" & Species == "ag" & Source == "bz")
points(dat$LMA_g.m2 ~ dat$Temp_C.mean, pch = 6, col = "firebrick")
dat<-subset(df, Treatment == "Ambient" & Species == "ag" & Source == "bz")
points(dat$LMA_g.m2 ~ dat$Temp_C.mean, pch = 6, col = "blue")
axis(1, at = seq(0,100,5), cex.axis = 1.2)
axis(2, at = seq(0,30,5), las = 2)

legend("topleft", "o)", bty = "n")

dum<-subset(source, Species == "ag" & R_calc == "R.mass")
m1<-lm(LMA_g.m2 ~ Treatment * Source * Temp_C.mean, dum)
Anova(m1); shapiro.test(m1$residuals)

legend("top", bty="n", c("Treatment", "Origin","Temperature*"), cex = 0.9)
legend("topright", bty="n", c("Tr x O","Tr x Te","O x Te","Tr x O x Te"), cex = 0.9)

######################

dat<-subset(df, Treatment == "Ambient" & Species == "rm" & Source == "fl")
plot(dat$LMA_g.m2 ~ dat$Temp_C.mean, xlim = c(10,30), ylim = c(5,25), pch = 2, col = "blue",
     xaxt="n",yaxt="n",xlab="",ylab="")
dat<-subset(df, Treatment == "Warmed" & Species == "rm" & Source == "fl")
points(dat$LMA_g.m2 ~ dat$Temp_C.mean, pch = 2, col = "firebrick")
dat<-subset(df, Treatment == "Warmed" & Species == "rm" & Source == "bz")
points(dat$LMA_g.m2 ~ dat$Temp_C.mean, pch = 6, col = "firebrick")
dat<-subset(df, Treatment == "Ambient" & Species == "rm" & Source == "bz")
points(dat$LMA_g.m2 ~ dat$Temp_C.mean, pch = 6, col = "blue")
axis(1, at = seq(0,100,5),cex.axis = 1.2)
axis(2, at = seq(0,30,5), las = 2)

legend("topleft", "p)", bty = "n")

dum<-subset(source, Species == "rm" & R_calc == "R.mass")
m1<-lm(LMA_g.m2 ~ Treatment * Source * Temp_C.mean, dum)
Anova(m1); shapiro.test(m1$residuals)
x<-plot_model(m1, type = c("pred"), terms = c("Temp_C.mean"))
newdata<-as.data.frame(x$data)
points(predicted ~ x, newdata, lty = 1, type = "l")
legend("top", bty="n", c("Treatment", "Origin","Temperature"), cex = 0.9)
legend("topright", bty="n", c("Tr x O","Tr x Te","O x Te","Tr x O x Te"), cex = 0.9)


##################################################################
############################################
##################################################################

mtext(side = 1, outer = T, adj = 0.2, padj = 3.3, "Date (mm-yy)", cex = 1.2)

mtext(side = 1, outer = T, adj = 0.8, padj = 2.1, expression(italic(T)[air]~over~5~days~(degree*C)), cex = 1.2)





dev.off()