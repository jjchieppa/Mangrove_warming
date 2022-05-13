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
source$LMA_g.m2<-source$LMA_kg.m2*100
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

tiff(file = "V_Mangrove Physiology ~ Date (Rhizophora).tiff", height = 8, width = 12, res = 600, units = "in", compression = "zip+p")

par(mfrow = c(4,2), omi = c(0.75,0.5,0.1,0.01), mar = c(1.2,1.6,1.2,1.6))

# Rarea AG ####

df<-subset(source, R_calc == "R.area")
df<-summaryBy(rdref ~ MoYr + Species + Source + trt, FUN = c(mean, std.error), na.rm = T, df)

plotCI(df$MoYr, df$rdref.mean, df$rdref.std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(0.35,2.75), xlim = c(start, end))
axis(2, at = seq(0.5,6.5,0.5), las = 2)
axis.Date(1, sdf$MoYr, at = seq(start, end, "month"), las = 2, format = "%m/%Y", labels = F)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "fl" & trt == "a")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=2, col = "dodgerblue4", ylim = c(0.35,2.75), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",cex=1.500,xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "dodgerblue4", dat, lty = 1)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "fl" & trt == "w")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=2, col = "firebrick", ylim = c(0.35,2.75), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",cex=1.500,xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 1)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "bz" & trt == "a")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=6, col = "dodgerblue4", ylim = c(0.35,2.75), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",cex=1.500,xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "dodgerblue4", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "bz" & trt == "w")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=6, col = "firebrick", ylim = c(0.35,2.75), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",cex=1.500,xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 2)

mtext(side = 2, expression(italic(R)[area]*''^25~(mu*mol~m^-2~s^-1)), cex = 0.9, padj = -1.7)

legend("topleft", "a)", bty = "n")

legend("bottomleft", horiz = F, pch = c(2,6), c("FL", "BZ"), bty = "n",
       col = c("black","black"), cex = 1.1, lty = c(1,2))
legend("bottomright", horiz = F, pch = c(15,15), c("Ambient", "Warmed"), bty = "n",
       col = c("dodgerblue4","firebrick"), cex = 1.2)

dum<-subset(source, Species == "rm" & R_calc == "R.area")
leveneTest(rdref ~ Timepoint * Source * Treatment, dum)
m1<-lm(rdref ~ Treatment * Source * Timepoint, dum)
Anova(m1); shapiro.test(m1$residuals)

legend("top", bty="n", c("Treatment", "Population","Date***"), cex = 1)
legend("topright", bty="n", c("T x P","T x D","P x D*","T x P x D"), cex = 1)

# **Rarea AG ####

df<-subset(source, R_calc == "R.area")
df<-subset(df, Species == "rm")
df<-summaryBy(rdref ~ MoYr * Source, FUN = c(mean, std.error), na.rm = T, df)

plotCI(df$MoYr, df$rdref.mean, df$rdref.std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(0.35,2.75), xlim = c(start, end))
axis(2, at = seq(0.5,6.5,0.5), las = 2)
axis.Date(1, sdf$MoYr, at = seq(start, end, "month"), las = 2, format = "%m/%Y", labels = F)

par(new = T)
dum<-subset(df, Source == "fl")
plotCI(dum$MoYr, dum$rdref.mean, dum$rdref.std.error, sfrac = 0, col = "black",
       xaxt="n",yaxt="n",xlab="",ylab="",pch=2, ylim = c(0.35,2.75), xlim = c(start, end), cex = 1.500)
points(rdref.mean ~ MoYr, type = "l", lty = 1, dum)

par(new = T)
dum<-subset(df, Source == "bz")
plotCI(dum$MoYr, dum$rdref.mean, dum$rdref.std.error, sfrac = 0, col = "black",
       xaxt="n",yaxt="n",xlab="",ylab="",pch=6, ylim = c(0.35,2.75), xlim = c(start, end), cex = 1.500)
points(rdref.mean ~ MoYr, type = "l", lty = 2, dum)

legend("topleft", "b)", bty = "n")

dum<-subset(source, Species == "rm" & R_calc == "R.area")
m1<-lm(rdref ~ Treatment * Source * Timepoint, dum)
em1<-emmeans(m1, ~Timepoint:Source)
em<-cld(em1)
em<-data.frame(em)
em<-em[order(em$Timepoint),]

em1<-subset(em, Source == "bz")
tuk<-c("cd", "abcd", "abcd", "abc", "abc", "ab", "abcd")
em1$tuk<-tuk
em1$Timepoint<-as.Date(em1$Timepoint)
em1<-em1[order(em1$Timepoint),]
em1$yy<-2
text(em1$Timepoint, em1$yy, em1$tuk, cex = 1.2)

em1<-subset(em, Source == "fl")
tuk<-c("abc", "abcd", "bcd", "abc", "a", "abc", "d")
em1$tuk<-tuk
em1$Timepoint<-as.Date(em1$Timepoint)
em1<-em1[order(em1$Timepoint),]
em1$yy<-2.25
text(em1$Timepoint, em1$yy, em1$tuk, cex = 1.2)

text(as.Date("2020-04-12"), 2, "BZ:", cex = 1.2)
text(as.Date("2020-04-12"), 2.25, "FL:", cex = 1.2)

legend("top", horiz = T, bty = "n", cex = 1.2, c(expression(italic(F)~'= 2.69;'~italic(P)~'< 0.05')))

# Rmass AG ####

df<-subset(source, R_calc == "R.mass")
df<-summaryBy(rdref ~ MoYr + Species + Source + trt, FUN = c(mean, std.error), na.rm = T, df)

plotCI(df$MoYr, df$rdref.mean, df$rdref.std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(0,20), xlim = c(start, end))
axis(2, at = seq(0,30,5), las = 2)
axis.Date(1, sdf$MoYr, at = seq(start, end, "month"), las = 2, format = "%m/%Y", labels = F)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "fl" & trt == "a")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=2, col = "dodgerblue4", ylim = c(0,20), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",cex=1.500,xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "dodgerblue4", dat, lty = 1)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "fl" & trt == "w")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=2, col = "firebrick", ylim = c(0,20), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",cex=1.500,xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 1)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "bz" & trt == "a")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=6, col = "dodgerblue4", ylim = c(0,20), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",cex=1.500,xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "dodgerblue4", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "bz" & trt == "w")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=6, col = "firebrick", ylim = c(0,20), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",cex=1.500,xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 2)

mtext(side = 2, expression(italic(R)[mass]*''^25~(nmol~g^-1~s^-1)), cex = 0.9, padj = -1.7)

legend("topleft", "c)", bty = "n")

dum<-subset(source, Species == "rm" & R_calc == "R.mass")
m1<-lm(rdref ~ Treatment * Source * Timepoint, dum)
Anova(m1); shapiro.test(m1$residuals)

legend("top", bty="n", c("Treatment", "Population","Date***"), cex = 1)
legend(as.Date("2020-10-10"), 20, bty="n", c("T x P","T x D","P x D","T x P x D"), cex = 1)

# **Rmass AG ####

df<-subset(source, R_calc == "R.mass")
df<-summaryBy(rdref ~ MoYr * Species, FUN = c(mean, std.error), na.rm = T, df)
df<-subset(df, Species == "rm")
MoYr<-df$MoYr
tuk<-c("abc","ab","bc","ab","a","ab","c")
df1<-data.frame(MoYr, tuk)

plotCI(df$MoYr, df$rdref.mean, df$rdref.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, ylim = c(0,20), xlim = c(start, end), cex = 1.500)
points(df$rdref.mean ~ df$MoYr, type = "l", lty = 2)
axis(2, at = seq(0,30,5), las = 2)
axis.Date(1, sdf$MoYr, at = seq(start, end, "month"), las = 2, format = "%m/%Y", labels = F)

legend("topleft", "d)", bty = "n")

text(df1$MoYr, 5, paste(df1$tuk), cex = 1.2)

dum<-subset(source, Species == "rm" & R_calc == "R.mass")
leveneTest(rdref ~ Treatment * Source * Timepoint, dum)
m1<-lm(rdref ~ Timepoint, dum)
em1<-emmeans(m1, ~Timepoint); cld(em1)

legend("top", horiz = T, bty = "n", cex = 1.2, c(expression(italic(F)~'= 4.56;'~italic(P)~'< 0.001')))

# Q10 AG ####

df<-subset(source, R_calc == "R.area")
df<-summaryBy(q10 ~ MoYr + Species + Source + trt, FUN = c(mean, std.error), na.rm = T, df)

plotCI(df$MoYr, df$q10.mean, df$q10.std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(2,3), xlim = c(start, end))
axis(2, at = seq(0.5,6.5,0.25), las = 2)
axis.Date(1, sdf$MoYr, at = seq(start, end, "month"), las = 2, format = "%m/%Y", labels = F)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "fl" & trt == "a")
plotCI(dat$MoYr, dat$q10.mean, dat$q10.std.error, pch=2, col = "dodgerblue4", ylim = c(2,3), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",cex=1.500,xlab="",ylab="")
points(q10.mean ~ dat$MoYr, type="l", col = "dodgerblue4", dat, lty = 1)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "fl" & trt == "w")
plotCI(dat$MoYr, dat$q10.mean, dat$q10.std.error, pch=2, col = "firebrick", ylim = c(2,3), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",cex=1.500,xlab="",ylab="")
points(q10.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 1)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "bz" & trt == "a")
plotCI(dat$MoYr, dat$q10.mean, dat$q10.std.error, pch=6, col = "dodgerblue4", ylim = c(2,3), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",cex=1.500,xlab="",ylab="")
points(q10.mean ~ dat$MoYr, type="l", col = "dodgerblue4", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "bz" & trt == "w")
plotCI(dat$MoYr, dat$q10.mean, dat$q10.std.error, pch=6, col = "firebrick", ylim = c(2,3), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",cex=1.500,xlab="",ylab="")
points(q10.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 2)

mtext(side = 2, expression(italic(Q)[10]), cex = 0.9, padj = -2.8)

legend("topleft", "e)", bty = "n")

dum<-subset(source, Species == "rm" & R_calc == "R.area")
m1<-lm(q10 ~ Treatment * Source * Timepoint, dum)
Anova(m1); shapiro.test(m1$residuals)

legend("top", bty="n", c("Treatment", "Population","Date"), cex = 1)
legend("topright", bty="n", c("T x P*","T x D","P x D","T x P x D"), cex = 1)

# **Q10 AG ####

df<-subset(source, R_calc == "R.area")
df<-summaryBy(q10 ~ Treatment * Species + Source, FUN = c(mean, std.error), na.rm = T, df)
df<-subset(df, Species == "rm")

xx<-c(-500,500); yy<-xx
plot(yy ~ xx, xaxt="n",yaxt="n",xlab="",ylab="", xlim = c(0,9), ylim = c(2,3))
axis(2, at = seq(2,3,0.25), las = 2)

dum<-subset(df, Treatment == "Ambient" & Source == "fl"); dum$y<-1
rect(0, 0, 2, dum$q10.mean, col = "dodgerblue4")
ablineclip(v = 1, y1 = (dum$q10.mean-dum$q10.std.error), y2 = (dum$q10.mean+dum$q10.std.error))
points(dum$q10.mean ~ dum$y, pch = 24, col = "black", bg = "dodgerblue4", cex = 2.5)
dum<-subset(df, Treatment == "Warmed" & Source == "fl"); dum$y<-3
rect(2, 0, 4, dum$q10.mean, col = "firebrick")
ablineclip(v = 3, y1 = (dum$q10.mean-dum$q10.std.error), y2 = (dum$q10.mean+dum$q10.std.error))
points(dum$q10.mean ~ dum$y, pch = 24, col = "black", bg = "firebrick", cex = 2.5)

dum<-subset(df, Treatment == "Ambient" & Source == "bz"); dum$y<-6
rect(5, 0, 7, dum$q10.mean, col = "dodgerblue4")
ablineclip(v = 6, y1 = (dum$q10.mean-dum$q10.std.error), y2 = (dum$q10.mean+dum$q10.std.error))
points(dum$q10.mean ~ dum$y, pch = 25, col = "black", bg = "dodgerblue4", cex = 2.5)
dum<-subset(df, Treatment == "Warmed" & Source == "bz"); dum$y<-8
rect(7, 0, 9, dum$q10.mean, col = "firebrick")
ablineclip(v = 8, y1 = (dum$q10.mean-dum$q10.std.error), y2 = (dum$q10.mean+dum$q10.std.error))
points(dum$q10.mean ~ dum$y, pch = 25, col = "black", bg = "firebrick", cex = 2.5)
box()
text(c(1,3,6,8), 2.8, c("a","a","a","a"), cex = 1.2)
axis(1, at = c(2,7), c("FL","BZ"))
legend("topleft", "f)", bty = "n")

legend("top", horiz = T, bty = "n", cex = 1.2, c(expression(italic(F)~'= 3.96;'~italic(P)~'< 0.05')))

# LMA AG ####

df<-subset(source, R_calc == "R.area")
df<-summaryBy(LMA_g.m2 ~ MoYr + Species + Source + trt, FUN = c(mean, std.error), na.rm = T, df)

plotCI(df$MoYr, df$LMA_g.m2.mean, df$LMA_g.m2.std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(8,20), xlim = c(start, end))
axis(2, at = seq(0,20,5), las = 2)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "fl" & trt == "a")
plotCI(dat$MoYr, dat$LMA_g.m2.mean, dat$LMA_g.m2.std.error, pch=2, col = "dodgerblue4", ylim = c(8,20), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",cex=1.500,xlab="",ylab="")
points(LMA_g.m2.mean ~ dat$MoYr, type="l", col = "dodgerblue4", dat, lty = 1)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "fl" & trt == "w")
plotCI(dat$MoYr, dat$LMA_g.m2.mean, dat$LMA_g.m2.std.error, pch=2, col = "firebrick", ylim = c(8,20), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",cex=1.500,xlab="",ylab="")
points(LMA_g.m2.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 1)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "bz" & trt == "a")
plotCI(dat$MoYr, dat$LMA_g.m2.mean, dat$LMA_g.m2.std.error, pch=6, col = "dodgerblue4", ylim = c(8,20), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",cex=1.500,xlab="",ylab="")
points(LMA_g.m2.mean ~ dat$MoYr, type="l", col = "dodgerblue4", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "bz" & trt == "w")
plotCI(dat$MoYr, dat$LMA_g.m2.mean, dat$LMA_g.m2.std.error, pch=6, col = "firebrick", ylim = c(8,20), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",cex=1.500,xlab="",ylab="")
points(LMA_g.m2.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 2)

axis.Date(1, sdf$MoYr, at = seq(start, end, "month"), las = 2, format = "%m/%y")

mtext(side = 2, expression(LMA~(g~m^-2)), cex = 0.9, padj = -1.6)

legend("topleft", "g)", bty = "n")


dum<-subset(source, Species == "rm" & R_calc == "R.mass")
m1<-lm(LMA_g.m2 ~ Treatment * Source * Timepoint, dum)
Anova(m1); shapiro.test(m1$residuals)

legend("top", bty="n", c("Treatment", "Population","Date***"), cex = 1)
legend("topright", bty="n", c("T x P","T x D","P x D","T x P x D"), cex = 1)

# **LMA AG ####

df<-subset(source, R_calc == "R.area")
df<-summaryBy(LMA_g.m2 ~ MoYr + Species, FUN = c(mean, std.error), na.rm = T, df)
df<-subset(df, Species == "rm")
tuk<-c("bc","c","a","ab","ab","ab","ab")
df$tuk<-tuk

plotCI(df$MoYr, df$LMA_g.m2.mean, df$LMA_g.m2.std.error, sfrac = 0, col = "black",
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, ylim = c(8,20), xlim = c(start, end), cex = 1.500)
points(LMA_g.m2.mean ~ df$MoYr, type="l", col = "black", df, lty = 2)
axis(2, at = seq(0,20,5), las = 2)
axis.Date(1, sdf$MoYr, at = seq(start, end, "month"), las = 2, format = "%m/%y")

legend("topleft", "h)", bty = "n")

text(df$MoYr, 18, paste(df$tuk), cex = 1.2)

dum<-subset(source, Species == "rm" & R_calc == "R.mass")
m1<-lm(LMA_g.m2 ~ Timepoint, dum)
em1<-emmeans(m1, ~Timepoint); cld(em1)

legend("bottom", horiz = T, bty = "n", cex = 1.2, c(expression(italic(F)~'= 7.72;'~italic(P)~'< 0.001')))

# off ####
mtext(side = 1, outer = T, padj = 3.3, "Date (mm-yy)", cex = 1.2)
dev.off(); 1+1
