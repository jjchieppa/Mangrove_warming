library(lattice);library(doBy); library(lubridate)
library(minpack.lm); library(effects); library(plotrix)

rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

#########################################################
#########################################################

home<-"C:/Users/N01456074/OneDrive - University of North Florida/Other Projects/Belize Florida Mangroves Exp/Data Analysis"
work<-"C:/Users/N01456074/OneDrive - University of North Florida/Other Projects/Belize Florida Mangroves Exp" # work
raw<-"C:/Users/N01456074/OneDrive - University of North Florida/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/raw"
hist<-"C:/Users/N01456074/OneDrive - University of North Florida/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/hist"
lins<-"C:/Users/N01456074/OneDrive - University of North Florida/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/lins"
deets<-"C:/Users/N01456074/OneDrive - University of North Florida/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/deets"
env<-"C:/Users/N01456074/OneDrive - University of North Florida/Other Projects/Belize Florida Mangroves Exp/Weather Data"
anly<-"C:/Users/N01456074/OneDrive - University of North Florida/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/Analysis"

#########################################################
#########################################################

### Bring in the ref respiration data, bind area and mass for LMA

setwd(deets)
source<-read.csv("MangroveRVT_Q10mod_Rarea and Rmass at 25C.csv")
source$Date<-as.Date(source$Date, format = "%Y-%m-%d")
setwd(work)
trts<-read.csv("Mangrove Treatment Assignments.csv")
source<-merge(source, trts, by = "UserIDs_in", all = F)
rm(trts)

source$Date<-as.Date(source$Date, format = "%Y-%m-%d") # Double checks date format!
source$MoYr<-floor_date(source$Date, "month") 

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

start<-as.Date("2020-03-28")
end<-as.Date("2020-05-05")

#########################################################
#########################################################

tiff(file = "V_Mangrove Overall Performance.tiff", height = 7, width = 10, res = 600, units = "in", compression = "zip+p")

par(mfrow = c(4,4), omi = c(0.4,0.4,0.05,0.01), mar = c(1.45,1.45,1.45,1.45))


#########################################################

df<-subset(source, R_calc == "R.area")
df<-summaryBy(rdref ~ MoYr + Species + Source + trt, FUN = c(mean, std.error), na.rm = T, df)

plotCI(df$MoYr, df$rdref.mean, df$rdref.std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(0.5,2.5), xlim = c(start, end))
axis(2, at = seq(0,5,0.5), las = 2)

par(new=T)
dat<-subset(df, Species == "ag" & Source == "fl" & trt == "a")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=2, col = "blue", ylim = c(0.5,2.5), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "blue", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "ag" & Source == "fl" & trt == "w")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=2, col = "firebrick", ylim = c(0.5,2.5), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "ag" & Source == "bz" & trt == "a")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=6, col = "blue", ylim = c(0.5,2.5), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "blue", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "ag" & Source == "bz" & trt == "w")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=6, col = "firebrick", ylim = c(0.5,2.5), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 2)

axis.Date(1, sdf$MoYr, at = seq(start, end, "month"), las = 2, format = "%m/%Y", labels = F)

mtext(side=3, expression(italic(A.~germinanas)))

######################

df<-subset(source, R_calc == "R.area")
df<-summaryBy(rdref ~ MoYr + Species + Source + trt, FUN = c(mean, std.error), na.rm = T, df)

plotCI(df$MoYr, df$rdref.mean, df$rdref.std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(0.5,2.5), xlim = c(start, end))
axis(2, at = seq(0,5,0.5), las = 2)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "fl" & trt == "a")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=2, col = "blue", ylim = c(0.5,2.5), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "blue", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "fl" & trt == "w")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=2, col = "firebrick", ylim = c(0.5,2.5), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "bz" & trt == "a")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=6, col = "blue", ylim = c(0.5,2.5), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "blue", dat, lty = 2)

par(new=T)
dat<-subset(df, Species == "rm" & Source == "bz" & trt == "w")
plotCI(dat$MoYr, dat$rdref.mean, dat$rdref.std.error, pch=6, col = "firebrick", ylim = c(0.5,2.5), 
       xlim = c(start,end), sfrac=0,xaxt="n",yaxt="n",xlab="",ylab="")
points(rdref.mean ~ dat$MoYr, type="l", col = "firebrick", dat, lty = 2)

axis.Date(1, sdf$MoYr, at = seq(start, end, "month"), las = 2, format = "%m/%Y", labels = F)

mtext(side=3, expression(italic(R.~mangle)))

######################

df<-subset(source, R_calc == "R.area")

dat<-subset(df, Treatment == "Ambient" & Species == "ag" & Source == "fl")
plot(dat$rdref ~ dat$Temp_C.mean, xlim = c(18,28), ylim = c(0,4), pch = 1, col = "blue",
     xaxt="n",yaxt="n",xlab="",ylab="")
dat<-subset(df, Treatment == "Warmed" & Species == "ag" & Source == "fl")
points(dat$rdref ~ dat$Temp_C.mean, pch = 1, col = "firebrick")
dat<-subset(df, Treatment == "Warmed" & Species == "ag" & Source == "bz")
points(dat$rdref ~ dat$Temp_C.mean, pch = 1, col = "firebrick")
dat<-subset(df, Treatment == "Ambient" & Species == "ag" & Source == "bz")
points(dat$rdref ~ dat$Temp_C.mean, pch = 1, col = "blue")
axis(1, at = seq(0,100,2), labels = F)
axis(2, at = seq(0,5,0.5), las = 2)

mtext(side=3, expression(italic(A.~germinanas)))

######################

dat<-subset(df, Treatment == "Ambient" & Species == "rm" & Source == "fl")
plot(dat$rdref ~ dat$Temp_C.mean, xlim = c(18,28), ylim = c(0,4), pch = 1, col = "blue",
     xaxt="n",yaxt="n",xlab="",ylab="")
dat<-subset(df, Treatment == "Warmed" & Species == "rm" & Source == "fl")
points(dat$rdref ~ dat$Temp_C.mean, pch = 1, col = "firebrick")
dat<-subset(df, Treatment == "Warmed" & Species == "rm" & Source == "bz")
points(dat$rdref ~ dat$Temp_C.mean, pch = 1, col = "firebrick")
dat<-subset(df, Treatment == "Ambient" & Species == "rm" & Source == "bz")
points(dat$rdref ~ dat$Temp_C.mean, pch = 1, col = "blue")
axis(1, at = seq(0,100,2), labels = F)
axis(2, at = seq(0,5,0.5), las = 2)

mtext(side=3, expression(italic(R.~mangle)))



dev.off()