# in data ####

library(lattice);library(doBy); library(lubridate)
library(minpack.lm); library(effects); library(plotrix)

rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

home<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis"
work<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp" # work
raw<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/raw"
hist<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/hist"
lins<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/lins"
deets<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/deets"
env<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Weather Data"
anly<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/Analysis"

### Bring in the ref respiration data, bind area and mass for LMA

setwd(deets)
source<-read.csv("MangroveRVT_LPRmod_Rarea and Rmass at 25C.csv")
source$Date<-as.Date(source$Date, format = "%Y-%m-%d")
setwd(work)
trts<-read.csv("Mangrove Treatment Assignments.csv")
source<-merge(source, trts, by = "UserIDs_in", all = F)
rm(trts)

source$Date<-as.Date(source$Date, format = "%Y-%m-%d") # Double checks date format!
source$MoYr<-floor_date(source$Date, "month") 

# env ####

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

new_env$SVP_kPa<-(610.78*(2.71828^(new_env$Temp_C/(new_env$Temp_C+238.3) * 17.2694)))/1000
new_env$VPD_kPa<-new_env$SVP_kPa * (1 - (new_env$RH_perc/100))
warm<-subset(new_env, Treatment == "Warm")
ambi<-subset(new_env, Treatment == "Ambi")
rm(new_env, env)

warm$VPD_kPa

warm<-summaryBy(Temp_C + RH_perc + VPD_kPa ~ Date, FUN = mean, na.rm = T, warm)
ambi<-summaryBy(Temp_C + RH_perc + VPD_kPa ~ Date, FUN = mean, na.rm = T, ambi)
warm$Treatment<-"Warmed"
ambi$Treatment<-"Ambient"

temp<-rbind(warm, ambi)
rm(warm, ambi)

source<-merge(temp, source, by = c("Date","Treatment"))
rm(temp, df)

start<-as.Date("2020-03-28")
end<-as.Date("2020-05-05")

# start ####
source<-subset(source, R_calc == "R.area")
source<-subset(source, q15 < 15)
par(mfrow = c(4,5), omi = c(1,1,0.1,0.1), mar = c(1.5,0,1.5,0))

# ag fl ####

df<-subset(source, Species == "ag" & Source == "fl")

df$x<-df$q15
plot(1~0, pch = NA, xlim = c(0,7), ylim = c(0,10), axes=F)
axis(1, at = 3.5, labels = F)
axis(2, at = seq(0,10,2), cex.axis = 1.2, las = 2); legend("topleft", bty = "n", cex = 1.2, adj = 1, "a)")
dat<-subset(df, trt == "a"); x1<-1; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "dodgerblue4"); ablineclip(h = md, x1 = x1, x2 = x2)
dat<-subset(df, trt == "w"); x1<-4; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "firebrick"); ablineclip(h = md, x1 = x1, x2 = x2)
box()
legend("topright", bty = "n", horiz = F, col = c("dodgerblue4","firebrick"),
       c("Ambient","Warmed"), pch = 15)

df$x<-df$q20
plot(1~0, pch = NA, xlim = c(0,7), ylim = c(0,10), axes=F)
axis(1, at = 3.5, labels = F)
dat<-subset(df, trt == "a"); x1<-1; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "dodgerblue4"); ablineclip(h = md, x1 = x1, x2 = x2)
dat<-subset(df, trt == "w"); x1<-4; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "firebrick"); ablineclip(h = md, x1 = x1, x2 = x2)
box()

df$x<-df$q27
plot(1~0, pch = NA, xlim = c(0,7), ylim = c(0,10), axes=F)
axis(1, at = 3.5, labels = F)
dat<-subset(df, trt == "a"); x1<-1; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "dodgerblue4"); ablineclip(h = md, x1 = x1, x2 = x2)
dat<-subset(df, trt == "w"); x1<-4; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "firebrick"); ablineclip(h = md, x1 = x1, x2 = x2)
box()

df$x<-df$q35
plot(1~0, pch = NA, xlim = c(0,7), ylim = c(0,10), axes=F)
axis(1, at = 3.5, labels = F)
dat<-subset(df, trt == "a"); x1<-1; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "dodgerblue4"); ablineclip(h = md, x1 = x1, x2 = x2)
dat<-subset(df, trt == "w"); x1<-4; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "firebrick"); ablineclip(h = md, x1 = x1, x2 = x2)
box()

df$x<-df$q40
plot(1~0, pch = NA, xlim = c(0,7), ylim = c(0,10), axes=F)
axis(1, at = 3.5, labels = F)
dat<-subset(df, trt == "a"); x1<-1; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "dodgerblue4"); ablineclip(h = md, x1 = x1, x2 = x2)
dat<-subset(df, trt == "w"); x1<-4; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "firebrick"); ablineclip(h = md, x1 = x1, x2 = x2)
box()

# ag bz ####

df<-subset(source, Species == "ag" & Source == "bz")

df$x<-df$q15
plot(1~0, pch = NA, xlim = c(0,7), ylim = c(0,10), axes=F)
axis(1, at = 3.5, labels = F)
axis(2, at = seq(0,10,2), cex.axis = 1.2, las = 2); legend("topleft", bty = "n", cex = 1.2, adj = 1, "b)")
dat<-subset(df, trt == "a"); x1<-1; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "dodgerblue4"); ablineclip(h = md, x1 = x1, x2 = x2)
dat<-subset(df, trt == "w"); x1<-4; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "firebrick"); ablineclip(h = md, x1 = x1, x2 = x2)
box()

df$x<-df$q20
plot(1~0, pch = NA, xlim = c(0,7), ylim = c(0,10), axes=F)
axis(1, at = 3.5, labels = F)
dat<-subset(df, trt == "a"); x1<-1; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "dodgerblue4"); ablineclip(h = md, x1 = x1, x2 = x2)
dat<-subset(df, trt == "w"); x1<-4; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "firebrick"); ablineclip(h = md, x1 = x1, x2 = x2)
box()

df$x<-df$q27
plot(1~0, pch = NA, xlim = c(0,7), ylim = c(0,10), axes=F)
axis(1, at = 3.5, labels = F)
dat<-subset(df, trt == "a"); x1<-1; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "dodgerblue4"); ablineclip(h = md, x1 = x1, x2 = x2)
dat<-subset(df, trt == "w"); x1<-4; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "firebrick"); ablineclip(h = md, x1 = x1, x2 = x2)
box()

df$x<-df$q35
plot(1~0, pch = NA, xlim = c(0,7), ylim = c(0,10), axes=F)
axis(1, at = 3.5, labels = F)
dat<-subset(df, trt == "a"); x1<-1; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "dodgerblue4"); ablineclip(h = md, x1 = x1, x2 = x2)
dat<-subset(df, trt == "w"); x1<-4; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "firebrick"); ablineclip(h = md, x1 = x1, x2 = x2)
box()

df$x<-df$q40
plot(1~0, pch = NA, xlim = c(0,7), ylim = c(0,10), axes=F)
axis(1, at = 3.5, labels = F)
dat<-subset(df, trt == "a"); x1<-1; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "dodgerblue4"); ablineclip(h = md, x1 = x1, x2 = x2)
dat<-subset(df, trt == "w"); x1<-4; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "firebrick"); ablineclip(h = md, x1 = x1, x2 = x2)
box()
# rm fl ####

df<-subset(source, Species == "rm" & Source == "fl")

df$x<-df$q15
plot(1~0, pch = NA, xlim = c(0,7), ylim = c(0,10), axes=F)
axis(1, at = 3.5, labels = F)
axis(2, at = seq(0,10,2), cex.axis = 1.2, las = 2); legend("topleft", bty = "n", cex = 1.2, adj = 1, "c)")
dat<-subset(df, trt == "a"); x1<-1; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "dodgerblue4"); ablineclip(h = md, x1 = x1, x2 = x2)
dat<-subset(df, trt == "w"); x1<-4; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "firebrick"); ablineclip(h = md, x1 = x1, x2 = x2)
box()

df$x<-df$q20
plot(1~0, pch = NA, xlim = c(0,7), ylim = c(0,10), axes=F)
axis(1, at = 3.5, labels = F)
dat<-subset(df, trt == "a"); x1<-1; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "dodgerblue4"); ablineclip(h = md, x1 = x1, x2 = x2)
dat<-subset(df, trt == "w"); x1<-4; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "firebrick"); ablineclip(h = md, x1 = x1, x2 = x2)
box()

df$x<-df$q27
plot(1~0, pch = NA, xlim = c(0,7), ylim = c(0,10), axes=F)
axis(1, at = 3.5, labels = F)
dat<-subset(df, trt == "a"); x1<-1; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "dodgerblue4"); ablineclip(h = md, x1 = x1, x2 = x2)
dat<-subset(df, trt == "w"); x1<-4; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "firebrick"); ablineclip(h = md, x1 = x1, x2 = x2)
box()

df$x<-df$q35
plot(1~0, pch = NA, xlim = c(0,7), ylim = c(0,10), axes=F)
axis(1, at = 3.5, labels = F)
dat<-subset(df, trt == "a"); x1<-1; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "dodgerblue4"); ablineclip(h = md, x1 = x1, x2 = x2)
dat<-subset(df, trt == "w"); x1<-4; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "firebrick"); ablineclip(h = md, x1 = x1, x2 = x2)
box()

df$x<-df$q40
plot(1~0, pch = NA, xlim = c(0,7), ylim = c(0,10), axes=F)
axis(1, at = 3.5, labels = F)
dat<-subset(df, trt == "a"); x1<-1; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "dodgerblue4"); ablineclip(h = md, x1 = x1, x2 = x2)
dat<-subset(df, trt == "w"); x1<-4; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "firebrick"); ablineclip(h = md, x1 = x1, x2 = x2)
box()

# rm bz ####

df<-subset(source, Species == "rm" & Source == "bz")

df$x<-df$q15
plot(1~0, pch = NA, xlim = c(0,7), ylim = c(0,10), axes=F)
axis(1, at = 3.5, labels = "15", cex.axis = 1.3)
axis(2, at = seq(0,10,2), cex.axis = 1.2, las = 2); legend("topleft", bty = "n", cex = 1.2, adj = 1, "d)")
dat<-subset(df, trt == "a"); x1<-1; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "dodgerblue4"); ablineclip(h = md, x1 = x1, x2 = x2)
dat<-subset(df, trt == "w"); x1<-4; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "firebrick"); ablineclip(h = md, x1 = x1, x2 = x2)
box()

df$x<-df$q20
plot(1~0, pch = NA, xlim = c(0,7), ylim = c(0,10), axes=F)
axis(1, at = 3.5, labels = "20", cex.axis = 1.3)
dat<-subset(df, trt == "a"); x1<-1; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "dodgerblue4"); ablineclip(h = md, x1 = x1, x2 = x2)
dat<-subset(df, trt == "w"); x1<-4; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "firebrick"); ablineclip(h = md, x1 = x1, x2 = x2)
box()

df$x<-df$q27
plot(1~0, pch = NA, xlim = c(0,7), ylim = c(0,10), axes=F)
axis(1, at = 3.5, labels = "27", cex.axis = 1.3)
dat<-subset(df, trt == "a"); x1<-1; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "dodgerblue4"); ablineclip(h = md, x1 = x1, x2 = x2)
dat<-subset(df, trt == "w"); x1<-4; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "firebrick"); ablineclip(h = md, x1 = x1, x2 = x2)
box()

df$x<-df$q35
plot(1~0, pch = NA, xlim = c(0,7), ylim = c(0,10), axes=F)
axis(1, at = 3.5, labels = "35", cex.axis = 1.3)
dat<-subset(df, trt == "a"); x1<-1; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "dodgerblue4"); ablineclip(h = md, x1 = x1, x2 = x2)
dat<-subset(df, trt == "w"); x1<-4; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "firebrick"); ablineclip(h = md, x1 = x1, x2 = x2)
box()

df$x<-df$q40
plot(1~0, pch = NA, xlim = c(0,7), ylim = c(0,10), axes=F)
axis(1, at = 3.5, labels = "40", cex.axis = 1.3)
dat<-subset(df, trt == "a"); x1<-1; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "dodgerblue4"); ablineclip(h = md, x1 = x1, x2 = x2)
dat<-subset(df, trt == "w"); x1<-4; x2<-x1+2
md<-median(dat$x, na.rm = T); lq<-as.numeric(quantile(dat$x, na.rm = T)[2]); up<-as.numeric(quantile(dat$x, na.rm = T)[4]); mn<-min(dat$x, na.rm = T); mx<-max(dat$x, na.rm = T)
ablineclip(v = (x1+x2)/2, y1 = mn, y2 = mx)
rect(x1, lq, x2, up, col = "firebrick"); ablineclip(h = md, x1 = x1, x2 = x2)
box()
