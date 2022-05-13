# start ####

library(lattice);library(doBy); library(lubridate)
library(minpack.lm); library(effects); library(plotrix)
library(car); library(sjPlot)

rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

home<-"C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis"
work<-"C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp" # work
raw<-"C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/raw"
hist<-"C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/hist"
lins<-"C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/lins"
deets<-"C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/deets"
env<-"C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Weather Data"
anly<-"C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/Analysis"

# Bring in the ref respiration data, bind area and mass for LMA ####

setwd(deets)
source<-read.csv("MangroveRvT_LPRmod_Rarea and Rmass at 25C.csv")
source$Date<-as.Date(source$Date, format = "%Y-%m-%d")
setwd(work)
trts<-read.csv("Mangrove Treatment Assignments.csv")
source<-merge(source, trts, by = "UserIDs_in", all = F)
# rm(trts)

source$Date<-as.Date(source$Date, format = "%Y-%m-%d") # Double checks date format!
source$LMA_g.m2<-source$LMA_kg.m2*100

setwd(deets)
fr<-read.csv("Mangrove frost damage.csv")
fr$Date<-as.Date(fr$Date, format = "%m/%d/%Y")
source<-merge(source, fr, by = c("UserIDs_in","Date"))
rm(fr)

# Bring in temperature data ####

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

# slope loop ####

reps<-unique(source$UserIDs_in)

newdf<-data.frame(UserIDs_in=as.numeric(),
                  trt=as.numeric(),
                  tair.int=as.numeric(),
                  tair.slope=as.numeric(),
                  pval=as.numeric(),
                  R_calc=as.numeric(),
                  frost=as.numeric())

source<-subset(source, UserIDs_in != "rm fl 7")

for (reps in unique(source$UserIDs_in)){
  dat<-subset(source, source$UserIDs_in == reps)
  area<-subset(dat, R_calc == "R.area")
  mod<-lm(rdref ~ Temp_C.mean, area)
  tair.int<-mod$coefficients[1]
  tair.slope<-mod$coefficients[2]
  an.mod<-Anova(mod)
  pval<-an.mod$`Pr(>F)`[1]
  UserIDs_in<-unique(area$UserIDs_in)
  trt<-unique(area$trt)
  R_calc<-unique(area$R_calc)
  frost<-unique(area$Frost)
  temp.df<-data.frame(UserIDs_in, trt, tair.int, tair.slope, pval, R_calc, frost)
  newdf<-rbind(newdf,temp.df)
  
  mass<-subset(dat, R_calc == "R.mass")
  mod<-lm(rdref ~ Temp_C.mean, mass)
  tair.int<-mod$coefficients[1]
  tair.slope<-mod$coefficients[2]
  an.mod<-Anova(mod)
  pval<-an.mod$`Pr(>F)`[1]
  UserIDs_in<-unique(mass$UserIDs_in)
  trt<-unique(mass$trt)
  R_calc<-unique(mass$R_calc)
  frost<-unique(mass$Frost)
  temp.df<-data.frame(UserIDs_in, trt, tair.int, tair.slope, pval, R_calc, frost)
  newdf<-rbind(newdf,temp.df)
}
rm(area, dat, mass, source, an.mod, temp.df)

newdf<-newdf[!is.na(newdf$fr),]
newdf$fr<-ifelse(newdf$frost == 0, 0, 1)
# newdf$fr<-as.factor(newdf$fr)
newdf<-merge(newdf, trts, by = c("UserIDs_in", "trt"))

dum<-subset(newdf, R_calc == "R.mass")
m<-lm(fr ~ tair.slope * Treatment, dum)
plot(allEffects(m))
Anova(m)

dat<-subset(newdf, Species == "rm" & Source == "fl" & Treatment == "Warmed")
summaryBy(tair.slope + tair.int ~ R_calc + fr, FUN = c(mean, std.error), na.rm = T, dat)
dat %>% count(fr, R_calc)

