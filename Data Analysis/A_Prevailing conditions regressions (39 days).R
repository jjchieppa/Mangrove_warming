# start ####

library(lattice);library(doBy); library(lubridate)
library(minpack.lm); library(effects); library(plotrix)
library(car); library(sjPlot); library(suncalc)
library(chron); library(reshape)

rm(list=ls())
dev.off()
cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

work<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp" # work
deets<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/deets"
envi<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Weather Data"

# in data ####

setwd(envi)
env<-read.csv("Prevailing env (39 days).csv")
env$Date<-as.Date(env$Date, format = "%Y-%m-%d")

setwd(deets)
source<-read.csv("MangroveRvT_LPRmod_Rarea and Rmass at 25C.csv")
source<-subset(source, KEEP == "Y")
source$Date<-as.Date(source$Date, format = "%m/%d/%Y")
setwd(work)
trts<-read.csv("Mangrove Treatment Assignments.csv")
source<-merge(source, trts, by = "UserIDs_in", all = F)
rm(trts)
source$Treatment<-as.factor(substr(source$Treatment, 1, 4))

source$Date<-as.Date(source$Date, format = "%Y-%m-%d") # Double checks date format!
source$MoYr<-floor_date(source$Date, "month")
source$MoYr<-source$MoYr+25
source$LMA_g.m2<-source$LMA_kg.m2*1000
source$Timepoint<-as.factor(source$MoYr)

df<-merge(source, env, by = c("Date","Treatment"))
rm(env, source)

# models 
# ag Rarea ####

dat<-subset(df, R_calc == "R.area" & Species == "ag")

ag.ra_out<-data.frame(r2=as.numeric(),
                      pred=as.numeric(),
                      slope=as.numeric(),
                      intercept=as.numeric(),
                      slope.p=as.numeric(),
                      intercept.p=as.numeric())

for (i in c(32:499)) {
  dum<-dat[,c(8,i)]
  m<-lm(dum[,1] ~ dum[,2])
  ms<-summary(m)
  r2<-ms$r.squared
  pred<-colnames(dum)[2]
  slope<-ms$coefficients[1,1]
  intercept<-ms$coefficients[2,1]
  slope.p<-ms$coefficients[1,4]
  intercept.p<-ms$coefficients[2,4]
  
  temp.df<-data.frame(r2,pred,slope,intercept,slope.p,intercept.p)
  ag.ra_out<-rbind(ag.ra_out, temp.df)
}

ag.ra_out$group<-"ag rarea"

# ag Rmass ####

dat<-subset(df, R_calc == "R.mass" & Species == "ag")

ag.rm_out<-data.frame(r2=as.numeric(),
                      pred=as.numeric(),
                      slope=as.numeric(),
                      intercept=as.numeric(),
                      slope.p=as.numeric(),
                      intercept.p=as.numeric())

for (i in c(32:499)) {
  dum<-dat[,c(8,i)]
  m<-lm(dum[,1] ~ dum[,2])
  ms<-summary(m)
  r2<-ms$r.squared
  pred<-colnames(dum)[2]
  slope<-ms$coefficients[1,1]
  intercept<-ms$coefficients[2,1]
  slope.p<-ms$coefficients[1,4]
  intercept.p<-ms$coefficients[2,4]

  temp.df<-data.frame(r2,pred,slope,intercept,slope.p,intercept.p)
  ag.rm_out<-rbind(ag.rm_out, temp.df)
}

ag.rm_out$group<-"ag rmass"

# rm Rarea ####

dat<-subset(df, R_calc == "R.area" & Species == "rm")

rm.ra_out<-data.frame(r2=as.numeric(),
                      pred=as.numeric(),
                      slope=as.numeric(),
                      intercept=as.numeric(),
                      slope.p=as.numeric(),
                      intercept.p=as.numeric())

for (i in c(32:499)) {
  dum<-dat[,c(8,i)]
  m<-lm(dum[,1] ~ dum[,2])
  ms<-summary(m)
  r2<-ms$r.squared
  pred<-colnames(dum)[2]
  slope<-ms$coefficients[1,1]
  intercept<-ms$coefficients[2,1]
  slope.p<-ms$coefficients[1,4]
  intercept.p<-ms$coefficients[2,4]

  temp.df<-data.frame(r2,pred,slope,intercept,slope.p,intercept.p)
  rm.ra_out<-rbind(rm.ra_out, temp.df)
}

rm.ra_out$group<-"rm rarea"

# rm Rmass ####

dat<-subset(df, R_calc == "R.mass" & Species == "rm")

rm.rm_out<-data.frame(r2=as.numeric(),
                      pred=as.numeric(),
                      slope=as.numeric(),
                      intercept=as.numeric(),
                      slope.p=as.numeric(),
                      intercept.p=as.numeric())

for (i in c(32:499)) {
  dum<-dat[,c(8,i)]
  m<-lm(dum[,1] ~ dum[,2])
  ms<-summary(m)
  r2<-ms$r.squared
  pred<-colnames(dum)[2]
  slope<-ms$coefficients[1,1]
  intercept<-ms$coefficients[2,1]
  slope.p<-ms$coefficients[1,4]
  intercept.p<-ms$coefficients[2,4]

  temp.df<-data.frame(r2,pred,slope,intercept,slope.p,intercept.p)
  rm.rm_out<-rbind(rm.rm_out, temp.df)
}

rm.rm_out$group<-"rm rmass"

out<-rbind(ag.ra_out, ag.rm_out, rm.ra_out, rm.rm_out)

write.csv(out, "Predictions from prevailing conditions (39 days).csv", row.names = F)
