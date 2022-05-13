# in data ####

library(doBy); library(lubridate); library(car)
library(effects); library(sjPlot); library(minpack.lm)

rm(list=ls())
dev.off()
cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

setwd("C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis")

df<-read.csv("Mangrove FlVBZ ins R data (no freeze).csv")
df<-subset(df, KEEP == "Y")
df$var<-interaction(df$UserIDs_in, df$R_calc)
df$T1<-df$Temp_C.mean
df$T2<-df$Temp_C.mean*df$Temp_C.mean
df$trt<-as.factor(df$trt)
df$Source<-as.factor(df$Source)

reps<-unique(df$var)

# Whole model ####

dat<-subset(df, Species == "rm" & R_calc == "R.mass")

# plot(Rins ~ Temp_C.mean, dat)
K<-273.15
dat$alpha<-dat$Temp_C.mean+K
dat$beta<-K+21

mod<-nlsLM(Rins ~ rdref * (q10) ^ ((alpha-beta)/10), start = list(rdref = 1, q10 = 2), dat)
summary(mod)
pred<-predict(mod)
obs<-dat$Rins

rmse(obs, pred)
ms<-summary(lm(obs~pred))
ms$r.squared
anova(lm(obs ~ pred))
