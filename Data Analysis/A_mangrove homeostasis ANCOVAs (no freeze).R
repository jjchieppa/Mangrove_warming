# in data ####

library(doBy); library(lubridate); library(car)
library(effects); library(sjPlot)

rm(list=ls())
dev.off()
cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

setwd("C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis")

df<-read.csv("Mangrove FlVBZ ins R data (no freeze).csv")

dat<-subset(df, Species == "rm" & R_calc == "R.mass")
dat$xx<-dat$Hom_T5_Tdiff
dat$yy<-dat$Hom_T5
dat$zz<-dat$Days_Date

m1<-lm(yy ~ 1 + xx * Source * trt, dat)
x<-plot_model(m1, type = c("pred"), terms = c("xx"))
newdata<-as.data.frame(x$data)
ln<-lm(newdata$predicted ~ newdata$x)

# par(mfrow = c(1,2), mar = c(1,2,1,1), omi = c(0.7,0.5,0.1,0.1))
plot(yy ~ xx, dat)
abline(ln); abline(h=1,col="red",lty=2)

# plot(yy ~ zz, dat)
# abline(h=1,col="red",lty=2)

Anova(m1)
summary(m1)
