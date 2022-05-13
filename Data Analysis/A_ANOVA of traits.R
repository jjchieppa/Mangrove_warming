# in data ####

library(lattice);library(doBy); library(lubridate)
library(minpack.lm); library(effects); library(plotrix)
library(emmeans); library(multcomp)

rm(list=ls())
dev.off()
cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# morphology ####

# setwd("C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis")
# 
# df<-read.csv("Mangrove morphology data_error2.csv")
# df<-subset(df, Dead != "Y")
# df$Date<-as.factor(df$Date)
# 
# dat<-subset(df, Species == "RM")
# m<-lm(dat$leaf.no ~ Date * Treatment * Source, dat)
# Anova(m)
# 
# em1<-cld(emmeans(m, ~ Source:Treatment))
# # em1$Date<-as.Date(em1$Date, format = "%m/%d/%Y")
# 
# summaryBy(leaf.no ~ Source + Treatment, FUN = c(mean, std.error), na.rm = T, dat)
# 
# mean(dat$TLA_cm2, na.rm = T); std.error(dat$TLA_cm2, na.rm = T)
# mean(dat$LAR, na.rm = T); std.error(dat$LAR, na.rm = T)
# mean(dat$SLA, na.rm = T); std.error(dat$SLA, na.rm = T)

# physiology ####

setwd("C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/deets")
source<-read.csv("MangroveRvT_LPRmod_Rarea and Rmass at 25C.csv")
source$Date<-as.Date(source$Date, format = "%Y-%m-%d")
setwd("C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp")
trts<-read.csv("Mangrove Treatment Assignments.csv")
source<-merge(source, trts, by = "UserIDs_in", all = F)
rm(trts)
source$Date<-as.Date(source$Date, format = "%Y-%m-%d") # Double checks date format!
source$MoYr<-floor_date(source$Date, "month")
source$MoYr<-source$MoYr+25
source$LMA_g.m2<-source$LMA_kg.m2*100
source$Timepoint<-as.factor(source$MoYr)

df<-subset(source, Species == "ag" & R_calc == "R.area")

m1<-lm(df$intercept ~ Timepoint * Treatment * Source, df)
Anova(m1)

cld(emmeans(m1, ~Timepoint:Source))
