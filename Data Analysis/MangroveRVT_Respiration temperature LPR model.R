library(lattice);library(doBy); library(lubridate)
library(minpack.lm)

rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

#########################################################
#########################################################
work<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis" # work
raw<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/raw"
hist<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/hist"
lins<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/lins"
deets<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/deets"

setwd(work)


df<-read.csv("RvT respiration data_cleaned.csv")
df<-subset(df, KEEP == "Y")
df$Date<-as.Date(df$Date, format = "%m/%d/%Y")
traits<-read.csv("Mangrove leaf data.csv")
traits$Date<-as.Date(traits$Date, format = "%m/%d/%Y")
df<-merge(traits, df, by = c("Date","UserIDs_in"))
rm(traits)

df$MoYr<-format(as.Date(df$Date), "%Y-%m")
df$R.area<-df$Photo_out*-1
df$R.area_log<-log(df$R.area)
df$CTleaf2<-df$CTleaf_out^2
df$R.mass<-(df$R.area)/df$LMA_kg.m2
df$R.mass_log<-log(df$R.mass)

R<-0.008314472
K<-273.15
df$Tk<-df$CTleaf_out+K
df$Tk_inv<-1/df$Tk

df<-summaryBy(R.area + R.area_log + R.mass + R.mass_log + CTleaf_out + CTleaf2
              + LMA_kg.m2
              ~ UserIDs_in + Date + Moyr + MyTemp, FUN = mean, na.rm = T, df)

df<-df[!is.na(df$Date),]
names(df)[4]<-"R.area"
names(df)[5]<-"R.area_log"
names(df)[6]<-"R.mass"
names(df)[7]<-"R.mass_log"
names(df)[8]<-"CTleaf"
names(df)[9]<-"CTleaf2"
names(df)[10]<-"LMA_kg.m2"

df$alpha<-df$CTleaf+K
df$var<-interaction(df$UserIDs_in, as.factor(df$Date))

###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################


##########################################################
##########################################################

### log polynomial regression model loop for area based respiration

df$alpha<-1/(K+25) # Reference Temperature
df$Tk<-df$CTleaf+K
df$beta<-1/df$Tk # Reference Temperature

reps<-as.character(unique(df$var))
newdf<-subset(df, UserIDs_in == "") # Just make a shell of a dataframe
df4res<-data.frame(res=as.numeric(),
                   CTleaf=as.numeric(),
                   Date=as.numeric())
dat.pred<-data.frame(predicted=as.numeric(),
                     observed=as.numeric())
df4pred<-data.frame(pred=as.numeric(),
                    obs=as.numeric(),
                    Temp=as.numeric())

for (reps in unique(df$var)){
  dat<-subset(df, df$var == reps)
  mod<-lm(log(R.area) ~ CTleaf + CTleaf2, dat)
  mod.sum<-summary(mod)
  
  intercept<-mod.sum$coefficients[1,1]
  CTleaf.slope<-mod.sum$coefficients[2,1]
  CTleaf2.slope<-mod.sum$coefficients[3,1]
  
  temp.df<-data.frame(intercept, CTleaf.slope, CTleaf2.slope)
  temp.df$UserIDs_in<-unique(dat$UserIDs_in)
  temp.df$Moyr<-unique(dat$Moyr)
  temp.df$Date<-unique(dat$Date)
  temp.df$LMA_kg.m2<-unique(dat$LMA_kg.m2)
  
  
  temp.df$rdref<-exp(temp.df$intercept+(temp.df$CTleaf.slope*25)+(temp.df$CTleaf2.slope*25**2))
  temp.df$q10<-exp(10*(temp.df$CTleaf.slope+(2*temp.df$CTleaf2.slope*25)))
  
  temp.df$r15<-exp(temp.df$intercept+(temp.df$CTleaf.slope*15)+(temp.df$CTleaf2.slope*15**2))
  temp.df$r20<-exp(temp.df$intercept+(temp.df$CTleaf.slope*20)+(temp.df$CTleaf2.slope*20**2))
  temp.df$r27<-exp(temp.df$intercept+(temp.df$CTleaf.slope*27)+(temp.df$CTleaf2.slope*27**2))
  temp.df$r35<-exp(temp.df$intercept+(temp.df$CTleaf.slope*35)+(temp.df$CTleaf2.slope*35**2))
  temp.df$r40<-exp(temp.df$intercept+(temp.df$CTleaf.slope*40)+(temp.df$CTleaf2.slope*40**2))
  temp.df$q15<-exp(10*(temp.df$CTleaf.slope+(2*temp.df$CTleaf2.slope*15)))
  temp.df$q20<-exp(10*(temp.df$CTleaf.slope+(2*temp.df$CTleaf2.slope*20)))
  temp.df$q27<-exp(10*(temp.df$CTleaf.slope+(2*temp.df$CTleaf2.slope*27)))
  temp.df$q35<-exp(10*(temp.df$CTleaf.slope+(2*temp.df$CTleaf2.slope*35)))
  temp.df$q40<-exp(10*(temp.df$CTleaf.slope+(2*temp.df$CTleaf2.slope*40)))
  
  newdf<-rbind(temp.df, newdf)
  
  pred<-predict(mod)
  pred.df<-data.frame(pred)
  pred.df$obs<-log(dat$R.area)
  pred.df$Temp<-dat$CTleaf
  df4pred<-rbind(df4pred, pred.df)
  
  res<-residuals(mod)
  res.df<-data.frame(res)
  res.df$CTleaf<-dat$CTleaf
  res.df$CTleaf2<-dat$CTleaf2
  res.df$Date<-dat$Date
  res.df$UserIDs_in<-dat$UserIDs_in
  res.df<-data.frame(res.df)
  df4res<-rbind(res.df, df4res)
  
  setwd(hist)
  pdf(paste("ALL_MangroveRVT_LPRmod_Rarea25_hist.pdf"))
  hist(df4res$res) # Prints histogram for all data
  dev.off()
  
  setwd(hist)
  pdf(paste("ALL_MangroveRVT_LPRmod_Rarea25_pred v res.pdf"))
  plot(df4res$res ~ df4pred$pred) # Prints histogram for all data
  dev.off()
  
  setwd(lins)
  pdf(paste("ALL_MangroveRVT_LPRmod_Rarea25_obs~pred.pdf", sep = ""))
  plot(df4pred$pred ~ df4pred$obs); abline(0, 1, lty = 2, col = "firebrick")
  sum<-summary(mod<-lm(df4pred$pred ~ df4pred$obs)); r2<-sum$r.squared
  legend("topleft",paste(r2))
  dev.off()
  
  setwd(lins)
  pdf(paste("ALL_MangroveRVT_LPRmod_Rarea25_pred v temp.pdf", sep = ""))
  plot(df4pred$pred ~ df4pred$Temp)
  sum<-summary(mod<-lm(df4pred$pred ~ df4pred$Temp)); r2<-sum$r.squared
  legend("topleft",paste(r2))
  dev.off()
  
}

LPR_Rarea_25<-newdf
LPR_Rarea_25$MyTemp<-as.numeric(25)
LPR_Rarea_25$R_calc<-"R.area"
rm(newdf, dat, dat.pred, df4pred, df4res, mod, mod.sum, pred.df, res.df, temp.df)
rm(CTleaf.slope, CTleaf2.slope, intercept, reps, res, pred, sum, r2)

##########################################################

### Log polynomial model loop for mass based respiration

### at 25 degrees (Change in calculation within loop)

reps<-as.character(unique(df$var))
newdf<-data.frame(intercept=as.numeric(),
                  CTleaf.slope=as.numeric(),
                  CTleaf2.slope=as.numeric(),
                  UserIDs_in=as.character(),
                  Moyr=as.character(),
                  Date=numeric(),
                  rdref=as.numeric(),
                  q10=as.numeric())
df4res<-data.frame(res=as.numeric(),
                   CTleaf=as.numeric(),
                   CTleaf2=as.numeric(),
                   UserIDs_in=as.character(),
                   Date=as.character())
dat.pred<-data.frame(pred=as.numeric(),
                     obs=as.numeric())
df4pred<-data.frame(pred=as.numeric(),
                    obs=as.numeric(),
                    Temp=as.numeric())

for (reps in unique(df$var)){
  dat<-subset(df, df$var == reps)
  mod<-lm(log(R.mass) ~ CTleaf + CTleaf2, dat)
  mod.sum<-summary(mod)
  
  intercept<-mod.sum$coefficients[1,1]
  CTleaf.slope<-mod.sum$coefficients[2,1]
  CTleaf2.slope<-mod.sum$coefficients[3,1]
  
  temp.df<-data.frame(intercept, CTleaf.slope, CTleaf2.slope)
  temp.df$UserIDs_in<-unique(dat$UserIDs_in)
  temp.df$Moyr<-unique(dat$Moyr)
  temp.df$Date<-unique(dat$Date)
  temp.df$LMA_kg.m2<-unique(dat$LMA_kg.m2)
  
  temp.df$rdref<-exp(temp.df$intercept+(temp.df$CTleaf.slope*25)+(temp.df$CTleaf2.slope*25**2))
  temp.df$q10<-exp(10*(temp.df$CTleaf.slope+(2*temp.df$CTleaf2.slope*25)))
  
  temp.df$r15<-exp(temp.df$intercept+(temp.df$CTleaf.slope*15)+(temp.df$CTleaf2.slope*15**2))
  temp.df$r20<-exp(temp.df$intercept+(temp.df$CTleaf.slope*20)+(temp.df$CTleaf2.slope*20**2))
  temp.df$r27<-exp(temp.df$intercept+(temp.df$CTleaf.slope*27)+(temp.df$CTleaf2.slope*27**2))
  temp.df$r35<-exp(temp.df$intercept+(temp.df$CTleaf.slope*35)+(temp.df$CTleaf2.slope*35**2))
  temp.df$r40<-exp(temp.df$intercept+(temp.df$CTleaf.slope*40)+(temp.df$CTleaf2.slope*40**2))
  temp.df$q15<-exp(10*(temp.df$CTleaf.slope+(2*temp.df$CTleaf2.slope*15)))
  temp.df$q20<-exp(10*(temp.df$CTleaf.slope+(2*temp.df$CTleaf2.slope*20)))
  temp.df$q27<-exp(10*(temp.df$CTleaf.slope+(2*temp.df$CTleaf2.slope*27)))
  temp.df$q35<-exp(10*(temp.df$CTleaf.slope+(2*temp.df$CTleaf2.slope*35)))
  temp.df$q40<-exp(10*(temp.df$CTleaf.slope+(2*temp.df$CTleaf2.slope*40)))
  
  newdf<-rbind(temp.df, newdf)
  
  pred<-predict(mod)
  pred.df<-data.frame(pred)
  pred.df$obs<-log(dat$R.mass)
  pred.df$Temp<-dat$CTleaf
  df4pred<-rbind(df4pred, pred.df)
  
  res<-residuals(mod)
  res.df<-data.frame(res)
  res.df$CTleaf<-dat$CTleaf
  res.df$CTleaf2<-dat$CTleaf2
  res.df$Date<-dat$Date
  res.df$UserIDs_in<-dat$UserIDs_in
  res.df<-data.frame(res.df)
  df4res<-rbind(res.df, df4res)
  
  setwd(hist)
  pdf(paste("ALL_MangroveRVT_LPRmod_Rmass25_hist.pdf"))
  hist(df4res$res) # Prints histogram for all data
  dev.off()
  
  setwd(hist)
  pdf(paste("ALL_MangroveRVT_LPRmod_Rmass25_pred v res.pdf"))
  plot(df4res$res ~ df4pred$pred) # Prints histogram for all data
  dev.off()
  
  setwd(lins)
  pdf(paste("ALL_MangroveRVT_LPRmod_Rmass25_obs~pred.pdf", sep = ""))
  plot(df4pred$pred ~ df4pred$obs); abline(0, 1, lty = 2, col = "firebrick")
  sum<-summary(mod<-lm(df4pred$pred ~ df4pred$obs)); r2<-sum$r.squared
  legend("topleft",paste(r2))
  dev.off()
  
  setwd(lins)
  pdf(paste("ALL_MangroveRVT_LPRmod_Rmass25_pred v temp.pdf", sep = ""))
  plot(df4pred$pred ~ df4pred$Temp)
  sum<-summary(mod<-lm(df4pred$pred ~ df4pred$Temp)); r2<-sum$r.squared
  legend("topleft",paste(r2))
  dev.off()
  
}

LPR_Rmass_25<-newdf
LPR_Rmass_25$MyTemp<-as.numeric(25)
LPR_Rmass_25$R_calc<-"R.mass"
rm(newdf, dat, dat.pred, df4pred, df4res, mod, mod.sum, pred.df, res.df, temp.df)
rm(CTleaf.slope, CTleaf2.slope, intercept, reps, res, pred, sum, r2)

##########################################################
##########################################################

##########################################################
##########################################################

rm(K, R)
# Bind 15, 20, and 25 reference Rs for area and mass together output

output<-rbind(LPR_Rarea_25, LPR_Rmass_25)
output$R_calc<-as.factor(output$R_calc)

##########################################################
##########################################################

setwd(deets)
write.csv(output, "MangroveRVT_LPRmod_Rarea and Rmass at 25C.csv", row.names = F)
rm(output)
