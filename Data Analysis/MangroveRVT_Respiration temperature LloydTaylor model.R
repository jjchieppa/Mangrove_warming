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

### Llloyd and Taylor model loop for area based respiration

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
  mod<-nlsLM(R.area ~ rdref * exp( (Ea/0.008314472) *(alpha-beta)), start = list(rdref = 1, Ea = 30), dat)
  mod.sum<-summary(mod)
  rdref<-mod.sum$parameters[1,1] # isolate the rdref estimate
  Ea<-mod.sum$parameters[2,1] # isolate the Ea estimate
  temp.df<-data.frame(rdref, Ea)

  temp.df$UserIDs_in<-unique(dat$UserIDs_in)
  temp.df$LMA_kg.m2<-unique(dat$LMA_kg.m2)
  temp.df$Chl<-unique(dat$Chl)
  temp.df$Date<-unique(dat$Date)
  temp.df$MoYr<-unique(dat$MoYr)

  newdf<-rbind(temp.df, newdf)

  res<-residuals(mod)
  res.df<-data.frame(res)
  res.df$temp<-dat$CTleaf
  res.df$Date<-dat$Date
  res.df<-data.frame(res.df)
  df4res<-rbind(res.df, df4res)

  pred<-fitted(mod)
  pred.df<-data.frame(pred)
  pred.df$obs<-dat$R.area
  pred.df$Temp<-dat$CTleaf
  df4pred<-rbind(df4pred, pred.df)

  setwd(hist)
  pdf(paste("ALL_MangroveRVT_LaTmod_Rarea25_hist.pdf"))
  hist(df4res$res) # Prints histogram for all data
  dev.off()

  setwd(hist)
  pdf(paste("ALL_MangroveRVT_LaTmod_Rarea25_pred v res.pdf"))
  plot(df4res$res ~ df4pred$pred) # Prints histogram for all data
  dev.off()

  setwd(lins)
  pdf(paste("ALL_MangroveRVT_LaTmod_Rarea25_obs v pred.pdf", sep = ""))
  plot(df4pred$pred ~ df4pred$obs); abline(0, 1, lty = 2, col = "firebrick")
  sum<-summary(mod<-lm(df4pred$pred ~ df4pred$obs)); r2<-sum$r.squared
  legend("topleft",paste(r2))
  dev.off()
  
  setwd(lins)
  pdf(paste("ALL_MangroveRVT_LaTmod_Rarea25_pred v temp.pdf", sep = ""))
  plot(df4pred$pred ~ df4pred$Temp)
  sum<-summary(mod<-lm(df4pred$pred ~ df4pred$Temp)); r2<-sum$r.squared
  legend("topleft",paste(r2))
  dev.off()
  
}

LaT_Rarea_25<-newdf
LaT_Rarea_25$RefTemp<-as.numeric(25)
LaT_Rarea_25$R_calc<-"R.area"
rm(dat, dat.pred, df4pred, df4res, newdf, pred.df, res.df,
   temp.df, mod, mod.sum, pred, Ea, R, r2, raw, rdref,
   reps, res, sum)

##########################################################
##########################################################

### Llloyd and Taylor model loop for mass based respiration

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
  mod<-nlsLM(R.mass ~ rdref * exp( (Ea/0.008314472) *(alpha-beta)), start = list(rdref = 1, Ea = 30), dat)
  mod.sum<-summary(mod)
  rdref<-mod.sum$parameters[1,1] # isolate the rdref estimate
  Ea<-mod.sum$parameters[2,1] # isolate the Ea estimate
  temp.df<-data.frame(rdref, Ea)
  
  temp.df$UserIDs_in<-unique(dat$UserIDs_in)
  temp.df$LMA_kg.m2<-unique(dat$LMA_kg.m2)
  temp.df$Chl<-unique(dat$Chl)
  temp.df$Date<-unique(dat$Date)
  temp.df$MoYr<-unique(dat$MoYr)
  
  newdf<-rbind(temp.df, newdf)
  
  res<-residuals(mod)
  res.df<-data.frame(res)
  res.df$temp<-dat$CTleaf
  res.df$Date<-dat$Date
  res.df<-data.frame(res.df)
  df4res<-rbind(res.df, df4res)
  
  pred<-fitted(mod)
  pred.df<-data.frame(pred)
  pred.df$obs<-dat$R.mass
  pred.df$Temp<-dat$CTleaf
  df4pred<-rbind(df4pred, pred.df)
  
  setwd(hist)
  pdf(paste("ALL_MangroveRVT_LaTmod_Rmass25_hist.pdf"))
  hist(df4res$res) # Prints histogram for all data
  dev.off()
  
  setwd(hist)
  pdf(paste("ALL_MangroveRVT_LaTmod_Rmass25_pred v res.pdf"))
  plot(df4res$res ~ df4pred$pred) # Prints histogram for all data
  dev.off()
  
  setwd(lins)
  pdf(paste("ALL_MangroveRVT_LaTmod_Rmass25_obs v pred.pdf", sep = ""))
  plot(df4pred$pred ~ df4pred$obs); abline(0, 1, lty = 2, col = "firebrick")
  sum<-summary(mod<-lm(df4pred$pred ~ df4pred$obs)); r2<-sum$r.squared
  legend("topleft",paste(r2))
  dev.off()

  setwd(lins)
  pdf(paste("ALL_MangroveRVT_LaTmod_Rmass25_pred v temp.pdf", sep = ""))
  plot(df4pred$pred ~ df4pred$Temp)
  sum<-summary(mod<-lm(df4pred$pred ~ df4pred$Temp)); r2<-sum$r.squared
  legend("topleft",paste(r2))
  dev.off()
  
}

LaT_Rmass_25<-newdf
LaT_Rmass_25$RefTemp<-as.numeric(25)
LaT_Rmass_25$R_calc<-"R.mass"
rm(dat, dat.pred, df4pred, df4res, newdf, pred.df, res.df,
   temp.df, mod, mod.sum, pred, Ea, r2, rdref,
   reps, res, sum, K)

##########################################################
##########################################################

# Bind 15, 20, and 25 reference Rs for area and mass together output

output<-rbind(LaT_Rarea_25, LaT_Rmass_25)
output$R_calc<-as.factor(output$R_calc)

##########################################################
##########################################################

setwd(deets)
write.csv(output, "MangroveRVT_LaTmod_Rarea and Rmass at 25C.csv", row.names = F)
rm(output)