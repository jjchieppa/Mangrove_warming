library(dplyr); library(readxl); library(lubridate)
library(doBy)
rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
############################################## 

### Loop for weather csv's and excel files


############################################################################################
############################################################################################

# Start with weather folders

############################################################################################
############################################################################################

setwd("C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Weather Data/Raw")

##############################################

temp = list.files(pattern="Warm")
for (i in 1:length(temp)) {
        assign(temp[i], read.csv(temp[i], header = F))
        
}
rm(temp, i)
dfs<-Filter(function(x) is(x, "data.frame"), mget(ls()))
dfs<-Map(cbind, filenames = names(dfs), dfs)

newdataset<-dfs[[1]] # Make a shell df to rbind data to
details<-newdataset[1,1]
obs.row<-which(newdataset[,2] == "#")
colnames(newdataset)<-NULL
newdataset<-newdataset[-c(obs.row-1),]
colnames(newdataset)<-as.character(unlist(newdataset[1,]))
newdataset<-newdataset[-1,]
names(newdataset)[1]<-"FileName"
names(newdataset)[2]<-"ObsNo"
names(newdataset)[3]<-"TimeStamp"
names(newdataset)[4]<-"Temp_C"
names(newdataset)[5]<-"RH_perc"

Warm<-newdataset[0,]; rm(newdataset)

for (i in 1:length(dfs)) {
        dat<-dfs[[i]]
        obs.row<-which(dat[,2] == "#")
        colnames(dat)<-NULL
        dat<-dat[-c(obs.row-1),]
        obs.row<-which(dat[,3] == "Temp, (*C)")
        colnames(dat)<-as.character(unlist(dat[1,]))
        dat<-dat[-1,]
        names(dat)[1]<-"FileName"
        names(dat)[2]<-"ObsNo"
        names(dat)[3]<-"TimeStamp"
        names(dat)[4]<-"Temp_C"
        names(dat)[5]<-"RH_perc"
        Warm<-bind_rows(Warm, dat)
}

setwd("C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Weather Data")

write.csv(Warm, "Mangrove Belize Florida_Warmed_joined.csv", row.names = F)

setwd("C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Weather Data/Raw")

rm(list=ls())

##############################################

temp = list.files(pattern="Ambi")
for (i in 1:length(temp)) {
        assign(temp[i], read.csv(temp[i], header = F))
        
}
rm(temp, i)
dfs<-Filter(function(x) is(x, "data.frame"), mget(ls()))
dfs<-Map(cbind, filenames = names(dfs), dfs)

newdataset<-dfs[[1]] # Make a shell df to rbind data to
details<-newdataset[1,1]
obs.row<-which(newdataset[,2] == "#")
colnames(newdataset)<-NULL
newdataset<-newdataset[-c(obs.row-1),]
colnames(newdataset)<-as.character(unlist(newdataset[1,]))
newdataset<-newdataset[-1,]
names(newdataset)[1]<-"FileName"
names(newdataset)[2]<-"ObsNo"
names(newdataset)[3]<-"TimeStamp"
names(newdataset)[4]<-"Temp_C"
names(newdataset)[5]<-"RH_perc"

Ambi<-newdataset[0,]; rm(newdataset)

for (i in 1:length(dfs)) {
        dat<-dfs[[i]]
        obs.row<-which(dat[,2] == "#")
        colnames(dat)<-NULL
        dat<-dat[-c(obs.row-1),]
        obs.row<-which(dat[,3] == "Temp, (*C)")
        colnames(dat)<-as.character(unlist(dat[1,]))
        dat<-dat[-1,]
        names(dat)[1]<-"FileName"
        names(dat)[2]<-"ObsNo"
        names(dat)[3]<-"TimeStamp"
        names(dat)[4]<-"Temp_C"
        names(dat)[5]<-"RH_perc"
        Ambi<-bind_rows(Ambi, dat)
}

rm(list=setdiff(ls(), "Ambi"))

setwd("C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Weather Data")

write.csv(Ambi, "Mangrove Belize Florida_Ambient_joined.csv", row.names = F)

setwd("C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Weather Data/Raw")

rm(list=ls())

############################################## 

setwd("C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Weather Data")

temp = list.files(pattern="joined")
for (i in 1:length(temp)) {
        assign(temp[i], read.csv(temp[i], header = T))

}
rm(temp, i)

df<-rbind(`Mangrove Belize Florida_Warmed_joined.csv`, `Mangrove Belize Florida_Ambient_joined.csv`)
rm(list=setdiff(ls(), "df"))
df$Date<-as.Date(df$TimeStamp, format = "%m/%d/%y")
df$Treatment<-substr(df$FileName, 5, 8); df$Treatment<-as.factor(df$Treatment)
df$Block<-substr(df$FileName, 15,15); df$Block<-as.factor(df$Block)

# df<-summaryBy(Temp_C + RH_perc ~ Block * Treatment * Date, FUN = c(min,mean,max), na.rm = T, df)

setwd("C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Weather Data")

write.csv(df, "Mangrove RVT weather data_all.csv", row.names = F)