library(dplyr); library(readxl); library(lubridate)
library(stringr)

##############################################
############################################## 

rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
rm(list=ls())
clean<-"C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Physiology Data/3_Files that say CLEANED go here"
setwd(clean)

############################################## 

temp = list.files(pattern="6400.x")
for (i in 1:length(temp)) {
  assign(temp[i], read.csv(temp[i], header = F))
  
}
rm(temp, i)

dfs<-Filter(function(x) is(x, "data.frame"), mget(ls()))
dfs<-Map(cbind, filenames = names(dfs), dfs)

newdataset<-dfs[[1]]
obs.row<-which(newdataset[,2] == "Obs_in")
newdataset<-newdataset[-c(obs.row-2),]
for (i in 1:ncol(newdataset)) {
  colnames(newdataset)[i]<-as.character(newdataset[1,i])
  
}
names(newdataset)[1]<-"FileName"
li6400<-newdataset[0,]; rm(newdataset)

for (i in 1:length(dfs)) {
  dat<-dfs[[i]]
    for (i in 1:ncol(dat)) {
      colnames(dat)[i]<-as.character(dat[1,i])
    }
  dat<-dat[-c(1),]
  names(dat)[1]<-"FileName"
  li6400<-rbind(li6400, dat)
  rm(dat)
}

levels(as.factor(li6400$FileName))

li6400$Date<-substr(li6400$FileName, 5, 14); li6400$Date<-as.factor(li6400$Date)
li6400$Date<-as.Date(li6400$Date, format = "%m-%d-%Y")
li6400$LicorModel<-substr(li6400$FileName, 16, 21); li6400$LicorModel<-as.factor(li6400$LicorModel)

write.csv(li6400, "RVT_li6400 machinex data.csv", row.names = F)

rm(list=setdiff(ls(), c("clean")))

############################################## 
############################################## 


##############################################
############################################## 

rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
clean<-"C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Physiology Data/3_Files that say CLEANED go here"
setwd(clean)

############################################## 

temp = list.files(pattern="*6800.1")
for (i in 1:length(temp)) {
  assign(temp[i], read.csv(temp[i], header = F))
  
}
rm(temp, i)

dfs<-Filter(function(x) is(x, "data.frame"), mget(ls()))
dfs<-Map(cbind, filenames = names(dfs), dfs)

newdataset<-dfs[[1]]
obs.row<-which(newdataset[,3] == "Sys_obs_NA")
newdataset<-newdataset[-c(obs.row-2),]
for (i in 1:ncol(newdataset)) {
  colnames(newdataset)[i]<-as.character(newdataset[1,i])
  
}
names(newdataset)[1]<-"FileName"
names(newdataset)[2]<-"Unknown"
li6801<-newdataset[0,]; rm(newdataset)



for (i in 1:length(dfs)) {
  dat<-dfs[[i]]
  obs.row<-which(dat[,3] == "Sys_obs_NA")
    for (i in 1:ncol(dat)) {
      colnames(dat)[i]<-as.character(dat[1,i])
    
    }
  dat<-dat[-c(1),]
  names(dat)[1]<-"FileName"
  names(dat)[2]<-"Unknown"
  li6801<-bind_rows(li6801, dat)
  rm(dat)
}

levels(as.factor(li6801$FileName))

li6801$Date<-substr(li6801$FileName, 5, 14); li6801$Date<-as.factor(li6801$Date)
li6801$Date<-as.Date(li6801$Date, format = "%m-%d-%Y")
li6801$LicorModel<-substr(li6801$FileName, 16, 21); li6801$LicorModel<-as.factor(li6801$LicorModel)


write.csv(li6801, "RVT_li6800_machine1 data.csv", row.names = F)

rm(list=setdiff(ls(), c("clean")))

############################################## 
##############################################

##############################################
############################################## 

rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
clean<-"C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Physiology Data/3_Files that say CLEANED go here"
setwd(clean)

############################################## 

temp = list.files(pattern="*6800.3")
for (i in 1:length(temp)) {
  assign(temp[i], read.csv(temp[i], header = F))
  
}
rm(temp, i)

dfs<-Filter(function(x) is(x, "data.frame"), mget(ls()))
dfs<-Map(cbind, filenames = names(dfs), dfs)

newdataset<-dfs[[1]]
obs.row<-which(newdataset[,3] == "Sys_obs_NA")
newdataset<-newdataset[-c(obs.row-2),]
for (i in 1:ncol(newdataset)) {
  colnames(newdataset)[i]<-as.character(newdataset[1,i])
  
}
names(newdataset)[1]<-"FileName"
names(newdataset)[2]<-"Unknown"
li6803<-newdataset[0,]; rm(newdataset)



for (i in 1:length(dfs)) {
  dat<-dfs[[i]]
  obs.row<-which(dat[,3] == "Sys_obs_NA")
  for (i in 1:ncol(dat)) {
    colnames(dat)[i]<-as.character(dat[1,i])
    
  }
  dat<-dat[-c(1),]
  names(dat)[1]<-"FileName"
  names(dat)[2]<-"Unknown"
  li6803<-bind_rows(li6803, dat)
  rm(dat)
}

levels(as.factor(li6803$FileName))

li6803$Date<-substr(li6803$FileName, 5, 14); li6803$Date<-as.factor(li6803$Date)
li6803$Date<-as.Date(li6803$Date, format = "%m-%d-%Y")
li6803$LicorModel<-substr(li6803$FileName, 16, 21); li6803$LicorModel<-as.factor(li6803$LicorModel)


write.csv(li6803, "RVT_li6800_machine3 data.csv", row.names = F)

rm(list=setdiff(ls(), c("clean")))

############################################## 
############################################## 
rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
clean<-"C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Physiology Data/3_Files that say CLEANED go here"
setwd(clean)

setwd(clean)
l64x<-read.csv("RVT_li6400 machinex data.csv")
l681<-read.csv("RVT_li6800_machine1 data.csv")
l683<-read.csv("RVT_li6800_machine3 data.csv")

# l683$Sys_hhmmss_NA<-as.factor(l683$Sys_hhmmss_NA)

l68<-bind_rows(l681, l683); rm(l681, l683)

l68$Ci.Ca_out<-l68$GasEx_Ci_µmol.mol.U.207B.¹/l68$GasEx_Ca_µmol.mol.U.207B.¹

names(l68)[3]<-"Obs_in"
names(l68)[7]<-"HHMMSS_in"
names(l68)[10]<-"Photo_out"
names(l68)[9]<-"Trmmol_out"
names(l68)[12]<-"Ci_out"
names(l68)[13]<-"Ci_Pa_out"
names(l68)[14]<-"Cond_out"
names(l68)[21]<-"CTleaf_out"
names(l68)[23]<-"RH_S_in"
names(l68)[24]<-"VpdL_out"
names(l68)[39]<-"PARi_in"
names(l68)[43]<-"Area_in"
names(l68)[44]<-"StmRat_in"
names(l68)[47]<-"CO2S_in"
names(l68)[48]<-"CO2R_in"
names(l68)[49]<-"H2OS_in"
names(l68)[50]<-"H2OR_in"
names(l68)[51]<-"Flow_in"
names(l68)[52]<-"Press_in"
names(l68)[54]<-"Tair_in"
names(l68)[55]<-"Tleaf_in"
names(l68)[92]<-"TBlk_in"
names(l68)[100]<-"UserIDs_in"
names(l68)[101]<-"Date"
names(l68)[102]<-"LicorModel"

l68$LicorModel<-as.factor(l68$LicorModel)

df<-bind_rows(l64x, l68); rm(l64x, l68)

# df$region<-str_extract(df$UserIDs_in, "(?<=\\s)(.*)(?=\\s)"); df$region<-as.factor(df$region)
# df$spp<-str_sub(df$UserIDs_in, 1, 2); df$spp<-as.factor(df$spp)
# df$plot<-str_sub(df$UserIDs_in, 7, length(df$UserIDs_in)); df$plot<-as.factor(df$plot)

# df$region<-as.factor(substr(df$UserIDs_in, 3, 4))
# df$spp<-as.factor(substr(df$UserIDs_in, 1, 2))
# df$plot<-as.factor(substr(df$UserIDs_in, nchar(df$UserIDs_in)-1, nchar(df$UserIDs_in)))

## Fixed ID recognition whether or not there are spaces in userids

df$KEEP<-"Y"

write.csv(df, "All KEEP are Y_Move by hand to masterfile.csv", row.names = F)



