############################################## 
############################################## Section i

## GUIDE ##

# Section 1: No user input required but read for potential issues

# Section 2: Installs libraries. User input may be required

# Section 3: Sets working directory, imports xlsx data. User input required!

# Section 4: Autorun section. No user input required.

# Section 5: Output new, cleaned csv. User input required.

##############################################
############################################## Section 1

## RESTRICTIONS ##

# 1. Ensure 6400 data are saved in the xlsx format

# 2. Make sure there are no "->" or ":" or "Log Option:" or "Launched"
# in remarks for entered remarks (i.e. for Plant IDs).

# 3. All measurements must occur from 00:00:01 to 23:59:59 on a single day
# If multiple days of measurements occur, must break into separate files

# 4. If multiple remarks are made between observations, only the latest (time-wise)
# will be kept

##############################################
############################################## Section 2

## LIBRARIES ##

## USER INPUT MAY BE REQUIRED

# ENSURE THESE LIBRARY ARE INSTALLED

library(readxl); library(DataCombine); library(chron)

rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ Clears All Old Files

##############################################
############################################## Section 3

## WORKING DIRECTORY AND XLSX IMPORT ##

## USER INPUT REQUIRED

# ENTER DIRECTORY NAME BELOW

dir<-"C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Physiology Data"
setwd(dir)

# ENTER FILE NAME BELOW

l64<-read_excel("RvT 12-07-2020 6400.x.xlsx", col_names = F)

# NAME OF OUTPUT CSV CAN BE CHANGED AT BOTTOM OF SCRIPT!!!

############################################## 
############################################## Section 4

## AUTORUN SECTION ##

## NO USER INPUT REQUIRED
# THIS SECTION AUTORUNS

l64$DF.OBS<-seq.int(nrow(l64))
DateTime<-l64[2,1]; DateTime<-as.character(DateTime)

unit.row<-which(l64$...1 == "Unit=")
unit.df<-data.frame(l64[unit.row,])
unit.df<-unit.df[sapply(unit.df, function(unit.df) !any(is.na(unit.df)))]
unit.df$DF.OBS<-NULL
LS.row<-which(l64$...1 == "LightSource=")
LS.df<-data.frame(l64[LS.row,])
LS.df<-LS.df[sapply(LS.df, function(LS.df) !any(is.na(LS.df)))]
LS.df$DF.OBS<-NULL
AD.row<-which(l64$...1 == "A/D AvgTime=")
AD.df<-data.frame(l64[AD.row,])
AD.df<-AD.df[sapply(AD.df, function(AD.df) !any(is.na(AD.df)))]
AD.df$DF.OBS<-NULL
Config.row<-which(l64$...1 == "Config=")
Config.df<-data.frame(l64[AD.row,])
Config.df<-Config.df[sapply(Config.df, function(Config.df) !any(is.na(Config.df)))]
Config.df$DF.OBS<-NULL
obs.row<-which(l64[,1] == "Obs")
remarks.rows<-which(l64$...1 == "Remark=")
remarks.above.obs<-remarks.rows[remarks.rows < as.numeric(obs.row)]
OtherComments.row<-which(l64$...1[1:as.numeric(obs.row-1)] != "Unit=" &
                           l64$...1[1:as.numeric(obs.row-1)] != "LightSource=" &
                           l64$...1[1:as.numeric(obs.row-1)] != "A/D AvgTime=" &
                           l64$...1[1:as.numeric(obs.row-1)] != "Config=")
OtherComments.row<-OtherComments.row[OtherComments.row != remarks.above.obs]
OtherComments.df<-data.frame(l64[OtherComments.row,])
OtherComments.df<-OtherComments.df[sapply(OtherComments.df, function(OtherComments.df) !any(is.na(OtherComments.df)))]
remarks.to.add<-data.frame(l64[remarks.above.obs,])

l64<-InsertRow(l64, NewRow = remarks.to.add, RowNum = as.numeric(obs.row+2))
l64<-l64[-c(1:as.numeric(obs.row-1)), ]
for(i in 1:ncol(l64)) {
  l64[1,i]<-paste(l64[1,i], l64[2,i], sep = "_")
}
obs.row<-which(l64[,1] == "Obs_in")
l64<-l64[-c(as.numeric(obs.row+1)), ]
colnames(l64)<-l64[as.numeric(obs.row),]
obs.row<-which(l64[,1] == "Obs_in")
l64<-l64[-c(as.numeric(obs.row)), ]

Unit<-as.character(unit.df)
Unit<-paste(Unit, collapse = '')
LS<-as.character(LS.df)
LS<-paste(LS, collapse = '')
AD.Time<-as.character(AD.df)
AD.Time<-paste(AD.Time, collapse = '')
Config<-as.character(Config.df)
Config<-paste(Config, collapse = '')
OtherComments<-as.character(OtherComments.df)
OtherComments<-paste(OtherComments, collapse = '')
AddOns<-data.frame(Unit, LS, AD.Time, Config, OtherComments, DateTime)

l64$Unit<-AddOns$Unit
l64$LS<-AddOns$LS
l64$Ad.Time<-AddOns$AD.Time
l64$Config<-AddOns$Config
l64$OtherComments<-AddOns$OtherComments
l64$`9_10`<-NULL
l64$DateTime<-substr(DateTime, 4, 15)

rm(list=setdiff(ls(), "l64"))

l64$HHMMSS_in<-gsub('"', '', l64$HHMMSS_in)
l64$Time<-as.character(l64$HHMMSS_in)
l64$Time<-substr(l64$Time, 0, 8)

l64$hr<-as.numeric(substr(l64$Time, 0, 2))
l64$mi<-as.numeric(substr(l64$Time, 4, 5))
l64$se<-as.numeric(substr(l64$Time, 7, 8))
l64$Seconds<-(l64$hr*60*60)+(l64$mi*60)+(l64$se)
l64$Time.pos<-chron(times = l64$Time, format = ('hh:mm:ss'))
l64<-l64[!is.na(l64$Time.pos), ]

remarks<-subset(l64, Obs_in == "Remark=")
# l64<-subset(l64, Obs_in != "Remark=")

max.ch<-max(nchar(remarks$HHMMSS_in), na.rm = T)
remarks$HHMMSS_in<-substr(remarks$HHMMSS_in, 9, max.ch)
remarks<-remarks[!is.na(remarks$HHMMSS_in), ]
remarks$HHMMSS_in<-trimws(remarks$HHMMSS_in, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")

CO2_Mixer<-grep("^CO2 Mixer:", remarks$HHMMSS_in)
CO2_Mixer<-data.frame(CO2_Mixer)
i<-unique(CO2_Mixer)
remarks$CO2_Mixer_setting<-NA
cn<-which(colnames(remarks)=="CO2_Mixer_setting")
for (i in CO2_Mixer){
  remarks[i,cn]<-paste(remarks$HHMMSS_in[i])
}

Tblock<-grep("^Coolers:", remarks$HHMMSS_in)
Tblock<-data.frame(Tblock)
i<-unique(Tblock)
remarks$Tblock_setting<-NA
cn<-which(colnames(remarks)=="Tblock_setting")
for (i in Tblock){
  remarks[i,cn]<-paste(remarks$HHMMSS_in[i])
}

Flow<-grep("^Flow:", remarks$HHMMSS_in)
Flow<-data.frame(Flow)
i<-unique(Flow)
remarks$Flow_setting<-NA
cn<-which(colnames(remarks)=="Flow_setting")
for (i in Flow){
  remarks[i,cn]<-paste(remarks$HHMMSS_in[i])
}

Lamp<-grep("^Lamp:", remarks$HHMMSS_in)
Lamp<-data.frame(Lamp)
i<-unique(Lamp)
remarks$Lamp_setting<-NA
cn<-which(colnames(remarks)=="Lamp_setting")
for (i in Lamp){
  remarks[i,cn]<-paste(remarks$HHMMSS_in[i])
}

Launch<-grep("^Launched", remarks$HHMMSS_in)
Logs<-grep("^Log Option", remarks$HHMMSS_in)
LogRemarks1<-c(Launch, Logs)
rm(Launch, Logs)
remarks$LogRemarks1<-NA
LogRemarks1<-data.frame(LogRemarks1)
i<-unique(LogRemarks1)
cn<-which(colnames(remarks)=="LogRemarks1")
for (i in LogRemarks1){
  remarks[i,cn]<-paste(remarks$HHMMSS_in[i])
}
rm(CO2_Mixer, Tblock, Flow, Lamp, LogRemarks1)

Remark.catch1<-grep("->", remarks$HHMMSS_in)
Remark.catch2<-grep(":", remarks$HHMMSS_in)
Remark.catch3<-grep("Log Option:", remarks$HHMMSS_in)
Remark.catch4<-grep("Launched", remarks$HHMMSS_in)
Remark.catch<-c(Remark.catch1, Remark.catch2, Remark.catch3, Remark.catch4)

remarks$Remark.catch<-NA
Remark.catch<-data.frame(Remark.catch)
i<-unique(Remark.catch)
cn<-which(colnames(remarks)=="Remark.catch")
for (i in Remark.catch){
  remarks[i,cn]<-paste(remarks$HHMMSS_in[i])
}

nonIDs<-sort(Remark.catch[!(duplicated(Remark.catch$Remark.catch)),])
Remark.catch<-data.frame(nonIDs)
UserIDs<-remarks[-c(as.numeric(nonIDs)),]

rm(list=setdiff(ls(), c("l64", "UserIDs", "remarks")))

l64$UserIDs_in<-NA
i<-unique(UserIDs$Time.pos)
for (i in UserIDs$Time.pos){
  refrows<-which(l64$Time.pos > chron(times = i, format = ('hh:mm:ss')))
  l64$UserIDs_in[refrows]<-as.character(UserIDs$HHMMSS_in[UserIDs$Time.pos == i])
}

l64$CO2_Mixer_setting<-NA
dummy<-remarks[!is.na(remarks$CO2_Mixer_setting), ]
i<-unique(dummy$Time.pos)
for (i in dummy$Time.pos){
  refrows<-which(l64$Time.pos > chron(times = i, format = ('hh:mm:ss')))
  l64$CO2_Mixer_setting[refrows]<-as.character(dummy$CO2_Mixer_setting[dummy$Time.pos == i])
}

l64$Tblock_setting<-NA
dummy<-remarks[!is.na(remarks$Tblock_setting), ]
i<-unique(dummy$Time.pos)
for (i in dummy$Time.pos){
  refrows<-which(l64$Time.pos > chron(times = i, format = ('hh:mm:ss')))
  l64$Tblock_setting[refrows]<-as.character(dummy$Tblock_setting[dummy$Time.pos == i])
}

l64$Flow_setting<-NA
dummy<-remarks[!is.na(remarks$Flow_setting), ]
i<-unique(dummy$Time.pos)
for (i in dummy$Time.pos){
  refrows<-which(l64$Time.pos > chron(times = i, format = ('hh:mm:ss')))
  l64$Flow_setting[refrows]<-as.character(dummy$Flow_setting[dummy$Time.pos == i])
}

l64$Lamp_setting<-NA
dummy<-remarks[!is.na(remarks$Lamp_setting), ]
i<-unique(dummy$Time.pos)
for (i in dummy$Time.pos){
  refrows<-which(l64$Time.pos > chron(times = i, format = ('hh:mm:ss')))
  l64$Lamp_setting[refrows]<-as.character(dummy$Lamp_setting[dummy$Time.pos == i])
}

l64$LogRemarks1<-NA
dummy<-remarks[!is.na(remarks$LogRemarks1), ]
i<-unique(dummy$Time.pos)
for (i in dummy$Time.pos){
  refrows<-which(l64$Time.pos > chron(times = i, format = ('hh:mm:ss')))
  l64$LogRemarks1[refrows]<-as.character(dummy$LogRemarks1[dummy$Time.pos == i])
}

l64$Remark.catch<-NA
dummy<-remarks[!is.na(remarks$Remark.catch), ]
i<-unique(dummy$Time.pos)
for (i in dummy$Time.pos){
  refrows<-which(l64$Time.pos > chron(times = i, format = ('hh:mm:ss')))
  l64$Remark.catch[refrows]<-as.character(dummy$Remark.catch[dummy$Time.pos == chron(times = i, format = ('hh:mm:ss'))])
}

rm(list=setdiff(ls(), c("l64", "remarks")))

remarks<-data.frame(remarks$Obs_in, remarks$HHMMSS_in, remarks$Time.pos)
l64<-subset(l64, Obs_in != "Remark=")

# END OF AUTORUN SECTION

##############################################
############################################## Section 5

## FILE OUTPUT ##

## NO USER INPUT REQUIRED

# CHANGE OUTPUT CSV FILENAME HERE!
# WILL FIND IN YOUR SET WORKING DIRECTORY (FROM TOP SECTION)

write.csv(l64, "RvT 12-07-2020 6400.x_Cleaned_li6400.csv", row.names=FALSE)
write.csv(remarks, "RvT 12-07-2020 6400.x_Check all remarks for input mistakes.csv")
### END FILE RvT April 18 2020_6400.x_