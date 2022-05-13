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

# 1. LICOR6800 files are in the xlsx format. There should be two sheets (1st with measurements, 2nd with remarks)

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
# MARK SHEETS 1 and 2

MS<-read_xlsx("RvT 12-07-2020 6800.3.xlsx", sheet = 1, col_names = F) # These files cannot be open when importing
RE<-read_xlsx("RvT 12-07-2020 6800.3.xlsx", sheet = 2, col_names = F) # These files cannot be open when importing

# NAME OF OUTPUT CSV CAN BE CHANGED AT BOTTOM OF SCRIPT!!!

############################################## 
############################################## Section 4

obs.row<-which(MS[,1] == "obs") # also row of variable names (e.g. A, E, gsw)
unit.row<-obs.row+1 # units
type.row<-obs.row-1 # Sys, GasEx, Status, etc.

shit.from.MS<-data.frame(MS[1:as.numeric(type.row-1), 1:ncol(MS)])
MS<-data.frame(MS[as.numeric(type.row):nrow(MS), 1:ncol(MS) ])

for(i in 1:ncol(MS)) {
  MS[1,i]<-paste(MS[1,i], MS[2,i], sep = "_")
}
for(i in 1:ncol(MS)) {
  MS[1,i]<-paste(MS[1,i], MS[3,i], sep = "_")
}

obs.row<-which(MS[,1] == "obs"); type.row<-obs.row-1
MS<-MS[-c(as.numeric(type.row+1)), ]; MS<-MS[-c(as.numeric(type.row+1)), ]

colnames(MS)<-MS[as.numeric(type.row),]
type.row<-which(MS[,1] == "Sys_obs_NA")
MS<-MS[-c(as.numeric(type.row)), ]

rm(list=setdiff(ls(), c("MS","RE", "shit.from.MS")))

shit.rows<-(grep("A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z", RE$...1))
shit.from.RE<-data.frame(RE[shit.rows, ])
RE<-RE[-c(shit.rows), ]
names(RE)[1]<-"Time"
names(RE)[2]<-"UserID_in"


MS$UserIDs_in<-NA

dummy<-RE[!is.na(RE$UserID_in), ]
i<-unique(dummy$Time)
for (i in dummy$Time){
  refrows<-which(chron(times = MS$Sys_hhmmss_NA, format = ('hh:mm:ss')) > chron(times = i, format = ('hh:mm:ss')))
  MS$UserIDs_in[refrows]<-as.character(dummy$UserID_in[dummy$Time == chron(times = i, format = ('hh:mm:ss'))])
}



write.csv(MS, "RvT 12-07-2020 6800.3_Cleaned_6800.csv")
write.csv(RE, "RvT 12-07-2020 6800.3_Check all remarks for input mistakes.csv")