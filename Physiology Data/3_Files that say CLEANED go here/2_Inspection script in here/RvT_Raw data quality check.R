library(dplyr); library(readxl); library(lubridate)
library(minpack.lm); library(lattice)

rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
############################################## 


############################################################################################
############################################################################################

# Make sure old files are removed from the 'clean' wd
# If old files are in there they'll be pulled and reassigned a Y for KEEP

############################################################################################
############################################################################################

clean<-"C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Physiology Data/3_Files that say CLEANED go here"
raw<-"C:/Users/Jeff/OneDrive - Auburn University//Other Projects/Belize Florida Mangroves Exp/Physiology Data/3_Files that say CLEANED go here/2_Inspection script in here/Quality Check Plots"

############################################################################################

setwd(clean)

df<-read.csv("All KEEP are Y_Move by hand to masterfile.csv") # Edit this file based on graphs

df$Date<-as.Date(df$Date, "%m/%d/%Y")
df$R.area<-df$Photo_out*-1
df$R.area_log<-log(df$R.area)

df<-subset(df, KEEP == "Y")

############################################################################################
############################################################################################

setwd(raw)

dates<-(unique(df$Date))

setwd(raw)
for (dates in unique(df$Date)){
  dat<-subset(df, df$Date == dates)
  pdf(file = paste("RvT_rawdata_",as.character(unique(dat$Date)),".pdf",sep = ""))
  print(xyplot(R.area ~ CTleaf_out | UserIDs_in, data = dat, main = paste(as.character(unique(dat$Date)))))
  dev.off()
}

############################################################################################
############################################################################################

setwd(raw)

# dates<-as.character(unique(df$Date))
# setwd(raw)
# for (dates in unique(df$Date)){
#   dat<-subset(df, df$Date == dates)
#   pdf(paste("WETFEET_lograwdata_",as.character(unique(dat$Date)),".pdf",sep = ""))
#   print(xyplot(R.area_log ~ CTleaf_out | UserIDs_in, data = dat, main = paste(as.character(unique(dat$Date)))))
#   dev.off()
# }

# Once all the plots look good, manually add them to the WETFEET MASTER file (in 'out_data' path)
1+1 # and done!