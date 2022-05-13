library(dplyr); library(readxl); library(lubridate)
library(doBy)
rm(list=ls())
dev.off()
cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
############################################## 

### Loop for weather csv's and excel files


############################################################################################
############################################################################################

# Start with weather folders

############################################################################################
############################################################################################

setwd("C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Weather Data")
source<-read.csv("Mangrove RVT weather data_all.csv")
source$SVP_kPa<-(610.78*(2.71828^(source$Temp_C/(source$Temp_C+238.3) * 17.2694)))/1000
source$VPD_kPa<-source$SVP_kPa * (1 - (source$RH_perc/100))

# rain<-read.csv("Boatyard Rainfall.csv") # Make sure date is available
# rain$Date<-as.Date(rain$Date, format = "%m/%d/%Y")

df<-summaryBy(Temp_C + RH_perc + VPD_kPa ~ Date + Treatment + Block, FUN = c(mean,min,max), na.rm = T, source)
df$Date<-as.Date(df$Date, format = "%m/%d/%Y")
df<-df[order(df$Date),]

min(df$Date)

############################################
############################################

tiff(file = "V_Temp~Treatment x Date & TempDiff~Date.tiff", height = 6, width = 10, res = 600, units = "in", compression = "zip+p")

############################################

par(mfrow = c(4,1), mar = c(0.5,0.8,0.5,0.1), omi = c(0.7,0.5,0.05,0.05))

############################################

plot(df$Temp_C.mean ~ df$Date, pch = NA, xaxt="n",yaxt="n",xlab="",ylab="", ylim = c(-5,45))
axis(2, at = seq(-5,45,5), las = 2)
axis.Date(1, df$Date, at = seq(min(df$Date, na.rm = T), max(df$Date, na.rm = T),"months"), labels = F, format = "%m/%d/%y", cex.axis = 1)

rect(xleft = as.Date("2020-04-17"), xright = as.Date("2020-04-18"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-05-15"), xright = as.Date("2020-05-16"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-06-09"), xright = as.Date("2020-06-10"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-06-09"), xright = as.Date("2020-06-10"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-07-09"), xright = as.Date("2020-07-10"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-08-13"), xright = as.Date("2020-08-14"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-10-12"), xright = as.Date("2020-10-13"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-12-06"), xright = as.Date("2020-12-07"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)


box()

dat<-subset(df, Treatment == "Ambi" & Block == "2")
points(dat$Temp_C.mean ~ dat$Date, type = "l", col = "black", lty = 1, lwd = 2)
points(dat$Temp_C.min ~ dat$Date, type = "l", col = "black", lty = 2)
points(dat$Temp_C.max ~ dat$Date, type = "l", col = "black", lty = 2)

dat<-subset(df, Treatment == "Warm")
dat<-summaryBy(Temp_C.mean + Temp_C.min + Temp_C.max ~ Date, FUN = mean, na.rm = T, dat)
points(dat$Temp_C.mean.mean ~ dat$Date, type = "l", col = "firebrick", lty = 1, lwd = 2)
points(dat$Temp_C.min.mean ~ dat$Date, type = "l", col = "firebrick", lty = 2)
points(dat$Temp_C.max.mean ~ dat$Date, type = "l", col = "firebrick", lty = 2)

legend("bottom", bty = "n", col = c("black","firebrick","grey50"), lwd = 2,
       c("Ambient", "Warmed", "Daily Min/Max"), lty = c(1,1,2), horiz = T, cex = 1.4)

legend("topleft", "a)", bty = "n", cex = 1.5, adj = 1)

mtext(side = 2, expression(T[air]~(degree*C)), padj = -1.5, cex = 1.2)

############################################

plot(df$RH_perc.mean ~ df$Date, pch = NA, xaxt="n",yaxt="n",xlab="",ylab="", ylim = c(0,100))
axis(2, at = seq(0,100,10), las = 2)
axis.Date(1, df$Date, at = seq(min(df$Date, na.rm = T), max(df$Date, na.rm = T),"months"), labels = F, format = "%m/%d/%y", cex.axis = 1)

rect(xleft = as.Date("2020-04-17"), xright = as.Date("2020-04-18"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-05-15"), xright = as.Date("2020-05-16"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-06-09"), xright = as.Date("2020-06-10"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-06-09"), xright = as.Date("2020-06-10"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-07-09"), xright = as.Date("2020-07-10"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-08-13"), xright = as.Date("2020-08-14"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-10-12"), xright = as.Date("2020-10-13"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-12-06"), xright = as.Date("2020-12-07"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)


box()

dat<-subset(df, Treatment == "Ambi" & Block == "2")
points(dat$RH_perc.mean ~ dat$Date, type = "l", col = "black", lty = 1, lwd = 2)
points(dat$RH_perc.min ~ dat$Date, type = "l", col = "black", lty = 2)
points(dat$RH_perc.max ~ dat$Date, type = "l", col = "black", lty = 2)

dat<-subset(df, Treatment == "Warm")
dat<-summaryBy(RH_perc.mean + RH_perc.min + RH_perc.max ~ Date, FUN = mean, na.rm = T, dat)
points(dat$RH_perc.mean.mean ~ dat$Date, type = "l", col = "firebrick", lty = 1, lwd = 2)
points(dat$RH_perc.min.mean ~ dat$Date, type = "l", col = "firebrick", lty = 2)
points(dat$RH_perc.max.mean ~ dat$Date, type = "l", col = "firebrick", lty = 2)

legend("topleft", "b)", bty = "n", cex = 1.5, adj = 1)

mtext(side = 2, expression(RH~('%')), padj = -1.5, cex = 1.2)

############################################

plot(df$VPD_kPa.mean ~ df$Date, pch = NA, xaxt="n",yaxt="n",xlab="",ylab="", ylim = c(0,7))
axis(2, at = seq(0,7,1), las = 2)
axis.Date(1, df$Date, at = seq(min(df$Date, na.rm = T), max(df$Date, na.rm = T),"months"), labels = F, format = "%m/%d/%y", cex.axis = 1)

rect(xleft = as.Date("2020-04-17"), xright = as.Date("2020-04-18"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-05-15"), xright = as.Date("2020-05-16"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-06-09"), xright = as.Date("2020-06-10"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-06-09"), xright = as.Date("2020-06-10"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-07-09"), xright = as.Date("2020-07-10"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-08-13"), xright = as.Date("2020-08-14"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-10-12"), xright = as.Date("2020-10-13"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-12-06"), xright = as.Date("2020-12-07"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
box()

dat<-subset(df, Treatment == "Ambi" & Block == "2")
points(dat$VPD_kPa.mean ~ dat$Date, type = "l", col = "black", lty = 1, lwd = 2)
points(dat$VPD_kPa.min ~ dat$Date, type = "l", col = "black", lty = 2)
points(dat$VPD_kPa.max ~ dat$Date, type = "l", col = "black", lty = 2)

dat<-subset(df, Treatment == "Warm")
dat<-summaryBy(VPD_kPa.mean + VPD_kPa.min + VPD_kPa.max ~ Date, FUN = mean, na.rm = T, dat)
points(dat$VPD_kPa.mean.mean ~ dat$Date, type = "l", col = "firebrick", lty = 1, lwd = 2)
points(dat$VPD_kPa.min.mean ~ dat$Date, type = "l", col = "firebrick", lty = 2)
points(dat$VPD_kPa.max.mean ~ dat$Date, type = "l", col = "firebrick", lty = 2)

legend("topleft", "c)", bty = "n", cex = 1.5, adj = 1)

mtext(side = 2, expression(VPD~(kPa)), padj = -1.5, cex = 1.2)

# par(new=T)

# plot(rain$Rain_mm ~ rain$Date, pch = NA, xaxt="n",yaxt="n",xlab="",ylab="", ylim = c(0,150))
# points(rain$Rain_mm ~ rain$Date, type = "h", lwd = 1.75, col = "blue")
# axis(4, seq(0,150,25), las = 2)
# 
# mtext(side = 4, "Rainfall (mm)", cex = 1.2, padj = 3)
# 
# legend("top", c("Rain"), col = "blue", pch = 15, bty = "n", horiz = T, cex = 1.4)

############################################


df<-summaryBy(Temp_C ~ Date * Treatment, FUN = c(min,mean,max), na.rm = T, source)
dat<-subset(df, Treatment == "Warm")
dat2<-subset(df, Treatment == "Ambi")
df<-merge(dat, dat2, by = c("Date")); rm(dat, dat2)
df$min<-df$Temp_C.min.x-df$Temp_C.min.y
df$mean<-df$Temp_C.mean.x-df$Temp_C.mean.y
df$max<-df$Temp_C.max.x-df$Temp_C.max.y
df$Date<-as.Date(df$Date, format = "%m/%d/%Y")
df<-subset(df, Date > "2020-02-26")
df<-df[order(df$Date),]


plot(df$max ~ df$Date, pch = NA, xaxt="n",yaxt="n",xlab="",ylab="", ylim = c(-2,10))
axis(2, at = seq(-20,20,2), las = 2)
axis.Date(1, df$Date, at = seq(min(df$Date, na.rm = T), max(df$Date, na.rm = T),"months"), las = 2, format = "%b %d", cex.axis = 1.4)

rect(xleft = as.Date("2020-04-17"), xright = as.Date("2020-04-18"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-05-15"), xright = as.Date("2020-05-16"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-06-09"), xright = as.Date("2020-06-10"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-06-09"), xright = as.Date("2020-06-10"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-07-09"), xright = as.Date("2020-07-10"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-08-13"), xright = as.Date("2020-08-14"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-10-12"), xright = as.Date("2020-10-13"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-12-06"), xright = as.Date("2020-12-07"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)

box()

points(df$min ~ df$Date, type = "l", col = "blue", lty = 1)
points(df$mean ~ df$Date, type = "l", col = "black", lty = 1)
points(df$max ~ df$Date, type = "l", col = "firebrick", lty = 1)

abline(h = 0, lty = 2, col = "grey40")

legend("top", bty = "n", col = c("black","blue","firebrick"), lwd = 2,
       c("Mean", "Min", "Max"), lty = c(1), horiz = T, cex = 1.4)

legend("topleft", "d)", bty = "n", cex = 1.5, adj = 1)

mtext(side = 2, expression(Delta~T[air]~(degree*C)), padj = -1.5, cex = 1.2)

dev.off()
