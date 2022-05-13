# in data ####
library(doBy); library(lubridate); library(plotrix)

rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

setwd("C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Ecotype Climate Origin")

df<-read.csv("Ft Myers Florida (2016-2018).csv")
t1<-read.csv("Twin Cayes 2016a.csv")
t2<-read.csv("Twin Cayes 2017.csv")
t3<-read.csv("Twin Cayes 2018.csv")
tc<-rbind(t1,t2,t3)
tc<-tc[!is.na(tc$Temp_C),]

df$Tmax<-(df$Tmax-32)*(5/9)
df$Tmin<-(df$Tmin-32)*(5/9)
df$Rain_mm<-df$PRECIPITATION

dfr<-df
dfr$Date<-as.Date(dfr$Date, format = "%m/%d/%Y")
dfr$MO<-month(dfr$Date)
dfr<-summaryBy(Rain_mm ~ MO, FUN = sum, na.rm = T, dfr)
dfr$Rain_mm<-dfr$Rain_mm.sum/3

tcr<-tc
tcr$Date<-as.Date(tcr$Date, format = "%m/%d/%Y")
tcr$MO<-month(tcr$Date)
tcr<-summaryBy(Rain_mm ~ MO, FUN = sum, na.rm = T, tcr)
tcr$Rain_mm<-tcr$Rain_mm.sum/300

df<-summaryBy(Tmax + Tmin + Rain_mm ~ MoDay, FUN = c(min,max,mean), na.rm = T, df)
df$Date<-as.Date(df$MoDay, format = "%d-%b")

tc<-summaryBy(Temp_C + Rain_mm ~ MoDay, FUN = c(min,max,mean), na.rm = T, tc)
tc$Date<-as.Date(tc$MoDay, format = "%d-%b")

# plot ####

# tiff(file = "V_Mangrove home climate parameters.tiff", height = 7, width = 10, res = 600, units = "in", compression = "zip+p")

par(mfrow = c(2,1), mar = c(1,1,1,1), omi = c(0.7,1,0.1,0.1))

tc<-tc[order(tc$Date),]
plot(tc$Temp_C.min ~ tc$Date, type = "l", col = "blue", ylim = c(-5,40), ylab = "", xlab = "", xaxt = "n", yaxt = "n")
points(tc$Temp_C.max ~ tc$Date, type = "l", col = "firebrick")
df<-df[order(df$Date),]
points(df$Tmin.min ~ df$Date, type = "l", col = "blue", lty = 2)
points(df$Tmax.max ~ df$Date, type = "l", col = "firebrick", lty = 2)

axis.Date(1, tc$Date, labels = F, at = seq(min(tc$Date), max(tc$Date+1), "months"))
axis(2, at = seq(0,40,10), las = 2, cex.axis = 1.25)

mtext(side = 2, expression(Temperature~(degree*C)), cex = 1.5, padj = -2.5)

legend("bottom", c("Daily Max", "Daily Min", "Twin Cayes, Belize", "Fort Myers, Florida"),
       pch = c(15,15,NA,NA), lty = c(NA,NA,1,2), lwd = 2,
       col = c("firebrick","blue","black","black"), bty = "n")

##########################################################

plot(tcr$Rain_mm ~ as.numeric(tcr$MO), pch = NA, ylim = c(0,300), xaxt = "n", yaxt = "n")

points(tcr$Rain_mm ~ as.numeric(tcr$MO+0.05), pch = 6)
points(tcr$Rain_mm ~ as.numeric(tcr$MO+0.05), type = "l", lty = 1)
points(dfr$Rain_mm ~ as.numeric(dfr$MO-0.05), type = "l", lty = 2)
points(dfr$Rain_mm ~ as.numeric(dfr$MO-0.05), pch = 2, lty = 1)

axis(1, at = seq(1,12,1), labels = c("Jan","Feb","Mar","Apr","May","Jun",
                                     "Jul","Aug","Sep","Oct","Nov", "Dec"))
axis(2, at = seq(0,300,50), las = 2, cex.axis = 1.25)

mtext(side = 2, expression(Rainfall~(mm)), cex = 1.5, padj = -2.5)
mtext(side = 1, "Month", cex = 1.5, padj = 3.5)

legend("topleft", c("Twin Cayes, Belize","Fort Myers, Florida"), pch = c(6,2), lty = c(1,2), bty = "n", lwd = 2)

# dev.off()
