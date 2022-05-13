# in data ####
library(doBy); library(chron)

setwd("C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Weather Data")

rm(list=ls()) # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
dev.off()     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
cat("\f")     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

df<-read.csv("Mangrove RVT weather data_all.csv")
df$Date<-as.Date(df$Date, format = "%Y-%m-%d")
df$SVP_kPa<-(610.78*(2.71828^(df$Temp_C/(df$Temp_C+238.3) * 17.2694)))/1000
df$VPD_kPa<-df$SVP_kPa * (1 - (df$RH_perc/100))
df$Time<-times(df$Time)
day<-subset(df, Time > "06:59:59" & Time < "19:00:01")
night1<-subset(df, Time < "07:00:00")
night2<-subset(df, Time > "19:00:00")
night<-rbind(night1,night2); rm(night1,night2,df)

# start ####

tiff(file = "V_Mangrove environment histograms.tiff", height = 8, width = 8, res = 600, units = "in", compression = "zip+p")

par(mfrow = c(3,2), mar = c(4,2,1,2), omi = c(0.4,0.7,0.01,0.01))

# a) day temp ####

dum<-subset(day, Treatment == "Ambi")
x<-density(dum$Temp_C)
x$y<-(x$y)/sum(x$y)
plot(x$y~x$x, type = "l", col = "white", xlim = c(5,50), ylim = c(0,0.01), xlab="",ylab="",xaxt="n",yaxt="n")
axis(2, seq(0,0.01,0.0025), las = 2)
axis(1, seq(0,100,5))

xb<-mean(dum$Temp_C, na.rm = T); xb<-format(round(xb,1), nsmall = 1)
bo<-quantile(dum$Temp_C, seq(0,1,0.05))[2]; bo<-format(round(bo,1), nsmall = 1)
to<-quantile(dum$Temp_C, seq(0,1,0.05))[20]; to<-format(round(to,1), nsmall = 1)

abline(v = xb, lty = 1, lwd = 2, col = rgb(0,0,1, alpha = 0.5))
abline(v = bo, lty = 2, lwd = 1, col = rgb(0,0,1, alpha = 0.5))
abline(v = to, lty = 2, lwd = 1, col = rgb(0,0,1, alpha = 0.5))
points(x$y~x$x, type = "l", col = "blue", lwd = 2)
x<-data.frame(x$x,x$y)
dum<-subset(x, x$x.x < to & x$x.x > bo)
points(dum$x.y~dum$x.x, type = "h", col = rgb(0,0,1, alpha = 0.2), lwd = 2)

dum<-subset(day, Treatment == "Warm")
x<-density(dum$Temp_C)
x$y<-(x$y)/sum(x$y)
xb<-mean(dum$Temp_C, na.rm = T); xb<-format(round(xb,1), nsmall = 1)
bo<-quantile(dum$Temp_C, seq(0,1,0.05))[2]; bo<-format(round(bo,1), nsmall = 1)
to<-quantile(dum$Temp_C, seq(0,1,0.05))[20]; to<-format(round(to,1), nsmall = 1)

abline(v = xb, lty = 1, lwd = 2, col = rgb(1,0,0, alpha = 0.5))
abline(v = bo, lty = 2, lwd = 1, col = rgb(1,0,0, alpha = 0.5))
abline(v = to, lty = 2, lwd = 1, col = rgb(1,0,0, alpha = 0.5))
points(x$y~x$x, type = "l", col = "firebrick", lwd = 2)
x<-data.frame(x$x,x$y)
dum<-subset(x, x$x.x < to & x$x.x > bo)
points(dum$x.y~dum$x.x, type = "h", col = rgb(1,0,0, alpha = 0.2), lwd = 2)

legend("topleft", "a)", bty = "n", cex = 1.2)

# b) night temp ####

dum<-subset(night, Treatment == "Ambi")
x<-density(dum$Temp_C)
x$y<-(x$y)/sum(x$y)
plot(x$y~x$x, type = "l", col = "white", xlim = c(-2,35), ylim = c(0,0.02), xlab="",ylab="",xaxt="n",yaxt="n")
axis(2, at = seq(0,0.02,0.005), las = 2)
axis(1, seq(0,100,5))

xb<-mean(dum$Temp_C, na.rm = T); xb<-format(round(xb,1), nsmall = 1)
bo<-quantile(dum$Temp_C, seq(0,1,0.05))[2]; bo<-format(round(bo,1), nsmall = 1)
to<-quantile(dum$Temp_C, seq(0,1,0.05))[20]; to<-format(round(to,1), nsmall = 1)

abline(v = xb, lty = 1, lwd = 2, col = rgb(0,0,1, alpha = 0.5))
abline(v = bo, lty = 2, lwd = 1, col = rgb(0,0,1, alpha = 0.5))
abline(v = to, lty = 2, lwd = 1, col = rgb(0,0,1, alpha = 0.5))
points(x$y~x$x, type = "l", col = "blue", lwd = 2)
x<-data.frame(x$x,x$y)
dum<-subset(x, x$x.x < to & x$x.x > bo)
points(dum$x.y~dum$x.x, type = "h", col = rgb(0,0,1, alpha = 0.2), lwd = 2)

dum<-subset(night, Treatment == "Warm")
x<-density(dum$Temp_C)
x$y<-(x$y)/sum(x$y)
xb<-mean(dum$Temp_C, na.rm = T); xb<-format(round(xb,1), nsmall = 1)
bo<-quantile(dum$Temp_C, seq(0,1,0.05))[2]; bo<-format(round(bo,1), nsmall = 1)
to<-quantile(dum$Temp_C, seq(0,1,0.05))[20]; to<-format(round(to,1), nsmall = 1)

abline(v = xb, lty = 1, lwd = 2, col = rgb(1,0,0, alpha = 0.5))
abline(v = bo, lty = 2, lwd = 1, col = rgb(1,0,0, alpha = 0.5))
abline(v = to, lty = 2, lwd = 1, col = rgb(1,0,0, alpha = 0.5))
points(x$y~x$x, type = "l", col = "firebrick", lwd = 2)
x<-data.frame(x$x,x$y)
dum<-subset(x, x$x.x < to & x$x.x > bo)
points(dum$x.y~dum$x.x, type = "h", col = rgb(1,0,0, alpha = 0.2), lwd = 2)

mtext(side = 1, expression(italic(T)[air]~(degree*C)), padj = 1.6, adj = -0.25, cex = 1.5)
legend("topleft", "b)", bty = "n", cex = 1.2)

# c) day rh ####

dum<-subset(day, Treatment == "Ambi")
x<-density(dum$RH_perc)
x$y<-(x$y)/sum(x$y)
plot(x$y~x$x, type = "l", col = "white", xlim = c(10,100), ylim = c(0,0.005), xlab="",ylab="",xaxt="n",yaxt="n")
axis(2, seq(0,0.01,0.00125), las = 2)
axis(1, seq(0,100,20))

xb<-mean(dum$RH_perc, na.rm = T); xb<-format(round(xb,1), nsmall = 1)
bo<-quantile(dum$RH_perc, seq(0,1,0.05))[2]; bo<-format(round(bo,1), nsmall = 1)
to<-quantile(dum$RH_perc, seq(0,1,0.05))[20]; to<-format(round(to,1), nsmall = 1)

abline(v = xb, lty = 1, lwd = 2, col = rgb(0,0,1, alpha = 0.5))
abline(v = bo, lty = 2, lwd = 1, col = rgb(0,0,1, alpha = 0.5))
abline(v = to, lty = 2, lwd = 1, col = rgb(0,0,1, alpha = 0.5))
points(x$y~x$x, type = "l", col = "blue", lwd = 2)
x<-data.frame(x$x,x$y)
dum<-subset(x, x$x.x < as.numeric(to) & x$x.x > as.numeric(bo))
points(dum$x.y~dum$x.x, type = "h", col = rgb(0,0,1, alpha = 0.2), lwd = 2)

dum<-subset(day, Treatment == "Warm")
x<-density(dum$RH_perc)
x$y<-(x$y)/sum(x$y)
xb<-mean(dum$RH_perc, na.rm = T); xb<-format(round(xb,1), nsmall = 1)
bo<-quantile(dum$RH_perc, seq(0,1,0.05))[2]; bo<-format(round(bo,1), nsmall = 1)
to<-quantile(dum$RH_perc, seq(0,1,0.05))[20]; to<-format(round(to,1), nsmall = 1)

abline(v = xb, lty = 1, lwd = 2, col = rgb(1,0,0, alpha = 0.5))
abline(v = bo, lty = 2, lwd = 1, col = rgb(1,0,0, alpha = 0.5))
abline(v = to, lty = 2, lwd = 1, col = rgb(1,0,0, alpha = 0.5))
points(x$y~x$x, type = "l", col = "firebrick", lwd = 2)
x<-data.frame(x$x,x$y)
dum<-subset(x, x$x.x < as.numeric(to) & x$x.x > as.numeric(bo))
points(dum$x.y~dum$x.x, type = "h", col = rgb(1,0,0, alpha = 0.2), lwd = 2)

legend("topleft", "c)", bty = "n", cex = 1.2)

# d) night rh ####

dum<-subset(night, Treatment == "Ambi")
x<-density(dum$RH_perc)
x$y<-(x$y)/sum(x$y)
plot(x$y~x$x, type = "l", col = "white", xlim = c(60,105), ylim = c(0,0.035), xlab="",ylab="",xaxt="n",yaxt="n")
axis(2, seq(0,1,0.01), las = 2)
axis(1, seq(0,100,10))

xb<-mean(dum$RH_perc, na.rm = T); xb<-format(round(xb,1), nsmall = 1)
bo<-quantile(dum$RH_perc, seq(0,1,0.05))[2]; bo<-format(round(bo,1), nsmall = 1)
to<-quantile(dum$RH_perc, seq(0,1,0.05))[20]; to<-format(round(to,1), nsmall = 1)

abline(v = xb, lty = 1, lwd = 2, col = rgb(0,0,1, alpha = 0.5))
abline(v = bo, lty = 2, lwd = 1, col = rgb(0,0,1, alpha = 0.5))
abline(v = to, lty = 2, lwd = 1, col = rgb(0,0,1, alpha = 0.5))
points(x$y~x$x, type = "l", col = "blue", lwd = 2)
x<-data.frame(x$x,x$y)
dum<-subset(x, x$x.x < as.numeric(to) & x$x.x > as.numeric(bo))
points(dum$x.y~dum$x.x, type = "h", col = rgb(0,0,1, alpha = 0.2), lwd = 2)

dum<-subset(night, Treatment == "Warm")
x<-density(dum$RH_perc)
x$y<-(x$y)/sum(x$y)
xb<-mean(dum$RH_perc, na.rm = T); xb<-format(round(xb,1), nsmall = 1)
bo<-quantile(dum$RH_perc, seq(0,1,0.05))[2]; bo<-format(round(bo,1), nsmall = 1)
to<-quantile(dum$RH_perc, seq(0,1,0.05))[20]; to<-format(round(to,1), nsmall = 1)

abline(v = xb, lty = 1, lwd = 2, col = rgb(1,0,0, alpha = 0.5))
abline(v = bo, lty = 2, lwd = 1, col = rgb(1,0,0, alpha = 0.5))
abline(v = to, lty = 2, lwd = 1, col = rgb(1,0,0, alpha = 0.5))
points(x$y~x$x, type = "l", col = "firebrick", lwd = 2)
x<-data.frame(x$x,x$y)
dum<-subset(x, x$x.x < as.numeric(to) & x$x.x > as.numeric(bo))
points(dum$x.y~dum$x.x, type = "h", col = rgb(1,0,0, alpha = 0.2), lwd = 2)

mtext(side = 1, expression(RH~('%')), padj = 1.6, adj = -0.25, cex = 1.5)
legend("topleft", "d)", bty = "n", cex = 1.2)

# e) day vpd ####

dum<-subset(day, Treatment == "Ambi")
x<-density(dum$VPD_kPa)
x$y<-(x$y)/sum(x$y)
plot(x$y~x$x, type = "l", col = "white", xlim = c(0,7), ylim = c(0,0.01), xlab="",ylab="",xaxt="n",yaxt="n")
axis(2, seq(0,0.01,0.0025), las = 2)
axis(1, seq(0,100,1))

xb<-mean(dum$VPD_kPa, na.rm = T); xb<-format(round(xb,1), nsmall = 1)
bo<-quantile(dum$VPD_kPa, seq(0,1,0.05))[2]; bo<-format(round(bo,1), nsmall = 1)
to<-quantile(dum$VPD_kPa, seq(0,1,0.05))[20]; to<-format(round(to,1), nsmall = 1)

abline(v = xb, lty = 1, lwd = 2, col = rgb(0,0,1, alpha = 0.5))
abline(v = bo, lty = 2, lwd = 1, col = rgb(0,0,1, alpha = 0.5))
abline(v = to, lty = 2, lwd = 1, col = rgb(0,0,1, alpha = 0.5))
points(x$y~x$x, type = "l", col = "blue", lwd = 2)
x<-data.frame(x$x,x$y)
dum<-subset(x, x$x.x < to & x$x.x > bo)
points(dum$x.y~dum$x.x, type = "h", col = rgb(0,0,1, alpha = 0.2), lwd = 2)

dum<-subset(day, Treatment == "Warm")
x<-density(dum$VPD_kPa)
x$y<-(x$y)/sum(x$y)
xb<-mean(dum$VPD_kPa, na.rm = T); xb<-format(round(xb,1), nsmall = 1)
bo<-quantile(dum$VPD_kPa, seq(0,1,0.05))[2]; bo<-format(round(bo,1), nsmall = 1)
to<-quantile(dum$VPD_kPa, seq(0,1,0.05))[20]; to<-format(round(to,1), nsmall = 1)

abline(v = xb, lty = 1, lwd = 2, col = rgb(1,0,0, alpha = 0.5))
abline(v = bo, lty = 2, lwd = 1, col = rgb(1,0,0, alpha = 0.5))
abline(v = to, lty = 2, lwd = 1, col = rgb(1,0,0, alpha = 0.5))
points(x$y~x$x, type = "l", col = "firebrick", lwd = 2)
x<-data.frame(x$x,x$y)
dum<-subset(x, x$x.x < to & x$x.x > bo)
points(dum$x.y~dum$x.x, type = "h", col = rgb(1,0,0, alpha = 0.2), lwd = 2)

legend("topright", "e)", bty = "n", cex = 1.2)

# f) night vpd ####

dum<-subset(night, Treatment == "Ambi")
x<-density(dum$VPD_kPa)
x$y<-(x$y)/sum(x$y)
plot(x$y~x$x, type = "l", col = "white", xlim = c(0,1), ylim = c(0,0.04), xlab="",ylab="",xaxt="n",yaxt="n")
axis(2, at = seq(0,1,0.01), las = 2)
axis(1, seq(0,100,0.2))

xb<-mean(dum$VPD_kPa, na.rm = T); xb<-format(round(xb,1), nsmall = 1)
bo<-quantile(dum$VPD_kPa, seq(0,1,0.05))[2]; bo<-format(round(bo,1), nsmall = 1)
to<-quantile(dum$VPD_kPa, seq(0,1,0.05))[20]; to<-format(round(to,1), nsmall = 1)

abline(v = xb, lty = 1, lwd = 2, col = rgb(0,0,1, alpha = 0.5))
abline(v = bo, lty = 2, lwd = 1, col = rgb(0,0,1, alpha = 0.5))
abline(v = to, lty = 2, lwd = 1, col = rgb(0,0,1, alpha = 0.5))
points(x$y~x$x, type = "l", col = "blue", lwd = 2)
x<-data.frame(x$x,x$y)
dum<-subset(x, x$x.x < to & x$x.x > bo)
points(dum$x.y~dum$x.x, type = "h", col = rgb(0,0,1, alpha = 0.2), lwd = 2)

dum<-subset(night, Treatment == "Warm")
x<-density(dum$VPD_kPa)
x$y<-(x$y)/sum(x$y)
xb<-mean(dum$VPD_kPa, na.rm = T); xb<-format(round(xb,1), nsmall = 1)
bo<-quantile(dum$VPD_kPa, seq(0,1,0.05))[2]; bo<-format(round(bo,1), nsmall = 1)
to<-quantile(dum$VPD_kPa, seq(0,1,0.05))[20]; to<-format(round(to,1), nsmall = 1)

abline(v = xb, lty = 1, lwd = 2, col = rgb(1,0,0, alpha = 0.5))
abline(v = bo, lty = 2, lwd = 1, col = rgb(1,0,0, alpha = 0.5))
abline(v = to, lty = 2, lwd = 1, col = rgb(1,0,0, alpha = 0.5))
points(x$y~x$x, type = "l", col = "firebrick", lwd = 2)
x<-data.frame(x$x,x$y)
dum<-subset(x, x$x.x < to & x$x.x > bo)
points(dum$x.y~dum$x.x, type = "h", col = rgb(1,0,0, alpha = 0.2), lwd = 2)

legend("topright", "f)", bty = "n", cex = 1.2)
mtext(side = 1, expression(italic(VPD)~(kPa)), padj = 1.6, adj = -0.5, cex = 1.5)

# off ####
mtext(side = 2, "Density of Readings", outer = T, padj = -2.5, cex = 1.5)
dev.off(); 1+1