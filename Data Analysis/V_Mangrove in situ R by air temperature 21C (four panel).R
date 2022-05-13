# in data ####

library(doBy); library(lubridate); library(car)
library(effects); library(sjPlot); library(Metrics)

rm(list=ls())
dev.off()
cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

setwd("C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis")
source<-read.csv("Mangrove FlVBZ ins R data (no freeze).csv")

# start ####

tiff(file = "V_Mangrove in situ R by air temperature 21C (four panel).tiff", height = 8, width = 12, res = 600, units = "in", compression = "zip+p")

par(mfrow = c(2,2), omi = c(1,0.7,0.2,0.01), mar = c(0.5,1,0.5,1))

# Rarea AG ####

df<-subset(source, R_calc == "R.area" & Species == "ag")
plot(1~0, xlim = c(20,30), ylim = c(0, 3), xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, seq(20,30,2), labels = F)
axis(2, seq(0,3,0.5), cex.axis = 1.2, las = 2)

dat<-subset(df, trt == "a" & Source == "fl")
points(dat$Rins ~ dat$Temp_C.mean, cex = 1.2, pch = 2, col = "dodgerblue4")
dat<-subset(df, trt == "w" & Source == "fl")
points(dat$Rins ~ dat$Temp_C.mean, cex = 1.2, pch = 2, col = "firebrick")
dat<-subset(df, trt == "a" & Source == "bz")
points(dat$Rins ~ dat$Temp_C.mean, cex = 1.2, pch = 6, col = "dodgerblue4")
dat<-subset(df, trt == "w" & Source == "bz")
points(dat$Rins ~ dat$Temp_C.mean, cex = 1.2, pch = 6, col = "firebrick")

K<-273.15; df$alpha<-df$Temp_C.mean+K; df$beta<-K+25
q10 <- function(x) {1.41435 * (1.41738) ^ (((x+273.15)-294.15)/10)}
points(q10(seq(20,30,1)) ~ seq(20,30,1), type = "l", lwd = 2, col = "grey50")

legend("topleft", "a)", bty = "n")
legend("bottomleft", bty = "n", 
       expression(italic(R)^2*' = 0.13; '
                  *italic(P)~'< 0.001;'~
                    italic(Q)[10]~'= 1.42;'~
                    italic(R)[22]~'= 1.41'), cex = 1.2)

mtext(side = 3, expression(italic(Avicennia)))
mtext(side = 2, expression(italic('in situ R'[area])~(mu*mol~m^-2~s^-1)), padj = -2, cex = 1.2)

legend("bottomright",pch=c(2,6,15,15),col=c("black","black","firebrick","dodgerblue4"),
       c("Subtropical","Tropical","Ambient","Warmed"), bty = "n", cex = 1.3)

# Rarea RM ####

df<-subset(source, R_calc == "R.area" & Species == "rm")
plot(1~0, xlim = c(20,30), ylim = c(0, 3), xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, seq(20,30,2), labels = F)
axis(2, seq(0,3,0.5), labels = F)

dat<-subset(df, trt == "a" & Source == "fl")
points(dat$Rins ~ dat$Temp_C.mean, pch = 2, col = "dodgerblue4")
dat<-subset(df, trt == "w" & Source == "fl")
points(dat$Rins ~ dat$Temp_C.mean, pch = 2, col = "firebrick")
dat<-subset(df, trt == "a" & Source == "bz")
points(dat$Rins ~ dat$Temp_C.mean, pch = 6, col = "dodgerblue4")
dat<-subset(df, trt == "w" & Source == "bz")
points(dat$Rins ~ dat$Temp_C.mean, pch = 6, col = "firebrick")

K<-273.15; df$alpha<-df$Temp_C.mean+K; df$beta<-K+25
q10 <- function(x) {0.99804 * (1.74861) ^ (((x+273.15)-294.15)/10)}
points(q10(seq(20,30,1)) ~ seq(20,30,1), type = "l", lwd = 2, col = "grey50")

legend("topleft", "b)", bty = "n")
legend("bottom", bty = "n", 
       expression(italic(R)^2*' = 0.20; '
                  *italic(P)~'< 0.001;'~
                    italic(Q)[10]~'= 1.75;'~
                    italic(R)[21]~'= 1.00'), cex = 1.2)

mtext(side = 3, expression(italic(Rhizophora)))

# Rmass AG ####

df<-subset(source, R_calc == "R.mass" & Species == "ag")
plot(1~0, xlim = c(20,30), ylim = c(0, 32), xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, seq(20,30,2), cex.axis = 1.2)
axis(2, seq(0,30,5), cex.axis = 1.2, las = 2)

dat<-subset(df, trt == "a" & Source == "fl")
points(dat$Rins ~ dat$Temp_C.mean, cex = 1.2, pch = 2, col = "dodgerblue4")
dat<-subset(df, trt == "w" & Source == "fl")
points(dat$Rins ~ dat$Temp_C.mean, cex = 1.2, pch = 2, col = "firebrick")
dat<-subset(df, trt == "a" & Source == "bz")
points(dat$Rins ~ dat$Temp_C.mean, cex = 1.2, pch = 6, col = "dodgerblue4")
dat<-subset(df, trt == "w" & Source == "bz")
points(dat$Rins ~ dat$Temp_C.mean, cex = 1.2, pch = 6, col = "firebrick")

K<-273.15; df$alpha<-df$Temp_C.mean+K; df$beta<-K+25
q10 <- function(x) {11.82 * (1.4385) ^ (((x+273.15)-294.15)/10)}
points(q10(seq(20,30,1)) ~ seq(20,30,1), type = "l", lwd = 2, col = "grey50")

legend("topleft", "c)", bty = "n")
legend("bottom", bty = "n", 
       expression(italic(R)^2*' = 0.06; '
                  *italic(P)~'< 0.01;'~
                    italic(Q)[10]~'= 1.44;'~
                    italic(R)[21]~'= 11.82'), cex = 1.2)

mtext(side = 2, expression(italic('in situ R'[mass])~(nmol~g^-1~s^-1)), padj = -2, cex = 1.2)

# Rmass RM ####

df<-subset(source, R_calc == "R.mass" & Species == "rm")
plot(1~0, xlim = c(20,30), ylim = c(0, 32), xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, seq(20,30,2), cex = 1.2)
axis(2, seq(0,30,5), labels = F)

dat<-subset(df, trt == "a" & Source == "fl")
points(dat$Rins ~ dat$Temp_C.mean, cex = 1.2, pch = 2, col = "dodgerblue4")
dat<-subset(df, trt == "w" & Source == "fl")
points(dat$Rins ~ dat$Temp_C.mean, cex = 1.2, pch = 2, col = "firebrick")
dat<-subset(df, trt == "a" & Source == "bz")
points(dat$Rins ~ dat$Temp_C.mean, cex = 1.2, pch = 6, col = "dodgerblue4")
dat<-subset(df, trt == "w" & Source == "bz")
points(dat$Rins ~ dat$Temp_C.mean, cex = 1.2, pch = 6, col = "firebrick")

K<-273.15; df$alpha<-df$Temp_C.mean+K; df$beta<-K+25
q10 <- function(x) {6.6155 * (2.0935) ^ (((x+273.15)-294.15)/10)}
points(q10(seq(20,30,1)) ~ seq(20,30,1), type = "l", lwd = 2, col = "grey50")

legend("topleft", "d)", bty = "n")
legend("bottom", bty = "n", 
       expression(italic(R)^2*' = 0.19; '
                  *italic(P)~'< 0.001;'~
                    italic(Q)[10]~'= 2.09;'~
                    italic(R)[21]~'= 6.62'), cex = 1.2)

# off ####
mtext(side = 1, outer = T, expression(italic(T)[5]~(degree*C)), cex = 1.5, padj = 2)
dev.off()