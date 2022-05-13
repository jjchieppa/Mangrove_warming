# in data ####

library(doBy); library(lubridate); library(car)
library(effects); library(sjPlot); library(Metrics)
library(plotrix)

rm(list=ls())
dev.off()
cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

setwd("C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis")
source<-read.csv("Mangrove FlVBZ ins R data (no freeze).csv")

# start ####

# tiff(file = "V_Mangrove Acclim_homeo by seasonal warming (four panel).tiff", height = 8, width = 12, res = 600, units = "in", compression = "zip+p")

par(mfrow = c(2,2), omi = c(1,0.7,0.2,0.01), mar = c(0.5,1.5,0.5,1.5))

# Rarea ag ####

plot(Hom_T5.sd ~ Hom_T5.sd_Tdiff, source, xlim = c(-0.2,7.3), ylim = c(0,2.5), pch = NA, xaxt="n",yaxt="n")
axis(1, seq(0,7,1), labels = F)
axis(2, seq(0,5,0.5), las = 2, cex = 1.2)
abline(h = 1, lty = 2, col = "grey50")
legend("topleft", bty = "n", "a)", cex = 1.2)

df<-subset(source, R_calc == "R.area" & Species == "ag" & Source == "fl" & trt == "a")
points(Hom_T5.sd ~ Hom_T5.sd_Tdiff, df, pch = 2, col = "dodgerblue4")
df<-subset(source, R_calc == "R.area" & Species == "ag" & Source == "fl" & trt == "w")
points(Hom_T5.sd ~ Hom_T5.sd_Tdiff, df, pch = 2, col = "firebrick")
df<-subset(source, R_calc == "R.area" & Species == "ag" & Source == "bz" & trt == "a")
points(Hom_T5.sd ~ Hom_T5.sd_Tdiff, df, pch = 6, col = "dodgerblue4")
df<-subset(source, R_calc == "R.area" & Species == "ag" & Source == "bz" & trt == "w")
points(Hom_T5.sd ~ Hom_T5.sd_Tdiff, df, pch = 6, col = "firebrick")

df<-subset(source, R_calc == "R.area" & Species == "ag")
m1<-lm(Hom_T5.sd ~ Hom_T5.sd_Tdiff * Source * trt, df); Anova(m1)
summary(m1)
legend("top", bty = "n", expression(italic(R)^2~'= 0.08;'~italic(P)~'= 0.37'), cex = 1.2)

mtext(side = 3, expression(italic(Avicennia)))
mtext(side = 2, expression(Acclim[Homeo]~italic(R)[area]), cex = 1.2, padj = -2)

legend("topright",pch=c(2,6,15,15),col=c("black","black","firebrick","dodgerblue4"),
       c("Subtropical","Tropical","Ambient","Warmed"), bty = "n", cex = 1.3)

# Rarea ag ####

plot(Hom_T5.sd ~ Hom_T5.sd_Tdiff, source, xlim = c(-0.2,7.3), ylim = c(0,2.5), pch = NA, xaxt="n",yaxt="n")
axis(1, seq(0,7,1), labels = F)
axis(2, seq(0,5,0.5), las = 2, cex = 1.2)
abline(h = 1, lty = 2, col = "grey50")
legend("topleft", bty = "n", "b)", cex = 1.2)

df<-subset(source, R_calc == "R.area" & Species == "rm" & Source == "fl" & trt == "a")
points(Hom_T5.sd ~ Hom_T5.sd_Tdiff, df, pch = 2, col = "dodgerblue4")
df<-subset(source, R_calc == "R.area" & Species == "rm" & Source == "fl" & trt == "w")
points(Hom_T5.sd ~ Hom_T5.sd_Tdiff, df, pch = 2, col = "firebrick")
df<-subset(source, R_calc == "R.area" & Species == "rm" & Source == "bz" & trt == "a")
points(Hom_T5.sd ~ Hom_T5.sd_Tdiff, df, pch = 6, col = "dodgerblue4")
df<-subset(source, R_calc == "R.area" & Species == "rm" & Source == "bz" & trt == "w")
points(Hom_T5.sd ~ Hom_T5.sd_Tdiff, df, pch = 6, col = "firebrick")

df<-subset(source, R_calc == "R.area" & Species == "rm")
m1<-lm(Hom_T5.sd ~ Hom_T5.sd_Tdiff + Source + trt + Hom_T5.sd_Tdiff:Source + Hom_T5.sd_Tdiff:trt + Hom_T5.sd_Tdiff:Source:trt, df); Anova(m1)
summary(m1)
legend("top", bty = "n", expression(italic(R)^2~'= 0.28;'~italic(P)~'< 0.01'), cex = 1.2)

m1<-lm(Hom_T5.sd ~ Hom_T5.sd_Tdiff + Source, df)
x<-plot_model(m1, type = c("pred"), terms = c("Hom_T5.sd_Tdiff", "Source")); new<-as.data.frame(x$data)
dum<-subset(new, group == "bz")
df<-subset(source, R_calc == "R.area" & Species == "rm" & Source == "bz")
ln<-lm(predicted~x,dum);ablineclip(ln, x1=min(df$Hom_T5.sd_Tdiff,na.rm=T),x2=max(df$Hom_T5.sd_Tdiff,na.rm=T),lwd=2,lty=2,col="black")
dum<-subset(new, group == "fl")
df<-subset(source, R_calc == "R.area" & Species == "rm" & Source == "fl")
ln<-lm(predicted~x,dum);ablineclip(ln, x1=min(df$Hom_T5.sd_Tdiff,na.rm=T),x2=max(df$Hom_T5.sd_Tdiff,na.rm=T),lwd=2,lty=1,col="black")

m1<-lm(Hom_T5.sd ~ Hom_T5.sd_Tdiff + trt, df)
x<-plot_model(m1, type = c("pred"), terms = c("Hom_T5.sd_Tdiff", "trt")); new<-as.data.frame(x$data)
dum<-subset(new, group == "a")
df<-subset(source, R_calc == "R.area" & Species == "rm" & trt == "a")
ln<-lm(predicted~x,dum);ablineclip(ln, x1=min(df$Hom_T5.sd_Tdiff,na.rm=T),x2=max(df$Hom_T5.sd_Tdiff,na.rm=T),lwd=2,lty=1,col="dodgerblue4")
dum<-subset(new, group == "w")
df<-subset(source, R_calc == "R.area" & Species == "rm" & trt == "w")
ln<-lm(predicted~x,dum);ablineclip(ln, x1=min(df$Hom_T5.sd_Tdiff,na.rm=T),x2=max(df$Hom_T5.sd_Tdiff,na.rm=T),lwd=2,lty=1,col="firebrick")
# m1<-lm(Hom_T5.sd ~ Hom_T5.sd_Tdiff + trt, df)

mtext(side = 3, expression(italic(Rhizophora)))

legend("topright", bty = "n", lty = c(1,1,1,2), col = c("dodgerblue4","firebrick","black","black"), lwd = 2,
       c("Ambient","Warmed","Subtropical","Tropical"), cex = 1.3)

# R.mass ag ####

plot(Hom_T5.sd ~ Hom_T5.sd_Tdiff, source, xlim = c(-0.2,7.3), ylim = c(0,4), pch = NA, xaxt="n",yaxt="n")
axis(1, seq(0,7,1), cex.axis = 1.2)
axis(2, seq(0,5,1), las = 2, cex = 1.2)
abline(h = 1, lty = 2, col = "grey50")
legend("topleft", bty = "n", "c)", cex = 1.2)

df<-subset(source, R_calc == "R.mass" & Species == "ag" & Source == "fl" & trt == "a")
points(Hom_T5.sd ~ Hom_T5.sd_Tdiff, df, pch = 2, col = "dodgerblue4")
df<-subset(source, R_calc == "R.mass" & Species == "ag" & Source == "fl" & trt == "w")
points(Hom_T5.sd ~ Hom_T5.sd_Tdiff, df, pch = 2, col = "firebrick")
df<-subset(source, R_calc == "R.mass" & Species == "ag" & Source == "bz" & trt == "a")
points(Hom_T5.sd ~ Hom_T5.sd_Tdiff, df, pch = 6, col = "dodgerblue4")
df<-subset(source, R_calc == "R.mass" & Species == "ag" & Source == "bz" & trt == "w")
points(Hom_T5.sd ~ Hom_T5.sd_Tdiff, df, pch = 6, col = "firebrick")

df<-subset(source, R_calc == "R.mass" & Species == "ag")
m1<-lm(Hom_T5.sd ~ Hom_T5.sd_Tdiff * Source * trt, df); Anova(m1)
summary(m1)
legend("top", bty = "n", expression(italic(R)^2~'= 0.03;'~italic(P)~'= 0.92'), cex = 1.2)

mtext(side = 2, expression(Acclim[Homeo]~italic(R)[mass]), cex = 1.2, padj = -2)

# R.mass ag ####

plot(Hom_T5.sd ~ Hom_T5.sd_Tdiff, source, xlim = c(-0.2,7.3), ylim = c(0,4), pch = NA, xaxt="n",yaxt="n")
axis(1, seq(0,7,1), cex.axis = 1.2)
axis(2, seq(0,5,1), las = 2, cex = 1.2)
abline(h = 1, lty = 2, col = "grey50")
legend("topleft", bty = "n", "d)", cex = 1.2)

df<-subset(source, R_calc == "R.mass" & Species == "rm" & Source == "fl" & trt == "a")
points(Hom_T5.sd ~ Hom_T5.sd_Tdiff, df, pch = 2, col = "dodgerblue4")
df<-subset(source, R_calc == "R.mass" & Species == "rm" & Source == "fl" & trt == "w")
points(Hom_T5.sd ~ Hom_T5.sd_Tdiff, df, pch = 2, col = "firebrick")
df<-subset(source, R_calc == "R.mass" & Species == "rm" & Source == "bz" & trt == "a")
points(Hom_T5.sd ~ Hom_T5.sd_Tdiff, df, pch = 6, col = "dodgerblue4")
df<-subset(source, R_calc == "R.mass" & Species == "rm" & Source == "bz" & trt == "w")
points(Hom_T5.sd ~ Hom_T5.sd_Tdiff, df, pch = 6, col = "firebrick")

df<-subset(source, R_calc == "R.mass" & Species == "rm")
m1<-lm(Hom_T5.sd ~ Hom_T5.sd_Tdiff + Source + trt + Hom_T5.sd_Tdiff:Source + Hom_T5.sd_Tdiff:trt + Hom_T5.sd_Tdiff:Source:trt, df); Anova(m1)
summary(m1)
legend("top", bty = "n", expression(italic(R)^2~'= 0.21;'~italic(P)~'< 0.05'), cex = 1.2)

m1<-lm(Hom_T5.sd ~ Hom_T5.sd_Tdiff + trt, df); Anova(m1)
x<-plot_model(m1, type = c("pred"), terms = c("Hom_T5.sd_Tdiff","trt")); new<-as.data.frame(x$data)
dum<-subset(new, group == "a")
df<-subset(source, R_calc == "R.mass" & Species == "rm" & trt == "a")
ln<-lm(predicted~x,dum);ablineclip(ln, x1=min(df$Hom_T5.sd_Tdiff,na.rm=T),x2=max(df$Hom_T5.sd_Tdiff,na.rm=T),lwd=2,col="dodgerblue4")
dum<-subset(new, group == "w")
df<-subset(source, R_calc == "R.mass" & Species == "rm" & trt == "w")
ln<-lm(predicted~x,dum);ablineclip(ln, x1=min(df$Hom_T5.sd_Tdiff,na.rm=T),x2=max(df$Hom_T5.sd_Tdiff,na.rm=T),lwd=2,col="firebrick")

legend("topright", bty = "n", lty = c(1,1), col = c("dodgerblue4","firebrick"), lwd = 2,
       c("Ambient","Warmed"), cex = 1.3)

# off ####
mtext(side = 1, outer = T, expression(Seasonal~Warming~(degree*C)), cex = 1.2, padj = 2)

# dev.off()
