# in data ####

library(doBy); library(lubridate); library(car)
library(effects); library(sjPlot); library(Metrics)
library(plotrix)

rm(list=ls())
dev.off()
cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

setwd("C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis")
source<-read.csv("Mangrove FlVBZ ins R pairs (no freeze).csv")

# start ####

tiff(file = "V_Mangrove Acclim_homeo by warming paired (four panel).tiff", height = 8, width = 12, res = 600, units = "in", compression = "zip+p")

par(mfrow = c(2,2), omi = c(1,0.7,0.2,0.01), mar = c(0.5,1,0.5,1))

# Rarea ag ####

plot(AH ~ Warming, source, xlim = c(0,2.5), ylim = c(0.4,2.2), pch = NA, xaxt="n",yaxt="n")
axis(1, seq(0,2.5,0.5), labels = F)
axis(2, seq(0.5,2,0.5), las = 2, cex = 1.2)
abline(h = 1, lty = 2, col = "grey50")
legend("topleft", bty = "n", "a)", cex = 1.2)

df<-subset(source, R_calc == "R.area" & Species == "ag" & Source == "fl")
points(AH ~ Warming, df, pch = 17, cex = 1.4)
df<-subset(source, R_calc == "R.area" & Species == "ag" & Source == "bz")
points(AH ~ Warming, df, pch = 25, cex = 1.4, bg = "black")

df<-subset(source, R_calc == "R.area" & Species == "ag")
m1<-lm(AH ~ Warming * Source, df); Anova(m1)
summary(m1)
legend("top", bty = "n", expression(italic(R)^2~'= 0.43;'~italic(P)~'= 0.19'), cex = 1.2)

mtext(side = 3, expression(italic(Avicennia)))
mtext(side = 2, expression(Acclim[Homeo]~italic(R)[area]), cex = 1.2, padj = -2)

legend("topright",pch=c(17,25),col=c("black","black"), pt.bg = "black",
       c("Subtropical","Tropical"), bty = "n", cex = 1.4)

# Rarea rm ####

plot(AH ~ Warming, source, xlim = c(0,2.5), ylim = c(0.4,2.2), pch = NA, xaxt="n",yaxt="n")
axis(1, seq(0,2.5,0.5), labels = F)
axis(2, seq(0.5,2,0.5), labels = F)
abline(h = 1, lty = 2, col = "grey50")
legend("topleft", bty = "n", "b)", cex = 1.2)

df<-subset(source, R_calc == "R.area" & Species == "rm" & Source == "fl")
points(AH ~ Warming, df, pch = 17, cex = 1.4)
df<-subset(source, R_calc == "R.area" & Species == "rm" & Source == "bz")
points(AH ~ Warming, df, pch = 25, cex = 1.4, bg = "black")

df<-subset(source, R_calc == "R.area" & Species == "rm")
m1<-lm(AH ~ Warming * Source, df); Anova(m1)
summary(m1)
legend("top", bty = "n", expression(italic(R)^2~'= 0.57;'~italic(P)~'= 0.02'), cex = 1.2)
x<-plot_model(m1, type = c("pred"), terms = c("Warming")); new<-as.data.frame(x$data)
ln<-lm(predicted~x,new);ablineclip(ln, x1=min(df$Warming,na.rm=T),x2=max(df$Warming,na.rm=T),lwd=2,col="grey50")

mtext(side = 3, expression(italic(Rhizophora)))

# Rmass ag ####

plot(AH ~ Warming, source, xlim = c(0,2.5), ylim = c(0.4,2.2), pch = NA, xaxt="n",yaxt="n")
axis(1, seq(0,2.5,0.5), cex.axis = 1.2)
axis(2, seq(0.5,2,0.5), las = 2, cex = 1.2)
abline(h = 1, lty = 2, col = "grey50")
legend("topleft", bty = "n", "c)", cex = 1.2)

df<-subset(source, R_calc == "R.mass" & Species == "ag" & Source == "fl")
points(AH ~ Warming, df, pch = 17, cex = 1.4)
df<-subset(source, R_calc == "R.mass" & Species == "ag" & Source == "bz")
points(AH ~ Warming, df, pch = 25, cex = 1.4, bg = "black")

df<-subset(source, R_calc == "R.mass" & Species == "ag")
m1<-lm(AH ~ Warming * Source, df); Anova(m1)
summary(m1)
legend("top", bty = "n", expression(italic(R)^2~'= 0.31;'~italic(P)~'= 0.37'), cex = 1.2)

mtext(side = 2, expression(Acclim[Homeo]~italic(R)[mass]), cex = 1.2, padj = -2)

# Rmass rm ####

plot(AH ~ Warming, source, xlim = c(0,2.5), ylim = c(0.4,2.2), pch = NA, xaxt="n",yaxt="n")
axis(1, seq(0,2.5,0.5), cex.axis = 1.2)
axis(2, seq(0.5,2,0.5), labels = F)
abline(h = 1, lty = 2, col = "grey50")
legend("topleft", bty = "n", "d)", cex = 1.2)

df<-subset(source, R_calc == "R.mass" & Species == "rm" & Source == "fl")
points(AH ~ Warming, df, pch = 17, cex = 1.4)
df<-subset(source, R_calc == "R.mass" & Species == "rm" & Source == "bz")
points(AH ~ Warming, df, pch = 25, cex = 1.4, bg = "black")

df<-subset(source, R_calc == "R.mass" & Species == "rm")
m1<-lm(AH ~ Warming * Source, df); Anova(m1)
summary(m1)
legend("topright", bty = "n", expression(italic(R)^2~'= 0.61;'~italic(P)~'= 0.03'), cex = 1.2)
x<-plot_model(m1, type = c("pred"), terms = c("Warming")); new<-as.data.frame(x$data)
ln<-lm(predicted~x,new);ablineclip(ln, x1=min(df$Warming,na.rm=T),x2=max(df$Warming,na.rm=T),lwd=2,col="grey50")

# off ####
mtext(side = 1, outer = T, expression(Warming~(degree*C)), cex = 1.2, padj = 2)

dev.off()