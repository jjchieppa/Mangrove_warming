# start ####

library(lattice);library(doBy); library(lubridate)
library(minpack.lm); library(effects); library(plotrix)
library(car); library(sjPlot)

rm(list=ls())
dev.off()
cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

home<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis"
work<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp" # work
raw<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/raw"
hist<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/hist"
lins<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/lins"
deets<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/deets"
env<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Weather Data"
anly<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/Analysis"

# data in ####

### Bring in the ref respiration data, bind area and mass for LMA

setwd(deets)
source<-read.csv("MangroveRvT_LPRmod_Rarea and Rmass at 25C.csv")
source<-subset(source, KEEP == "Y")
source$Date<-as.Date(source$Date, format = "%m/%d/%Y")
setwd(work)
trts<-read.csv("Mangrove Treatment Assignments.csv")
source<-merge(source, trts, by = "UserIDs_in", all = F)
rm(trts)

source$Date<-as.Date(source$Date, format = "%Y-%m-%d") # Double checks date format!
source$MoYr<-floor_date(source$Date, "month")
source$MoYr<-source$MoYr+25
source$LMA_g.m2<-source$LMA_kg.m2*1000
source$Timepoint<-as.factor(source$MoYr)

# env data in ####
# Bring in temperature data

setwd(env)

env<-read.csv("Mangrove RVT weather data_all.csv")
env$Date<-as.Date(env$Date, format = "%m/%d/%Y")
df<-source
df$sDate<-as.factor(df$Date)
env$sDate<-as.factor(env$Date)
reps<-unique(df$sDate)
new_env<-subset(env, Block == "")
for (reps in unique(df$sDate)){
  dat<-subset(env, env$sDate == reps)
  mnd<-min(dat$Date, na.rm = T)
  
  dat1<-subset(env, env$Date == mnd-1)
  dat2<-subset(env, env$Date == mnd-2)
  dat3<-subset(env, env$Date == mnd-3)
  dat4<-subset(env, env$Date == mnd-4)
  dat5<-subset(env, env$Date == mnd-5)
  
  newdat<-rbind(dat1,dat2,dat3,dat4,dat5)
  newdat$Date<-max(newdat$Date, na.rm = T)+1
  
  new_env<-rbind(newdat, new_env)
  rm(dat, mnd, dat1, dat2, dat3, dat4, dat5)
}
rm(newdat)

warm<-subset(new_env, Treatment == "Warm")
ambi<-subset(new_env, Treatment == "Ambi")
rm(new_env, env)

warm<-summaryBy(Temp_C ~ Date, FUN = mean, na.rm = T, warm)
ambi<-summaryBy(Temp_C ~ Date, FUN = mean, na.rm = T, ambi)
warm$Treatment<-"Warmed"
ambi$Treatment<-"Ambient"

temp<-rbind(warm, ambi)
rm(warm, ambi)

source<-merge(temp, source, by = c("Date","Treatment"))
rm(temp, df)

source$Qins<-exp(10*(source$CTleaf.slope+(2*source$CTleaf2.slope*source$Temp_C.mean)))
source$Rins<-exp(source$intercept + (source$CTleaf.slope * source$Temp_C.mean) + (source$CTleaf2.slope * source$Temp_C.mean^2))

start<-as.Date("2020-04-15")
end<-as.Date("2020-12-27")



# start plot ####

setwd("C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis")

tiff(file = "V_Mangrove Physiology (ins) ~ 5DayTemp (both species).tiff", height = 8, width = 7, res = 600, units = "in", compression = "zip+p")

par(mfrow = c(3,2), omi = c(0.75,0.75,0.1,0.01), mar = c(0.8,1.7,0.8,1.7))

# Rarea AG ####

df<-subset(source, Species == "ag" & R_calc == "R.area")
plot(df$Rins ~ df$Temp_C.mean, xlim = c(10,30), ylim = c(0.25,3.5), pch = NA, xaxt="n",yaxt="n",xlab="",ylab="",xaxs="i")
axis(1, seq(10,30,5), labels = F)
axis(2, seq(0.5,3.5,1), las = 2, cex.axis = 1.5)

dat<-subset(df, Source == "bz" & Treatment == "Ambient")
points(dat$Rins ~ dat$Temp_C.mean, pch = 6, col = "blue")
dat<-subset(df, Source == "bz" & Treatment == "Warmed")
points(dat$Rins ~ dat$Temp_C.mean, pch = 6, col = "firebrick")
dat<-subset(df, Source == "fl" & Treatment == "Ambient")
points(dat$Rins ~ dat$Temp_C.mean, pch = 2, col = "blue")
dat<-subset(df, Source == "fl" & Treatment == "Warmed")
points(dat$Rins ~ dat$Temp_C.mean, pch = 2, col = "firebrick")

mtext(side = 3, expression(italic(Avicennia)))

legend("topleft", "a)", bty = "n", cex = 1.2)

m1<-lm(Rins ~ Temp_C.mean * Source * Treatment, df)
Anova(m1); summary(m1)
x<-plot_model(m1, type = c("pred"), terms = c("Temp_C.mean"))
newdata<-as.data.frame(x$data)
points(predicted ~ x, newdata, lty = 1, type = "l", lwd = 2, col = "grey50")
legend("bottom", expression(italic(R)^2~' = 0.57,'~italic(P)~' < 0.001'), bty = "n", cex = 1.3)
text(16, 1.8, "All", cex = 1.3)
arrows(16, 1.6, 16, 1.2, length = 0.1, lwd = 2)

dat<-subset(df, Temp_C.mean > 16)
m1<-lm(Rins ~ Temp_C.mean * Source * Treatment, dat)
Anova(m1); summary(m1)
x<-plot_model(m1, type = c("pred"), terms = c("Temp_C.mean"))
newdata<-as.data.frame(x$data)
points(predicted ~ x, newdata, lty = 1, type = "l", lwd = 3)
legend("topright", expression(italic(R)^2~' = 0.18,'~italic(P)~' < 0.001'), bty = "n", cex = 1.3)
text(18, 2.2, "Pre-Frost", cex = 1.2)
arrows(17.5, 2.0, 19.5, 1.9, length = 0.1, lwd = 2)

# legend("top", c("Temperature***","Treatment","Origin"), horiz = T, bty = "n", cex = 1.2)
# legend("bottom", c("Te x Tr", "Te x O", "Tr x O", "Te x Tr x O"), horiz = T, bty = "n", cex = 1.2)

mtext(side = 2, expression(italic(R)[area]*''^italic('in situ')~(mu*mol~m^-2~s^-1)), cex = 1, padj = -2.5)

# Rarea RM ####

df<-subset(source, Species == "rm" & R_calc == "R.area")
plot(df$Rins ~ df$Temp_C.mean, xlim = c(10,30), ylim = c(0.25,3.5), pch = NA, xaxt="n",yaxt="n",xlab="",ylab="",xaxs="i")
axis(1, seq(10,30,5), labels = F)
axis(2, seq(0.5,3.5,1), las = 2, cex.axis = 1.5)

dat<-subset(df, Source == "bz" & Treatment == "Ambient")
points(dat$Rins ~ dat$Temp_C.mean, pch = 6, col = "blue")
dat<-subset(df, Source == "bz" & Treatment == "Warmed")
points(dat$Rins ~ dat$Temp_C.mean, pch = 6, col = "firebrick")
dat<-subset(df, Source == "fl" & Treatment == "Ambient")
points(dat$Rins ~ dat$Temp_C.mean, pch = 2, col = "blue")
dat<-subset(df, Source == "fl" & Treatment == "Warmed")
points(dat$Rins ~ dat$Temp_C.mean, pch = 2, col = "firebrick")

mtext(side = 3, expression(italic(Rhizophora)))

legend("topleft", "b)", bty = "n", cex = 1.2)

m1<-lm(Rins ~ Temp_C.mean * Source * Treatment, df)
Anova(m1); summary(m1)
x<-plot_model(m1, type = c("pred"), terms = c("Temp_C.mean"))
newdata<-as.data.frame(x$data)
points(predicted ~ x, newdata, lty = 1, type = "l", lwd = 2, col = "grey50")
legend("bottom", expression(italic(R)^2~' = 0.61,'~italic(P)~' < 0.001'), bty = "n", cex = 1.3)
text(18, 2.2, "Pre-Frost", cex = 1.2)
arrows(18, 2.0, 20, 1.8, length = 0.1, lwd = 2)

dat<-subset(df, Temp_C.mean > 16)
m1<-lm(Rins ~ Temp_C.mean * Source * Treatment, dat)
Anova(m1); summary(m1)
x<-plot_model(m1, type = c("pred"), terms = c("Temp_C.mean"))
newdata<-as.data.frame(x$data)
points(predicted ~ x, newdata, lty = 1, type = "l", lwd = 3)
legend("topright", expression(italic(R)^2~' = 0.25,'~italic(P)~' < 0.001'), bty = "n", cex = 1.3)
text(16, 1.6, "All", cex = 1.3)
arrows(16, 1.4, 16, 1.0, length = 0.1, lwd = 2)
# legend("top", c("Temperature***","Treatment","Origin"), horiz = T, bty = "n", cex = 1.2)
# legend("bottom", c("Te x Tr", "Te x O", "Tr x O", "Te x Tr x O"), horiz = T, bty = "n", cex = 1.2)

# Rmass AG ####

df<-subset(source, Species == "ag" & R_calc == "R.mass")
plot(df$Rins ~ df$Temp_C.mean, xlim = c(10,30), ylim = c(0,35), pch = NA, xaxt="n",yaxt="n",xlab="",ylab="",xaxs="i")
axis(1, seq(10,35,5), labels = F)
axis(2, seq(0,35,5), las = 2, cex.axis = 1.5)

dat<-subset(df, Source == "bz" & Treatment == "Ambient")
points(dat$Rins ~ dat$Temp_C.mean, pch = 6, col = "blue")
dat<-subset(df, Source == "bz" & Treatment == "Warmed")
points(dat$Rins ~ dat$Temp_C.mean, pch = 6, col = "firebrick")
dat<-subset(df, Source == "fl" & Treatment == "Ambient")
points(dat$Rins ~ dat$Temp_C.mean, pch = 2, col = "blue")
dat<-subset(df, Source == "fl" & Treatment == "Warmed")
points(dat$Rins ~ dat$Temp_C.mean, pch = 2, col = "firebrick")

legend("topleft", "c)", bty = "n", cex = 1.2)

m1<-lm(Rins ~ Temp_C.mean * Source * Treatment, df)
Anova(m1); summary(m1)
x<-plot_model(m1, type = c("pred"), terms = c("Temp_C.mean"))
newdata<-as.data.frame(x$data)
points(predicted ~ x, newdata, lty = 1, type = "l", lwd = 2, col = "grey50")
legend("bottom", expression(italic(R)^2~' = 0.39,'~italic(P)~' < 0.001'), bty = "n", cex = 1.3)
text(15, 17, "All", cex = 1.2)
arrows(15, 15, 15, 10, length = 0.1, lwd = 2)

dat<-subset(df, Temp_C.mean > 16)
m1<-lm(Rins ~ Temp_C.mean * Source * Treatment, dat)
Anova(m1); summary(m1)
x<-plot_model(m1, type = c("pred"), terms = c("Temp_C.mean"))
newdata<-as.data.frame(x$data)
points(predicted ~ x, newdata, lty = 1, type = "l", lwd = 3)
legend("topright", expression(italic(R)^2~' = 0.25,'~italic(P)~' < 0.001'), bty = "n", cex = 1.3)
text(17, 23, "Pre-Frost", cex = 1.2)
arrows(18, 20, 20, 17.5, length = 0.1, lwd = 2)
# legend("top", c("Temperature","Treatment","Origin"), horiz = T, bty = "n", cex = 1.2)
# legend("bottom", c("Te x Tr", "Te x O", "Tr x O", "Te x Tr x O"), horiz = T, bty = "n", cex = 1.2)

mtext(side = 2, expression(italic(R)[mass]*''^italic('in situ')~(nmol~g^-1~s^-1)), cex = 1, padj = -2.5)

# Rmass RM ####

df<-subset(source, Species == "rm" & R_calc == "R.mass")
plot(df$Rins ~ df$Temp_C.mean, xlim = c(10,30), ylim = c(0,35), pch = NA, xaxt="n",yaxt="n",xlab="",ylab="",xaxs="i")
axis(1, seq(10,30,5), labels = F)
axis(2, seq(0,30,5), las = 2, cex.axis = 1.5)

dat<-subset(df, Source == "bz" & Treatment == "Ambient")
points(dat$Rins ~ dat$Temp_C.mean, pch = 6, col = "blue")
dat<-subset(df, Source == "bz" & Treatment == "Warmed")
points(dat$Rins ~ dat$Temp_C.mean, pch = 6, col = "firebrick")
dat<-subset(df, Source == "fl" & Treatment == "Ambient")
points(dat$Rins ~ dat$Temp_C.mean, pch = 2, col = "blue")
dat<-subset(df, Source == "fl" & Treatment == "Warmed")
points(dat$Rins ~ dat$Temp_C.mean, pch = 2, col = "firebrick")

legend("topleft", "d)", bty = "n", cex = 1.2)

m1<-lm(Rins ~ Temp_C.mean * Source * Treatment, df)
Anova(m1); summary(m1)
x<-plot_model(m1, type = c("pred"), terms = c("Temp_C.mean"))
newdata<-as.data.frame(x$data)
points(predicted ~ x, newdata, lty = 1, type = "l", lwd = 2, col = "grey50")
legend("bottom", expression(italic(R)^2~' = 0.46,'~italic(P)~' < 0.001'), bty = "n", cex = 1.3)
text(15, 17, "All", cex = 1.2)
arrows(15, 15, 15, 10, length = 0.1, lwd = 2)

dat<-subset(df, Temp_C.mean > 16)
m1<-lm(Rins ~ Temp_C.mean * Source * Treatment, dat)
Anova(m1); summary(m1)
x<-plot_model(m1, type = c("pred"), terms = c("Temp_C.mean"))
newdata<-as.data.frame(x$data)
points(predicted ~ x, newdata, lty = 1, type = "l", lwd = 3)
legend("topright", expression(italic(R)^2~' = 0.22,'~italic(P)~' < 0.001'), bty = "n", cex = 1.3)
text(17, 23, "Pre-Frost", cex = 1.2)
arrows(18, 20, 20, 17.5, length = 0.1, lwd = 2)
# legend("top", c("Temperature**","Treatment","Origin"), horiz = T, bty = "n", cex = 1.2)
# legend("bottom", c("Te x Tr", "Te x O", "Tr x O", "Te x Tr x O"), horiz = T, bty = "n", cex = 1.2)


# Qins AG ####

df<-subset(source, Species == "ag" & R_calc == "R.area")
plot(df$Qins ~ df$Temp_C.mean, xlim = c(10,30), ylim = c(1,5), pch = NA, xaxt="n",yaxt="n",xlab="",ylab="",xaxs="i")
axis(1, seq(10,30,5), cex.axis = 1.2)
axis(2, seq(0,6,1), las = 2, cex.axis = 1.5)

dat<-subset(df, Source == "bz" & Treatment == "Ambient")
points(dat$Qins ~ dat$Temp_C.mean, pch = 6, col = "blue")
dat<-subset(df, Source == "bz" & Treatment == "Warmed")
points(dat$Qins ~ dat$Temp_C.mean, pch = 6, col = "firebrick")
dat<-subset(df, Source == "fl" & Treatment == "Ambient")
points(dat$Qins ~ dat$Temp_C.mean, pch = 2, col = "blue")
dat<-subset(df, Source == "fl" & Treatment == "Warmed")
points(dat$Qins ~ dat$Temp_C.mean, pch = 2, col = "firebrick")

legend("topleft", "e)", bty = "n")
mtext(side = 2, expression(italic(Q)[10]*''^italic('in situ')), cex = 1, padj = -2.3)

m1<-lm(Qins ~ Temp_C.mean * Source * Treatment, df)
Anova(m1); summary(m1)
x<-plot_model(m1, type = c("pred"), terms = c("Temp_C.mean"))
newdata<-as.data.frame(x$data)
points(predicted ~ x, newdata, lty = 1, type = "l", lwd = 2, col = "grey50")
legend("bottomleft", expression(italic(R)^2~' = 0.34,'~italic(P)~' < 0.001'), bty = "n", cex = 1.3)
text(17, 4.0, "All", cex = 1.2)
arrows(17, 3.8, 17, 3.3, length = 0.1, lwd = 2)


dat<-subset(df, Temp_C.mean > 16)
m1<-lm(Qins ~ Temp_C.mean * Source * Treatment, dat)
Anova(m1); summary(m1)
x<-plot_model(m1, type = c("pred"), terms = c("Temp_C.mean", "Source"))
newdata<-as.data.frame(x$data)
dat<-subset(newdata, group == "fl")
points(predicted ~ x, dat, lty = 1, type = "l", lwd = 2)
text(17, 1.7, "FL", cex = 1.2)
arrows(18, 1.8, 20, 2.05, length = 0.1, lwd = 2)
dat<-subset(newdata, group == "bz")
points(predicted ~ x, dat, lty = 1, type = "l", lwd = 3)
legend("topright", expression(italic(R)^2~' = 0.10,'~italic(P)~' < 0.01'), bty = "n", cex = 1.3)
text(16, 2.7, "BZ", cex = 1.2)
arrows(17, 2.7, 19.5, 2.7, length = 0.1, lwd = 2)
# legend("top", c("Temperature","Treatment","Origin"), horiz = T, bty = "n", cex = 1.2)
# legend("bottom", c("Te x Tr", "Te x O", "Tr x O", "Te x Tr x O"), horiz = T, bty = "n", cex = 1.2)

# Qins RM ####

df<-subset(source, Species == "rm" & R_calc == "R.area")
plot(df$Qins ~ df$Temp_C.mean, xlim = c(10,30), ylim = c(1,5), pch = NA, xaxt="n",yaxt="n",xlab="",ylab="",xaxs="i")
axis(1, seq(10,30,5), cex.axis = 1.2)
axis(2, seq(0,6,1), las = 2, cex.axis = 1.5)

dat<-subset(df, Source == "bz" & Treatment == "Ambient")
points(dat$Qins ~ dat$Temp_C.mean, pch = 6, col = "blue")
dat<-subset(df, Source == "bz" & Treatment == "Warmed")
points(dat$Qins ~ dat$Temp_C.mean, pch = 6, col = "firebrick")
dat<-subset(df, Source == "fl" & Treatment == "Ambient")
points(dat$Qins ~ dat$Temp_C.mean, pch = 2, col = "blue")
dat<-subset(df, Source == "fl" & Treatment == "Warmed")
points(dat$Qins ~ dat$Temp_C.mean, pch = 2, col = "firebrick")

legend("topleft", "f)", bty = "n", adj = 1)

m1<-lm(Qins ~ Temp_C.mean * Source * Treatment, df)
Anova(m1); summary(m1)
x<-plot_model(m1, type = c("pred"), terms = c("Temp_C.mean", "Source"))
newdata<-as.data.frame(x$data)
dat<-subset(newdata, group == "fl")
points(predicted ~ x, dat, lty = 1, type = "l", lwd = 3, col = "grey50")
text(20, 4.3, "FL", cex = 1.2)
arrows(20, 4.1, 20, 3.5, length = 0.1, lwd = 2)
dat<-subset(newdata, group == "bz")
points(predicted ~ x, dat, lty = 1, type = "l", lwd = 3, col = "grey50")
text(16, 1.8, "BZ", cex = 1.2)
arrows(16, 2, 16, 2.4, length = 0.1, lwd = 2)
legend("bottomleft", expression(italic(R)^2~' = 0.43,'~italic(P)~' < 0.01'), bty = "n", cex = 1.3)

dat<-subset(df, Temp_C.mean > 16)
m1<-lm(Qins ~ Temp_C.mean * Source * Treatment, dat)
Anova(m1); summary(m1)
x<-plot_model(m1, type = c("pred"), terms = c("Temp_C.mean"))
newdata<-as.data.frame(x$data)
points(predicted ~ x, newdata, lty = 1, type = "l", lwd = 3, col = "black")
legend("topright", expression(italic(R)^2~' = 0.24,'~italic(P)~' < 0.001'), bty = "n", cex = 1.3)
text(17, 3.2, "Pre-frost", cex = 1.2)
arrows(17.3, 3, 20.5, 2.8, length = 0.1, lwd = 2)
# legend("top", c("Temperature*","Treatment","Origin"), horiz = T, bty = "n", cex = 1.2)
# legend("bottom", c("Te x Tr", "Te x O*", "Tr x O*", "Te x Tr x O"), horiz = T, bty = "n", cex = 1.2)


# off ####
mtext(side = 1, expression(italic(T)[5]~(degree*C)), outer = T, padj = 2)
dev.off()