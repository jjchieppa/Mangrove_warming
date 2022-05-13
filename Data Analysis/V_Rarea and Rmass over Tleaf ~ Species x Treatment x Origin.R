library(lattice);library(doBy); library(lubridate)
library(minpack.lm)

rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

#########################################################
#########################################################
work<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis" # work
raw<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/raw"
hist<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/hist"
lins<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/lins"
deets<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/deets"

setwd(work)

df<-read.csv("RvT respiration data_cleaned.csv")
df<-subset(df, KEEP == "Y")
df$Date<-as.Date(df$Date, format = "%m/%d/%Y")
traits<-read.csv("Mangrove leaf data.csv")
traits$Date<-as.Date(traits$Date, format = "%m/%d/%Y")
df<-merge(traits, df, by = c("Date","UserIDs_in"))
rm(traits)
setwd("C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp")
trt<-read.csv("Mangrove treatment assignments.csv")
setwd(work)
names(trt)[2]<-"plot"
trt$plot<-as.factor(trt$plot)
df$MoYr<-format(as.Date(df$Date), "%Y-%m")
df$R.area<-df$Photo_out*-1
df$R.area_log<-log(df$R.area)
df$CTleaf2<-df$CTleaf_out^2
df$R.mass<-(df$R.area)/df$LMA_kg.m2
df$R.mass_log<-log(df$R.mass)

df<-summaryBy(R.area + R.area_log + R.mass + R.mass_log + CTleaf_out + CTleaf2 + LMA_kg.m2
              ~ UserIDs_in + Date + Moyr + MyTemp,
              FUN = mean, na.rm = T, df)
df<-merge(trt, df, by = "UserIDs_in", all = T)
rm(trt)

#########################################################
#########################################################

setwd("C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis")

tiff(file = "V_Rarea and Rmass over Tleaf ~ Species x Treatment x Origin.tiff", height = 8, width = 10, res = 1000, units = "in", compression = "zip+p")


par(mfrow = c(2,2), mar = c(1.5,2,1.5,0.5), omi = c(0.5,0.5,0.2,0.01))

plot(R.area_log.mean ~ CTleaf_out.mean, df, xlim = c(13,43), ylim = c(-2,3), pch = NA, xaxt="n",yaxt="n")
axis(1, at = seq(15,40,5), cex.axis = 1.5)
axis(2, at = seq(-5,5,1), cex.axis = 1.5, las = 2)
dat<-subset(df, Species == "ag" & Source == "bz" & Treatment == "Ambient")
points(R.area_log.mean ~ CTleaf_out.mean, dat, pch = 6, col = "blue")
dat<-subset(df, Species == "ag" & Source == "fl" & Treatment == "Ambient")
points(R.area_log.mean ~ CTleaf_out.mean, dat, pch = 2, col = "blue")
dat<-subset(df, Species == "ag" & Source == "bz" & Treatment == "Warmed")
points(R.area_log.mean ~ CTleaf_out.mean, dat, pch = 6, col = "firebrick")
dat<-subset(df, Species == "ag" & Source == "fl" & Treatment == "Warmed")
points(R.area_log.mean ~ CTleaf_out.mean, dat, pch = 2, col = "firebrick")

legend("topleft", "a)", cex = 1.5, bty = "n")
legend("bottomright", horiz = F, cex = 1.2, pch = c(6,2,15,15), c("Belize","Florida","Ambient","Warmed"),
       col = c("black","black","blue","firebrick"), bty = "n")

mtext(side = 2, expression(italic(ln)*'('*italic(R)[area]*')'), cex = 1.5, padj = -2)
mtext(side = 3, expression(italic(A.~germinans)), cex = 1.5)

################

plot(R.area_log.mean ~ CTleaf_out.mean, df, xlim = c(13,43), ylim = c(-2,3), pch = NA, xaxt="n",yaxt="n")
axis(1, at = seq(15,40,5), cex.axis = 1.5)
axis(2, at = seq(-5,5,1), cex.axis = 1.5, las = 2)
dat<-subset(df, Species == "rm" & Source == "bz" & Treatment == "Ambient")
points(R.area_log.mean ~ CTleaf_out.mean, dat, pch = 6, col = "blue")
dat<-subset(df, Species == "rm" & Source == "fl" & Treatment == "Ambient")
points(R.area_log.mean ~ CTleaf_out.mean, dat, pch = 2, col = "blue")
dat<-subset(df, Species == "rm" & Source == "bz" & Treatment == "Warmed")
points(R.area_log.mean ~ CTleaf_out.mean, dat, pch = 6, col = "firebrick")
dat<-subset(df, Species == "rm" & Source == "fl" & Treatment == "Warmed")
points(R.area_log.mean ~ CTleaf_out.mean, dat, pch = 2, col = "firebrick")

legend("topleft", "b)", cex = 1.5, bty = "n")

mtext(side = 3, expression(italic(R.~mangle)), cex = 1.5)

##################################################

plot(R.mass_log.mean ~ CTleaf_out.mean, df, xlim = c(13,43), ylim = c(0,5), pch = NA, xaxt="n",yaxt="n")
axis(1, at = seq(15,40,5), cex.axis = 1.5)
axis(2, at = seq(-5,5,1), cex.axis = 1.5, las = 2)
dat<-subset(df, Species == "ag" & Source == "bz" & Treatment == "Ambient")
points(R.mass_log.mean ~ CTleaf_out.mean, dat, pch = 6, col = "blue")
dat<-subset(df, Species == "ag" & Source == "fl" & Treatment == "Ambient")
points(R.mass_log.mean ~ CTleaf_out.mean, dat, pch = 2, col = "blue")
dat<-subset(df, Species == "ag" & Source == "bz" & Treatment == "Warmed")
points(R.mass_log.mean ~ CTleaf_out.mean, dat, pch = 6, col = "firebrick")
dat<-subset(df, Species == "ag" & Source == "fl" & Treatment == "Warmed")
points(R.mass_log.mean ~ CTleaf_out.mean, dat, pch = 2, col = "firebrick")

legend("topleft", "c)", cex = 1.5, bty = "n")

mtext(side = 2, expression(italic(ln)*'('*italic(R)[mass]*')'), cex = 1.5, padj = -2)

################

plot(R.area_log.mean ~ CTleaf_out.mean, df, xlim = c(13,43), ylim = c(0,5), pch = NA, xaxt="n",yaxt="n")
axis(1, at = seq(15,40,5), cex.axis = 1.5)
axis(2, at = seq(-5,5,1), cex.axis = 1.5, las = 2)
dat<-subset(df, Species == "rm" & Source == "bz" & Treatment == "Ambient")
points(R.mass_log.mean ~ CTleaf_out.mean, dat, pch = 6, col = "blue")
dat<-subset(df, Species == "rm" & Source == "fl" & Treatment == "Ambient")
points(R.mass_log.mean ~ CTleaf_out.mean, dat, pch = 2, col = "blue")
dat<-subset(df, Species == "rm" & Source == "bz" & Treatment == "Warmed")
points(R.mass_log.mean ~ CTleaf_out.mean, dat, pch = 6, col = "firebrick")
dat<-subset(df, Species == "rm" & Source == "fl" & Treatment == "Warmed")
points(R.mass_log.mean ~ CTleaf_out.mean, dat, pch = 2, col = "firebrick")

legend("topleft", "d)", cex = 1.5, bty = "n")

mtext(side = 1, outer = T, expression(italic(T)[leaf]~(degree*C)), cex = 1.75, padj = 1.1)

dev.off()