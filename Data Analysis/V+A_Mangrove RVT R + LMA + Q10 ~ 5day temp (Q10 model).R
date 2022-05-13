library(lattice);library(doBy); library(lubridate)
library(minpack.lm); library(effects); library(plotrix)

rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

#########################################################
#########################################################

home<-"C:/Users/n01456074/OneDrive - University of North Florida/Other Projects/Mangrove rvt"
work<-"C:/Users/n01456074/OneDrive - University of North Florida/Other Projects/Mangrove rvt" # work
raw<-"C:/Users/n01456074/OneDrive - University of North Florida/Other Projects/Mangrove rvt/Model Outputs/raw"
hist<-"C:/Users/n01456074/OneDrive - University of North Florida/Other Projects/Mangrove rvt/Model Outputs/hist"
lins<-"C:/Users/n01456074/OneDrive - University of North Florida/Other Projects/Mangrove rvt/Model Outputs/lins"
deets<-"C:/Users/n01456074/OneDrive - University of North Florida/Other Projects/Mangrove rvt/Model Outputs/deets"
env<-"C:/Users/n01456074/OneDrive - University of North Florida/Other Projects/Mangrove rvt/Environmental Data"
anly<-"C:/Users/n01456074/OneDrive - University of North Florida/Other Projects/Mangrove rvt/Model Outputs/Analysis"

#########################################################
#########################################################

### Bring in the ref respiration data, bind area and mass for LMA

setwd(deets)
source<-read.csv("MangroveRVT_Q10mod_Rarea and Rmass at 15 20 25c.csv")
source$Date<-as.Date(source$Date, format = "%Y-%m-%d")
setwd(work)
dates<-read.csv("Mangrove rvt date assignments.csv")
dates$Date<-as.Date(dates$Date, format = "%m/%d/%Y")
trts<-read.csv("Mangrove rvt trt assignments.csv")
source<-merge(source, dates, by = "Date", all = T)
source<-merge(source, trts, by = "PlantID", all = T)
rm(dates, trts)

setwd(work)
area<-read.csv("Mangrove rvt combined data.csv")
area<-subset(area, KEEP == "Y")
area$Date<-as.Date(area$Date, format = "%m/%d/%Y")
area<-summaryBy(Area_in ~ PlantID + Date, FUN = mean, na.rm = T, area)
source<-merge(area, source, by = c("PlantID","Date"))
mass<-read.csv("raw leaf weights.csv")
source<-merge(source, mass, by = c("PlantID","Moyr"))
rm(area, mass)
source$LMA_g.cm2<-source$mass_g/source$Area_in.mean
quantile(source$LMA_g.cm2, probs = seq(0,1,0.01), na.rm = T)
source$Timepoint<-as.factor(source$Timepoint)
source<-subset(source, LMA_g.cm2 < 0.03)
source$LMA_g.m2<-source$mass_g/(source$Area_in.mean*0.0001)

source$PlantID2.x<-NULL
source$PlantID2.y<-NULL
names(source)[4]<-"Area_cm"
names(source)[10]<-"block"
source$Date<-as.Date(source$Date, format = "%Y-%m-%d") # Double checks date format!

#########################################################
#########################################################

# Bring in temperature data

setwd(env)

env<-read.csv("Mangrove rvt temp data_cleaned.csv")
env$Date<-as.Date(env$Date, format = "%Y-%m-%d")

#########################################################
#########################################################


df<-source
df$sDate<-as.factor(df$Date)
env$sDate<-as.factor(env$Date)
reps<-unique(df$sDate)
new_env<-subset(env, sensor == "")
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


hobo<-subset(new_env, sensor == "hobo_boatyard")
ibuW<-subset(new_env, sensor != "hobo_boatyard" & treatment == "warmed")
ibuA<-subset(new_env, sensor != "hobo_boatyard" & treatment == "ambient")
rm(new_env, env)

hobo<-summaryBy(tempNA ~ Date, FUN = mean, na.rm = T, hobo)
ibuW<-summaryBy(tempNA ~ Date, FUN = mean, na.rm = T, ibuW)
ibuA<-summaryBy(tempNA ~ Date, FUN = mean, na.rm = T, ibuA)
names(hobo)[2]<-"hobo_amb"
names(ibuW)[2]<-"ibut_war"
names(ibuA)[2]<-"ibut_amb"
temp<-merge(hobo, ibuW, by = c("Date"), all = T)
temp<-merge(temp, ibuA, by = c("Date"), all = T)
rm(hobo, ibuA, ibuW)

df<-merge(df, temp, by = "Date"); rm(temp)

#########################################################
#########################################################

### Graphs below

####################################################################################################
####################################################################################################

### Rarea + Rmass @ 20 ~ 5day temp

setwd(anly)

#############################

tiff(file = "Q10mod_V+A_Rarea20 and Rarea25 ~ iBut + Hobo 5day temp (Q10 model).tiff", height = 6, width = 10, res = 600, units = "in", compression = "zip+p")

par(mfrow = c(4,3), mar = c(0.5,0.1,0.5,0.1), omi = c(0.75,0.75,0.15,0.5))

dat<-subset(df, R_calc == "R.area" & MyTemp == "20")
plot(dat$rdref ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0,2.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(2, at = seq(0,2.25,0.5), las = 2)
axis(1, at = seq(10,30,5), labels = F)
mtext(side = 3, expression(italic(Rhizophora~mangle)))
dum<-subset(dat, Species == "red" & treatment == "ambient")
points(dum$rdref ~ dum$ibut_amb, pch = 1, col = "black")
dum<-subset(dat, Species == "red" & treatment == "warmed")
points(dum$rdref ~ dum$ibut_war, pch = 1, col = "firebrick")

legend("topleft", c("Ambient","Warmed"), pch = 1, col = c("black","firebrick"), cex = 1.25, bty = "n", horiz = T)

plot(dat$rdref ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0,2.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), labels = F)
mtext(side = 3, expression(italic(Laguncularia~racemosa)))
dum<-subset(dat, Species == "white" & treatment == "ambient")
points(dum$rdref ~ dum$ibut_amb, pch = 1, col = "black")
dum<-subset(dat, Species == "white" & treatment == "warmed")
points(dum$rdref ~ dum$ibut_war, pch = 1, col = "firebrick")

plot(dat$rdref ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0,2.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), labels = F)
mtext(side = 3, expression(italic(Avincennia~germinans)))
dum<-subset(dat, Species == "black" & treatment == "ambient")
points(dum$rdref ~ dum$ibut_amb, pch = 1, col = "black")
dum<-subset(dat, Species == "black" & treatment == "warmed")
points(dum$rdref ~ dum$ibut_war, pch = 1, col = "firebrick")
mtext(side = 4, "iButtons", padj = 1)

#############################

dat<-subset(df, R_calc == "R.area" & MyTemp == "20")
plot(dat$rdref ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0,2.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(2, at = seq(0,2.25,0.5), las = 2)
axis(1, at = seq(10,30,5), labels = F)
dum<-subset(dat, Species == "red" & treatment == "ambient")
points(dum$rdref ~ dum$hobo_amb, pch = 1, col = "black")

plot(dat$rdref ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0,2.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), labels = F)
dum<-subset(dat, Species == "white" & treatment == "ambient")
points(dum$rdref ~ dum$hobo_amb, pch = 1, col = "black")

plot(dat$rdref ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0,2.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), labels = F)
dum<-subset(dat, Species == "black" & treatment == "ambient")
points(dum$rdref ~ dum$hobo_amb, pch = 1, col = "black")
mtext(side = 4, "Hobo", padj = 1)

#############################

dat<-subset(df, R_calc == "R.area" & MyTemp == "25")
plot(dat$rdref ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0,2.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(2, at = seq(0,2.25,0.5), las = 2)
axis(1, at = seq(10,30,5), labels = F)
dum<-subset(dat, Species == "red" & treatment == "ambient")
points(dum$rdref ~ dum$ibut_amb, pch = 1, col = "black")
dum<-subset(dat, Species == "red" & treatment == "warmed")
points(dum$rdref ~ dum$ibut_war, pch = 1, col = "firebrick")

plot(dat$rdref ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0,2.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), labels = F)
dum<-subset(dat, Species == "white" & treatment == "ambient")
points(dum$rdref ~ dum$ibut_amb, pch = 1, col = "black")
dum<-subset(dat, Species == "white" & treatment == "warmed")
points(dum$rdref ~ dum$ibut_war, pch = 1, col = "firebrick")

plot(dat$rdref ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0,2.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), labels = F)
dum<-subset(dat, Species == "black" & treatment == "ambient")
points(dum$rdref ~ dum$ibut_amb, pch = 1, col = "black")
dum<-subset(dat, Species == "black" & treatment == "warmed")
points(dum$rdref ~ dum$ibut_war, pch = 1, col = "firebrick")
mtext(side = 4, "iButtons", padj = 1)

#############################

dat<-subset(df, R_calc == "R.area" & MyTemp == "25")
plot(dat$rdref ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0,2.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(2, at = seq(0,2.25,0.5), las = 2)
axis(1, at = seq(10,30,5), cex.axis = 1.1)
dum<-subset(dat, Species == "red" & treatment == "ambient")
points(dum$rdref ~ dum$hobo_amb, pch = 1, col = "black")

plot(dat$rdref ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0,2.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), cex.axis = 1.1)
dum<-subset(dat, Species == "white" & treatment == "ambient")
points(dum$rdref ~ dum$hobo_amb, pch = 1, col = "black")

plot(dat$rdref ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0,2.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), cex.axis = 1.1)
dum<-subset(dat, Species == "black" & treatment == "ambient")
points(dum$rdref ~ dum$hobo_amb, pch = 1, col = "black")
mtext(side = 4, "Hobo", padj = 1)

mtext(side = 2, outer = T, expression(R[20-Area]), cex = 1.5, padj = -1.7, adj = 0.75)
mtext(side = 2, outer = T, expression(R[25-Area]), cex = 1.5, padj = -1.7, adj = 0.25)
mtext(side = 1, outer = T, expression(5~Day~Mean~Temperature~(degree*C)), cex = 1.5, padj = 1.5)

dev.off()

rm(dat, dum)

#######################################################################
#######################################################################

###  Rmass @ 20 + 25 ~ 5day temp (ibut and hobo)

setwd(anly)

#############################

tiff(file = "Q10mod_V+A_Rmass20 and Rmass25 ~ iBut + Hobo 5day temp (Q10 model).tiff", height = 6, width = 10, res = 600, units = "in", compression = "zip+p")

par(mfrow = c(4,3), mar = c(0.5,0.1,0.5,0.1), omi = c(0.75,0.75,0.15,0.5))

dat<-subset(df, R_calc == "R.mass" & MyTemp == "20")
plot(dat$rdref ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0,15), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(2, at = seq(0,15,2.5), las = 2)
axis(1, at = seq(10,30,5), labels = F)
mtext(side = 3, expression(italic(Rhizophora~mangle)))
dum<-subset(dat, Species == "red" & treatment == "ambient")
points(dum$rdref ~ dum$ibut_amb, pch = 1, col = "black")
dum<-subset(dat, Species == "red" & treatment == "warmed")
points(dum$rdref ~ dum$ibut_war, pch = 1, col = "firebrick")

legend("topleft", c("Ambient","Warmed"), pch = 1, col = c("black","firebrick"), cex = 1.25, bty = "n", horiz = T)

plot(dat$rdref ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0,15), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), labels = F)
mtext(side = 3, expression(italic(Laguncularia~racemosa)))
dum<-subset(dat, Species == "white" & treatment == "ambient")
points(dum$rdref ~ dum$ibut_amb, pch = 1, col = "black")
dum<-subset(dat, Species == "white" & treatment == "warmed")
points(dum$rdref ~ dum$ibut_war, pch = 1, col = "firebrick")

plot(dat$rdref ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0,15), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), labels = F)
mtext(side = 3, expression(italic(Avincennia~germinans)))
dum<-subset(dat, Species == "black" & treatment == "ambient")
points(dum$rdref ~ dum$ibut_amb, pch = 1, col = "black")
dum<-subset(dat, Species == "black" & treatment == "warmed")
points(dum$rdref ~ dum$ibut_war, pch = 1, col = "firebrick")
mtext(side = 4, "iButtons", padj = 1)

#############################

dat<-subset(df, R_calc == "R.mass" & MyTemp == "20")
plot(dat$rdref ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0,15), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(2, at = seq(0,15,2.5), las = 2)
axis(1, at = seq(10,30,5), labels = F)
dum<-subset(dat, Species == "red" & treatment == "ambient")
points(dum$rdref ~ dum$hobo_amb, pch = 1, col = "black")

plot(dat$rdref ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0,15), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), labels = F)
dum<-subset(dat, Species == "white" & treatment == "ambient")
points(dum$rdref ~ dum$hobo_amb, pch = 1, col = "black")

plot(dat$rdref ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0,15), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), labels = F)
dum<-subset(dat, Species == "black" & treatment == "ambient")
points(dum$rdref ~ dum$hobo_amb, pch = 1, col = "black")
mtext(side = 4, "Hobo", padj = 1)

#############################

dat<-subset(df, R_calc == "R.mass" & MyTemp == "25")
plot(dat$rdref ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0,15), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(2, at = seq(0,15,2.5), las = 2)
axis(1, at = seq(10,30,5), labels = F)
dum<-subset(dat, Species == "red" & treatment == "ambient")
points(dum$rdref ~ dum$ibut_amb, pch = 1, col = "black")
dum<-subset(dat, Species == "red" & treatment == "warmed")
points(dum$rdref ~ dum$ibut_war, pch = 1, col = "firebrick")

plot(dat$rdref ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0,15), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), labels = F)
dum<-subset(dat, Species == "white" & treatment == "ambient")
points(dum$rdref ~ dum$ibut_amb, pch = 1, col = "black")
dum<-subset(dat, Species == "white" & treatment == "warmed")
points(dum$rdref ~ dum$ibut_war, pch = 1, col = "firebrick")

plot(dat$rdref ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0,15), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), labels = F)
dum<-subset(dat, Species == "black" & treatment == "ambient")
points(dum$rdref ~ dum$ibut_amb, pch = 1, col = "black")
dum<-subset(dat, Species == "black" & treatment == "warmed")
points(dum$rdref ~ dum$ibut_war, pch = 1, col = "firebrick")
mtext(side = 4, "iButtons", padj = 1)

#############################

dat<-subset(df, R_calc == "R.mass" & MyTemp == "25")
plot(dat$rdref ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0,15), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(2, at = seq(0,15,2.5), las = 2)
axis(1, at = seq(10,30,5), cex.axis = 1.1)
dum<-subset(dat, Species == "red" & treatment == "ambient")
points(dum$rdref ~ dum$hobo_amb, pch = 1, col = "black")

plot(dat$rdref ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0,15), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), cex.axis = 1.1)
dum<-subset(dat, Species == "white" & treatment == "ambient")
points(dum$rdref ~ dum$hobo_amb, pch = 1, col = "black")

plot(dat$rdref ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0,15), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), cex.axis = 1.1)
dum<-subset(dat, Species == "black" & treatment == "ambient")
points(dum$rdref ~ dum$hobo_amb, pch = 1, col = "black")
mtext(side = 4, "Hobo", padj = 1)

mtext(side = 2, outer = T, expression(R[20-Mass]), cex = 1.5, padj = -2, adj = 0.75)
mtext(side = 2, outer = T, expression(R[25-Mass]), cex = 1.5, padj = -2, adj = 0.25)
mtext(side = 1, outer = T, expression(5~Day~Mean~Temperature~(degree*C)), cex = 1.5, padj = 1.5)

dev.off()

rm(dat, dum)

####################################################################################################
####################################################################################################

### Q10 area @ 20 + 25 ~ 5day temp (ibut and hobo)

setwd(anly)

#############################

tiff(file = "Q10mod_V+A_Q10 area20 and area25 ~ iBut + Hobo 5day temp (Q10 model).tiff", height = 6, width = 10, res = 600, units = "in", compression = "zip+p")

par(mfrow = c(4,3), mar = c(0.5,0.1,0.5,0.1), omi = c(0.75,0.75,0.15,0.5))

dat<-subset(df, R_calc == "R.area" & MyTemp == "20")
plot(dat$q10 ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0.25,3.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(2, at = seq(0,3,0.5), las = 2)
axis(1, at = seq(10,30,5), labels = F)
mtext(side = 3, expression(italic(Rhizophora~mangle)))
dum<-subset(dat, Species == "red" & treatment == "ambient")
points(dum$q10 ~ dum$ibut_amb, pch = 1, col = "black")
dum<-subset(dat, Species == "red" & treatment == "warmed")
points(dum$q10 ~ dum$ibut_war, pch = 1, col = "firebrick")

legend("topleft", c("Ambient","Warmed"), pch = 1, col = c("black","firebrick"), cex = 1.25, bty = "n", horiz = T)

plot(dat$q10 ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0.25,3.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), labels = F)
mtext(side = 3, expression(italic(Laguncularia~racemosa)))
dum<-subset(dat, Species == "white" & treatment == "ambient")
points(dum$q10 ~ dum$ibut_amb, pch = 1, col = "black")
dum<-subset(dat, Species == "white" & treatment == "warmed")
points(dum$q10 ~ dum$ibut_war, pch = 1, col = "firebrick")

plot(dat$q10 ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0.25,3.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), labels = F)
mtext(side = 3, expression(italic(Avincennia~germinans)))
dum<-subset(dat, Species == "black" & treatment == "ambient")
points(dum$q10 ~ dum$ibut_amb, pch = 1, col = "black")
dum<-subset(dat, Species == "black" & treatment == "warmed")
points(dum$q10 ~ dum$ibut_war, pch = 1, col = "firebrick")
mtext(side = 4, "iButtons", padj = 1)

#############################

dat<-subset(df, R_calc == "R.area" & MyTemp == "20")
plot(dat$q10 ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0.25,3.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(2, at = seq(0,3,0.5), las = 2)
axis(1, at = seq(10,30,5), labels = F)
dum<-subset(dat, Species == "red" & treatment == "ambient")
points(dum$q10 ~ dum$hobo_amb, pch = 1, col = "black")

plot(dat$q10 ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0.25,3.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), labels = F)
dum<-subset(dat, Species == "white" & treatment == "ambient")
points(dum$q10 ~ dum$hobo_amb, pch = 1, col = "black")

plot(dat$q10 ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0.25,3.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), labels = F)
dum<-subset(dat, Species == "black" & treatment == "ambient")
points(dum$q10 ~ dum$hobo_amb, pch = 1, col = "black")
mtext(side = 4, "Hobo", padj = 1)

#############################

dat<-subset(df, R_calc == "R.area" & MyTemp == "25")
plot(dat$q10 ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0.25,3.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(2, at = seq(0,3,0.5), las = 2)
axis(1, at = seq(10,30,5), labels = F)
dum<-subset(dat, Species == "red" & treatment == "ambient")
points(dum$q10 ~ dum$ibut_amb, pch = 1, col = "black")
dum<-subset(dat, Species == "red" & treatment == "warmed")
points(dum$q10 ~ dum$ibut_war, pch = 1, col = "firebrick")

plot(dat$q10 ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0.25,3.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), labels = F)
dum<-subset(dat, Species == "white" & treatment == "ambient")
points(dum$q10 ~ dum$ibut_amb, pch = 1, col = "black")
dum<-subset(dat, Species == "white" & treatment == "warmed")
points(dum$q10 ~ dum$ibut_war, pch = 1, col = "firebrick")

plot(dat$q10 ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0.25,3.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), labels = F)
dum<-subset(dat, Species == "black" & treatment == "ambient")
points(dum$q10 ~ dum$ibut_amb, pch = 1, col = "black")
dum<-subset(dat, Species == "black" & treatment == "warmed")
points(dum$q10 ~ dum$ibut_war, pch = 1, col = "firebrick")
mtext(side = 4, "iButtons", padj = 1)

#############################

dat<-subset(df, R_calc == "R.area" & MyTemp == "25")
plot(dat$q10 ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0.25,3.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(2, at = seq(0,3,0.5), las = 2)
axis(1, at = seq(10,30,5), cex.axis = 1.1)
dum<-subset(dat, Species == "red" & treatment == "ambient")
points(dum$q10 ~ dum$hobo_amb, pch = 1, col = "black")

plot(dat$q10 ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0.25,3.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), cex.axis = 1.1)
dum<-subset(dat, Species == "white" & treatment == "ambient")
points(dum$q10 ~ dum$hobo_amb, pch = 1, col = "black")

plot(dat$q10 ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0.25,3.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), cex.axis = 1.1)
dum<-subset(dat, Species == "black" & treatment == "ambient")
points(dum$q10 ~ dum$hobo_amb, pch = 1, col = "black")
mtext(side = 4, "Hobo", padj = 1)

mtext(side = 2, outer = T, expression(Q10[20-Area]), cex = 1.5, padj = -1.7, adj = 0.75)
mtext(side = 2, outer = T, expression(Q10[25-Area]), cex = 1.5, padj = -1.7, adj = 0.25)
mtext(side = 1, outer = T, expression(5~Day~Mean~Temperature~(degree*C)), cex = 1.5, padj = 1.5)

dev.off()

rm(dat, dum)


#######################################################################
#######################################################################

### Q10 mass @ 20 + 25 ~ 5day temp (ibut and hobo)

tiff(file = "Q10mod_V+A_Q10 mass20 and mass25 ~ iBut + Hobo 5day temp (Q10 model).tiff", height = 6, width = 10, res = 600, units = "in", compression = "zip+p")

par(mfrow = c(4,3), mar = c(0.5,0.1,0.5,0.1), omi = c(0.75,0.75,0.15,0.5))

dat<-subset(df, R_calc == "R.mass" & MyTemp == "20")
plot(dat$q10 ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0.25,3.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(2, at = seq(0,3,0.5), las = 2)
axis(1, at = seq(10,30,5), labels = F)
mtext(side = 3, expression(italic(Rhizophora~mangle)))
dum<-subset(dat, Species == "red" & treatment == "ambient")
points(dum$q10 ~ dum$ibut_amb, pch = 1, col = "black")
dum<-subset(dat, Species == "red" & treatment == "warmed")
points(dum$q10 ~ dum$ibut_war, pch = 1, col = "firebrick")

legend("topleft", c("Ambient","Warmed"), pch = 1, col = c("black","firebrick"), cex = 1.25, bty = "n", horiz = T)

plot(dat$q10 ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0.25,3.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), labels = F)
mtext(side = 3, expression(italic(Laguncularia~racemosa)))
dum<-subset(dat, Species == "white" & treatment == "ambient")
points(dum$q10 ~ dum$ibut_amb, pch = 1, col = "black")
dum<-subset(dat, Species == "white" & treatment == "warmed")
points(dum$q10 ~ dum$ibut_war, pch = 1, col = "firebrick")

plot(dat$q10 ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0.25,3.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), labels = F)
mtext(side = 3, expression(italic(Avincennia~germinans)))
dum<-subset(dat, Species == "black" & treatment == "ambient")
points(dum$q10 ~ dum$ibut_amb, pch = 1, col = "black")
dum<-subset(dat, Species == "black" & treatment == "warmed")
points(dum$q10 ~ dum$ibut_war, pch = 1, col = "firebrick")
mtext(side = 4, "iButtons", padj = 1)

#############################

dat<-subset(df, R_calc == "R.mass" & MyTemp == "20")
plot(dat$q10 ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0.25,3.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(2, at = seq(0,3,0.5), las = 2)
axis(1, at = seq(10,30,5), labels = F)
dum<-subset(dat, Species == "red" & treatment == "ambient")
points(dum$q10 ~ dum$hobo_amb, pch = 1, col = "black")

plot(dat$q10 ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0.25,3.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), labels = F)
dum<-subset(dat, Species == "white" & treatment == "ambient")
points(dum$q10 ~ dum$hobo_amb, pch = 1, col = "black")

plot(dat$q10 ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0.25,3.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), labels = F)
dum<-subset(dat, Species == "black" & treatment == "ambient")
points(dum$q10 ~ dum$hobo_amb, pch = 1, col = "black")
mtext(side = 4, "Hobo", padj = 1)

#############################

dat<-subset(df, R_calc == "R.mass" & MyTemp == "25")
plot(dat$q10 ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0.25,3.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(2, at = seq(0,3,0.5), las = 2)
axis(1, at = seq(10,30,5), labels = F)
dum<-subset(dat, Species == "red" & treatment == "ambient")
points(dum$q10 ~ dum$ibut_amb, pch = 1, col = "black")
dum<-subset(dat, Species == "red" & treatment == "warmed")
points(dum$q10 ~ dum$ibut_war, pch = 1, col = "firebrick")

plot(dat$q10 ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0.25,3.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), labels = F)
dum<-subset(dat, Species == "white" & treatment == "ambient")
points(dum$q10 ~ dum$ibut_amb, pch = 1, col = "black")
dum<-subset(dat, Species == "white" & treatment == "warmed")
points(dum$q10 ~ dum$ibut_war, pch = 1, col = "firebrick")

plot(dat$q10 ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0.25,3.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), labels = F)
dum<-subset(dat, Species == "black" & treatment == "ambient")
points(dum$q10 ~ dum$ibut_amb, pch = 1, col = "black")
dum<-subset(dat, Species == "black" & treatment == "warmed")
points(dum$q10 ~ dum$ibut_war, pch = 1, col = "firebrick")
mtext(side = 4, "iButtons", padj = 1)

#############################

dat<-subset(df, R_calc == "R.mass" & MyTemp == "25")
plot(dat$q10 ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0.25,3.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(2, at = seq(0,3,0.5), las = 2)
axis(1, at = seq(10,30,5), cex.axis = 1.1)
dum<-subset(dat, Species == "red" & treatment == "ambient")
points(dum$q10 ~ dum$hobo_amb, pch = 1, col = "black")

plot(dat$q10 ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0.25,3.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), cex.axis = 1.1)
dum<-subset(dat, Species == "white" & treatment == "ambient")
points(dum$q10 ~ dum$hobo_amb, pch = 1, col = "black")

plot(dat$q10 ~ dat$hobo_amb, xlim = c(8,32), ylim = c(0.25,3.25), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), cex.axis = 1.1)
dum<-subset(dat, Species == "black" & treatment == "ambient")
points(dum$q10 ~ dum$hobo_amb, pch = 1, col = "black")
mtext(side = 4, "Hobo", padj = 1)

mtext(side = 2, outer = T, expression(Q10[20-Mass]), cex = 1.5, padj = -1.7, adj = 0.75)
mtext(side = 2, outer = T, expression(Q10[25-Mass]), cex = 1.5, padj = -1.7, adj = 0.25)
mtext(side = 1, outer = T, expression(5~Day~Mean~Temperature~(degree*C)), cex = 1.5, padj = 1.5)

dev.off()

rm(dat, dum)

####################################################################################################
####################################################################################################

### LMA ~ Rarea @ 20 + 25 (ibut + hobo)

tiff(file = "Q10mod_V+A_LMA ~ iBut + Hobo 5day temp (Q10 model).tiff", height = 6, width = 10, res = 600, units = "in", compression = "zip+p")

par(mfrow = c(2,3), mar = c(0.5,0.1,0.5,0.1), omi = c(0.75,0.75,0.15,0.5))

dat<-subset(df, R_calc == "R.mass" & MyTemp == "20")
plot(dat$LMA_g.m2 ~ dat$hobo_amb, xlim = c(8,32), ylim = c(50,275), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(2, at = seq(50,300,25), las = 2)
axis(1, at = seq(10,30,5), labels = F)
mtext(side = 3, expression(italic(Rhizophora~mangle)))
dum<-subset(dat, Species == "red" & treatment == "ambient")
points(dum$LMA_g.m2 ~ dum$ibut_amb, pch = 1, col = "black")
dum<-subset(dat, Species == "red" & treatment == "warmed")
points(dum$LMA_g.m2 ~ dum$ibut_war, pch = 1, col = "firebrick")

legend("topleft", c("Ambient","Warmed"), pch = 1, col = c("black","firebrick"), cex = 1.25, bty = "n", horiz = T)

plot(dat$LMA_g.m2 ~ dat$hobo_amb, xlim = c(8,32), ylim = c(50,275), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), labels = F)
mtext(side = 3, expression(italic(Laguncularia~racemosa)))
dum<-subset(dat, Species == "white" & treatment == "ambient")
points(dum$LMA_g.m2 ~ dum$ibut_amb, pch = 1, col = "black")
dum<-subset(dat, Species == "white" & treatment == "warmed")
points(dum$LMA_g.m2 ~ dum$ibut_war, pch = 1, col = "firebrick")

plot(dat$LMA_g.m2 ~ dat$hobo_amb, xlim = c(8,32), ylim = c(50,275), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), labels = F)
mtext(side = 3, expression(italic(Avincennia~germinans)))
dum<-subset(dat, Species == "black" & treatment == "ambient")
points(dum$LMA_g.m2 ~ dum$ibut_amb, pch = 1, col = "black")
dum<-subset(dat, Species == "black" & treatment == "warmed")
points(dum$LMA_g.m2 ~ dum$ibut_war, pch = 1, col = "firebrick")
mtext(side = 4, "iButtons", padj = 1)

#############################

dat<-subset(df, R_calc == "R.mass" & MyTemp == "20")
plot(dat$LMA_g.m2 ~ dat$hobo_amb, xlim = c(8,32), ylim = c(50,275), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(2, at = seq(50,300,25), las = 2)
axis(1, at = seq(10,30,5), cex.axis = 1.1)
dum<-subset(dat, Species == "red" & treatment == "ambient")
points(dum$LMA_g.m2 ~ dum$hobo_amb, pch = 1, col = "black")

plot(dat$LMA_g.m2 ~ dat$hobo_amb, xlim = c(8,32), ylim = c(50,275), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), cex.axis = 1.1)
dum<-subset(dat, Species == "white" & treatment == "ambient")
points(dum$LMA_g.m2 ~ dum$hobo_amb, pch = 1, col = "black")

plot(dat$LMA_g.m2 ~ dat$hobo_amb, xlim = c(8,32), ylim = c(50,275), pch = NA,xaxt="n",yaxt="n",xlab="",ylab="")
axis(1, at = seq(10,30,5), cex.axis = 1.1)
dum<-subset(dat, Species == "black" & treatment == "ambient")
points(dum$LMA_g.m2 ~ dum$hobo_amb, pch = 1, col = "black")
mtext(side = 4, "Hobo", padj = 1)

mtext(side = 2, outer = T, expression(LMA~(g~m^-2)), cex = 1.5, padj = -1.2)
mtext(side = 1, outer = T, expression(5~Day~Mean~Temperature~(degree*C)), cex = 1.5, padj = 1.5)

dev.off()

rm(dat, dum)

####################################################################################################
####################################################################################################

### LMA ~ Date

setwd(anly)
dat<-summaryBy(LMA_g.m2 ~ Timepoint * Species * treatment, FUN = c(mean, std.error), na.rm = T, df)
dat$Timepoint<-as.numeric(dat$Timepoint)

tiff(file = "Q10mod_V+A_LMA ~ Date (Q10 model).tiff", height = 6, width = 10, res = 600, units = "in", compression = "zip+p")

par(mfrow = c(3,1), mar = c(0.5,0.1,0.5,0.1), omi = c(0.75,0.75,0.15,0.5))

dum<-subset(dat, Species  == "red" & treatment == "ambient")
plotCI(dum$Timepoint, dum$LMA_g.m2.mean, dum$LMA_g.m2.std.error, xlim = c(0.5,7.5),
       ylim = c(75, 225), xaxt = "n", yaxt = "n", xlab = "", ylab = "", pch = 1, col = "black")
par(new=T)
dum<-subset(dat, Species  == "red" & treatment == "warmed")
plotCI(dum$Timepoint, dum$LMA_g.m2.mean, dum$LMA_g.m2.std.error, xlim = c(0.5,7.5),
       ylim = c(75, 225), xaxt = "n", yaxt = "n", xlab = "", ylab = "", pch = 1, col = "firebrick")

axis(2, at = seq(50,300,50), las = 2)
legend("top", c("Ambient","Warmed"), pch = 1, col = c("black","firebrick"), cex = 1.25, horiz = T, bty = "n")
legend("topright", expression(italic(Rhizophora~mangle)), bty = "n")

#############################

dum<-subset(dat, Species  == "white" & treatment == "ambient")
plotCI(dum$Timepoint, dum$LMA_g.m2.mean, dum$LMA_g.m2.std.error, xlim = c(0.5,7.5),
       ylim = c(75, 225), xaxt = "n", yaxt = "n", xlab = "", ylab = "", pch = 1, col = "black")
par(new=T)
dum<-subset(dat, Species  == "white" & treatment == "warmed")
plotCI(dum$Timepoint, dum$LMA_g.m2.mean, dum$LMA_g.m2.std.error, xlim = c(0.5,7.5),
       ylim = c(75, 225), xaxt = "n", yaxt = "n", xlab = "", ylab = "", pch = 1, col = "firebrick")
axis(2, at = seq(50,300,50), las = 2)
# legend("top", c("Ambient","Warmed"), pch = 1, col = c("black","firebrick"), cex = 1.25, horiz = T, bty = "n")
legend("topright", expression(italic(Laguncularia~racemosa)), bty = "n")

#############################

dum<-subset(dat, Species  == "black" & treatment == "ambient")
plotCI(dum$Timepoint, dum$LMA_g.m2.mean, dum$LMA_g.m2.std.error, xlim = c(0.5,7.5),
       ylim = c(75, 225), xaxt = "n", yaxt = "n", xlab = "", ylab = "", pch = 1, col = "black")
par(new=T)
dum<-subset(dat, Species  == "black" & treatment == "warmed")
plotCI(dum$Timepoint, dum$LMA_g.m2.mean, dum$LMA_g.m2.std.error, xlim = c(0.5,7.5),
       ylim = c(75, 225), xaxt = "n", yaxt = "n", xlab = "", ylab = "", pch = 1, col = "firebrick")
axis(2, at = seq(50,300,50), las = 2)
# legend("top", c("Ambient","Warmed"), pch = 1, col = c("black","firebrick"), cex = 1.25, horiz = T, bty = "n")
legend("topright", expression(italic(Avincennia~germinans)), bty = "n")

axis(1, at = seq(1,7,1), las = 2, cex.axis = 1.5,
     labels = c("Jul-19","Aug-19","Sep-19","Oct-19","Nov-19","Dec-19","Jan-20"))

mtext(side = 2, outer = T, expression(LMA~(g~m^-2)), cex = 1.5, padj = -1.2)

dev.off()

rm(dat, dum)

####################################################################################################
####################################################################################################

### Space for plot snooping

# dat<-subset(df, R_calc == "R.mass" & MyTemp == "20")
# 
# par(mfrow = c(3,1), mar = c(1,1,1,1), omi = c(0.5,0.5,0,0))
# plot(dat$rdref ~ dat$q10, pch = NA)
# dum<-subset(dat, Species == "red" & treatment == "warmed")
# points(dum$rdref ~ dum$q10, pch = 1, col = "firebrick")
# dum<-subset(dat, Species == "red" & treatment == "ambient")
# points(dum$rdref ~ dum$q10, pch = 1, col = "black")
# 
# plot(dat$rdref ~ dat$q10, pch = NA)
# dum<-subset(dat, Species == "white" & treatment == "warmed")
# points(dum$rdref ~ dum$q10, pch = 2, col = "firebrick")
# dum<-subset(dat, Species == "white" & treatment == "ambient")
# points(dum$rdref ~ dum$q10, pch = 2, col = "black")
# 
# plot(dat$rdref ~ dat$q10, pch = NA)
# dum<-subset(dat, Species == "black" & treatment == "warmed")
# points(dum$rdref ~ dum$q10, pch = 3, col = "firebrick")
# dum<-subset(dat, Species == "black" & treatment == "ambient")
# points(dum$rdref ~ dum$q10, pch = 3, col = "black")


