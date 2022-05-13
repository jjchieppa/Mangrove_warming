library(lattice);library(doBy); library(lubridate)
library(minpack.lm); library(effects); library(plotrix)
library(lsmeans); library(multcomp); library(multcompView)
library(lme4)

rm(list=ls()) # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
dev.off()     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
cat("\f")     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# in data ####

setwd("C:/Users/jjchi/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis")

source<-read.csv("Mangrove morphology data_error2.csv")
source$Date<-as.Date(source$Date, format = "%m/%d/%Y")
source<-subset(source, Dead != "Y")
source$Date<-as.factor(source$Date)

df<-summaryBy(LDM + SDM + RDM + LMF + SMF + RMF + TDM + TLA_cm2 + LAR + SLA ~ 
                Species + Source + Treatment,
              FUN = c(mean, std.error), na.rm = T, source)

# plots ####

tiff(file = "V+A_Mangrove dry mass.tiff", height = 7, width = 10, res = 600, units = "in", compression = "zip+p")

par(mfrow = c(1,2), mar = c(1, 1.5, 0, 2), omi = c(0.9, 0.8, 0.25, 0.01))

# panel a) w model ####

xx<-c(-500,500); yy<-xx
plot(yy ~ xx, pch = NA, xlim = c(0,16), ylim = c(0,100),xaxt="n",yaxt="n",xlab="",ylab=""); rm(xx,yy)
axis(2, at = seq(0,100,20), cex.axis = 1.2, las = 2)
axis(1, at = c(1.5,5.5,10.5,14.5),
     labels = c("Ambient","Warmed","Ambient","Warmed"))
mtext(side = 2, "Dry Mass (g)", cex = 1.2, padj = -4)
legend("topleft", "a)", bty = "n", cex = 1.1)
legend("topright", horiz = F, bty = "n", cex = 1.1, pt.cex = 1.5,
       pch = 15, c("Leaf","Stem","Root"),
       col = c("grey90","grey70","grey50"))
mtext(side = 3, expression(italic(Avicennia)))

dum<-subset(source, Species == "AG")
m<-lm(TDM ~ Treatment * Source, dum); Anova(m)
cld(emmeans(m, ~Treatment:Source))


# AG BZ AM ####

dat<-subset(df, Species == "AG" & Source == "BZ" & Treatment == "Ambient")
xx<-0; yy<-3
rect(xx, 00, yy, dat$RDM.mean, col = "grey50")
rect(xx, dat$RDM.mean, yy, (dat$RDM.mean+dat$SDM.mean), col = "grey70")
rect(xx, (dat$RDM.mean+dat$SDM.mean), yy, (dat$RDM.mean+dat$SDM.mean+dat$LDM.mean), col = "grey90")
ablineclip(v = as.numeric((xx+yy)/2), y1 = as.numeric(dat$RDM.mean+dat$SDM.mean+dat$LDM.mean-dat$TDM.std.error), y2 = as.numeric(dat$RDM.mean+dat$SDM.mean+dat$LDM.mean+dat$TDM.std.error))
text(as.numeric((xx+yy)/2), (dat$RDM.mean/2), "b", cex = 1.5) #root
# text(as.numeric((xx+yy)/2), (dat$RDM.mean+dat$SDM.mean/2), "a", cex = 1.5) #stem
text(as.numeric((xx+yy)/2), (dat$RDM.mean+dat$SDM.mean+dat$LDM.mean/2), "b", cex = 1.5) #leaf
text(as.numeric((xx+yy)/2), (dat$RDM.mean+dat$SDM.mean+dat$LDM.mean+10), "b", cex = 1.5) #total

# AG BZ WA ####

dat<-subset(df, Species == "AG" & Source == "BZ" & Treatment == "Warmed")
xx<-4; yy<-7
rect(xx, 00, yy, dat$RDM.mean, col = "grey50"); rect(xx, 00, yy, dat$RDM.mean, density = 5)
rect(xx, dat$RDM.mean, yy, (dat$RDM.mean+dat$SDM.mean), col = "grey70"); rect(xx, dat$RDM.mean, yy, (dat$RDM.mean+dat$SDM.mean), density = 5)
rect(xx, (dat$RDM.mean+dat$SDM.mean), yy, (dat$RDM.mean+dat$SDM.mean+dat$LDM.mean), col = "grey90"); rect(xx, (dat$RDM.mean+dat$SDM.mean), yy, (dat$RDM.mean+dat$SDM.mean+dat$LDM.mean), density = 5)
ablineclip(v = as.numeric((xx+yy)/2), y1 = as.numeric(dat$RDM.mean+dat$SDM.mean+dat$LDM.mean-dat$TDM.std.error), y2 = as.numeric(dat$RDM.mean+dat$SDM.mean+dat$LDM.mean+dat$TDM.std.error))
text(as.numeric((xx+yy)/2), (dat$RDM.mean/2), "c", cex = 1.5) #root
# text(as.numeric((xx+yy)/2), (dat$RDM.mean+dat$SDM.mean/2), "a", cex = 1.5) #stem
text(as.numeric((xx+yy)/2), (dat$RDM.mean+dat$SDM.mean+dat$LDM.mean/2), "c", cex = 1.5) #leaf
text(as.numeric((xx+yy)/2), (dat$RDM.mean+dat$SDM.mean+dat$LDM.mean+10), "c", cex = 1.5) #total

# AG FL AM ####

dat<-subset(df, Species == "AG" & Source == "FL" & Treatment == "Ambient")
xx<-9; yy<-12
rect(xx, 00, yy, dat$RDM.mean, col = "grey50")
rect(xx, dat$RDM.mean, yy, (dat$RDM.mean+dat$SDM.mean), col = "grey70")
rect(xx, (dat$RDM.mean+dat$SDM.mean), yy, (dat$RDM.mean+dat$SDM.mean+dat$LDM.mean), col = "grey90")
ablineclip(v = as.numeric((xx+yy)/2), y1 = as.numeric(dat$RDM.mean+dat$SDM.mean+dat$LDM.mean-dat$TDM.std.error), y2 = as.numeric(dat$RDM.mean+dat$SDM.mean+dat$LDM.mean+dat$TDM.std.error))
text(as.numeric((xx+yy)/2), (dat$RDM.mean/2), "a", cex = 1.5) #root
# text(as.numeric((xx+yy)/2), (dat$RDM.mean+dat$SDM.mean/2), "a", cex = 1.5) #stem
text(as.numeric((xx+yy)/2), (dat$RDM.mean+dat$SDM.mean+dat$LDM.mean/2), "a", cex = 1.5) #leaf
text(as.numeric((xx+yy)/2), (dat$RDM.mean+dat$SDM.mean+dat$LDM.mean+10), "a", cex = 1.5) #total

# AG FL WA ####

dat<-subset(df, Species == "AG" & Source == "FL" & Treatment == "Warmed")
xx<-13; yy<-16
rect(xx, 00, yy, dat$RDM.mean, col = "grey50"); rect(xx, 00, yy, dat$RDM.mean, density = 5)
rect(xx, dat$RDM.mean, yy, (dat$RDM.mean+dat$SDM.mean), col = "grey70"); rect(xx, dat$RDM.mean, yy, (dat$RDM.mean+dat$SDM.mean), density = 5)
rect(xx, (dat$RDM.mean+dat$SDM.mean), yy, (dat$RDM.mean+dat$SDM.mean+dat$LDM.mean), col = "grey90"); rect(xx, (dat$RDM.mean+dat$SDM.mean), yy, (dat$RDM.mean+dat$SDM.mean+dat$LDM.mean), density = 5)
ablineclip(v = as.numeric((xx+yy)/2), y1 = as.numeric(dat$RDM.mean+dat$SDM.mean+dat$LDM.mean-dat$TDM.std.error), y2 = as.numeric(dat$RDM.mean+dat$SDM.mean+dat$LDM.mean+dat$TDM.std.error))
text(as.numeric((xx+yy)/2), (dat$RDM.mean/2), "a", cex = 1.5) #root
# text(as.numeric((xx+yy)/2), (dat$RDM.mean+dat$SDM.mean/2), "a", cex = 1.5) #stem
text(as.numeric((xx+yy)/2), (dat$RDM.mean+dat$SDM.mean+dat$LDM.mean/2), "a", cex = 1.5) #leaf
text(as.numeric((xx+yy)/2), (dat$RDM.mean+dat$SDM.mean+dat$LDM.mean+10), "a", cex = 1.5) #total

axis(1, at = c(3.5,12.5), labels = c("Tropical","Subtropical"), tick = 0, padj = 2)

# panel b) w model ####

xx<-c(-500,500); yy<-xx
plot(yy ~ xx, pch = NA, xlim = c(0,16), ylim = c(0,20),xaxt="n",yaxt="n",xlab="",ylab=""); rm(xx,yy)
axis(2, at = seq(0,20,4), cex.axis = 1.2, las = 2)
axis(1, at = c(1.5,5.5,10.5,14.5),
     labels = c("Ambient","Warmed","Ambient","Warmed"))
legend("topleft", "b)", bty = "n", cex = 1.1)
mtext(side = 3, expression(italic(Rhizophora)))

dum<-subset(source, Species == "RM")
dum$trt<-interaction(dum$Treatment, dum$Source)
m<-lm(TDM ~ Treatment * Source, dum); Anova(m)
m<-lm(TDM ~ trt, dum); Anova(m)
t1<-summary(glht(m, linfct = mcp(trt = "Tukey")))
n1<-summary(t1, test = adjusted("single-step"))
cld(n1)

# RM BZ AM ####

dat<-subset(df, Species == "RM" & Source == "BZ" & Treatment == "Ambient")
xx<-0; yy<-3
rect(xx, 00, yy, dat$RDM.mean, col = "grey50")
rect(xx, dat$RDM.mean, yy, (dat$RDM.mean+dat$SDM.mean), col = "grey70")
rect(xx, (dat$RDM.mean+dat$SDM.mean), yy, (dat$RDM.mean+dat$SDM.mean+dat$LDM.mean), col = "grey90")
ablineclip(v = as.numeric(1), y1 = as.numeric(dat$RDM.mean+dat$SDM.mean+dat$LDM.mean-dat$TDM.std.error), y2 = as.numeric(dat$RDM.mean+dat$SDM.mean+dat$LDM.mean+dat$TDM.std.error))
text(as.numeric((xx+yy)/2), (dat$RDM.mean/2), "b", cex = 1.5) #root
text(as.numeric((xx+yy)/2), (dat$RDM.mean+dat$SDM.mean/2), "b", cex = 1.5) #stem
text(as.numeric((xx+yy)/2), (dat$RDM.mean+dat$SDM.mean+dat$LDM.mean/2), "a", cex = 1.5) #leaf
# text(as.numeric((xx+yy)/2), (dat$RDM.mean+dat$SDM.mean+dat$LDM.mean+10), "b", cex = 1.5) #total

# RM BZ WA ####

dat<-subset(df, Species == "RM" & Source == "BZ" & Treatment == "Warmed")
xx<-4; yy<-7
rect(xx, 00, yy, dat$RDM.mean, col = "grey50"); rect(xx, 00, yy, dat$RDM.mean, density = 5)
rect(xx, dat$RDM.mean, yy, (dat$RDM.mean+dat$SDM.mean), col = "grey70"); rect(xx, dat$RDM.mean, yy, (dat$RDM.mean+dat$SDM.mean), density = 5)
rect(xx, (dat$RDM.mean+dat$SDM.mean), yy, (dat$RDM.mean+dat$SDM.mean+dat$LDM.mean), col = "grey90"); rect(xx, (dat$RDM.mean+dat$SDM.mean), yy, (dat$RDM.mean+dat$SDM.mean+dat$LDM.mean), density = 5)
ablineclip(v = as.numeric((xx+yy)/2), y1 = as.numeric(dat$RDM.mean+dat$SDM.mean+dat$LDM.mean-dat$TDM.std.error), y2 = as.numeric(dat$RDM.mean+dat$SDM.mean+dat$LDM.mean+dat$TDM.std.error))
text(as.numeric((xx+yy)/2), (dat$RDM.mean/2), "c", cex = 1.5) #root
text(as.numeric((xx+yy)/2), (dat$RDM.mean+dat$SDM.mean/2), "ab", cex = 1.5) #stem
text(as.numeric((xx+yy)/2), (dat$RDM.mean+dat$SDM.mean+dat$LDM.mean/2), "a", cex = 1.5) #leaf
# text(as.numeric((xx+yy)/2), (dat$RDM.mean+dat$SDM.mean+dat$LDM.mean+10), "c", cex = 1.5) #total

# RM FL AM ####

dat<-subset(df, Species == "RM" & Source == "FL" & Treatment == "Ambient")
xx<-9; yy<-12
rect(xx, 00, yy, dat$RDM.mean, col = "grey50")
rect(xx, dat$RDM.mean, yy, (dat$RDM.mean+dat$SDM.mean), col = "grey70")
rect(xx, (dat$RDM.mean+dat$SDM.mean), yy, (dat$RDM.mean+dat$SDM.mean+dat$LDM.mean), col = "grey90")
ablineclip(v = as.numeric((xx+yy)/2), y1 = as.numeric(dat$RDM.mean+dat$SDM.mean+dat$LDM.mean-dat$TDM.std.error), y2 = as.numeric(dat$RDM.mean+dat$SDM.mean+dat$LDM.mean+dat$TDM.std.error))
text(as.numeric((xx+yy)/2), (dat$RDM.mean/2), "a", cex = 1.5) #root
text(as.numeric((xx+yy)/2), (dat$RDM.mean+dat$SDM.mean/2), "a", cex = 1.5) #stem
text(as.numeric((xx+yy)/2), (dat$RDM.mean+dat$SDM.mean+dat$LDM.mean/2), "b", cex = 1.5) #leaf
# text(as.numeric((xx+yy)/2), (dat$RDM.mean+dat$SDM.mean+dat$LDM.mean+10), "a", cex = 1.5) #total

# RM FL WA ####

dat<-subset(df, Species == "RM" & Source == "FL" & Treatment == "Warmed")
xx<-13; yy<-16
rect(xx, 00, yy, dat$RDM.mean, col = "grey50"); rect(xx, 00, yy, dat$RDM.mean, density = 5)
rect(xx, dat$RDM.mean, yy, (dat$RDM.mean+dat$SDM.mean), col = "grey70"); rect(xx, dat$RDM.mean, yy, (dat$RDM.mean+dat$SDM.mean), density = 5)
rect(xx, (dat$RDM.mean+dat$SDM.mean), yy, (dat$RDM.mean+dat$SDM.mean+dat$LDM.mean), col = "grey90"); rect(xx, (dat$RDM.mean+dat$SDM.mean), yy, (dat$RDM.mean+dat$SDM.mean+dat$LDM.mean), density = 5)
ablineclip(v = as.numeric((xx+yy)/2), y1 = as.numeric(dat$RDM.mean+dat$SDM.mean+dat$LDM.mean-dat$TDM.std.error), y2 = as.numeric(dat$RDM.mean+dat$SDM.mean+dat$LDM.mean+dat$TDM.std.error))
text(as.numeric((xx+yy)/2), (dat$RDM.mean/2), "a", cex = 1.5) #root
text(as.numeric((xx+yy)/2), (dat$RDM.mean+dat$SDM.mean/2), "b", cex = 1.5) #stem
text(as.numeric((xx+yy)/2), (dat$RDM.mean+dat$SDM.mean+dat$LDM.mean/2), "a", cex = 1.5) #leaf
# text(as.numeric((xx+yy)/2), (dat$RDM.mean+dat$SDM.mean+dat$LDM.mean+10), "a", cex = 1.5) #total

axis(1, at = c(3.5,12.5), labels = c("Tropical","Subtropical"), tick = 0, padj = 2)

# off ####
mtext(side = 1, outer = T, cex = 1.5, expression(Temperature~Treatment~"&"~Population), padj = 3)
dev.off()