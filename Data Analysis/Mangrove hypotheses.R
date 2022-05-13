# in data ####

setwd("C:/Users/jjchi/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis")

rm(list=ls()) # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
dev.off()     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
cat("\f")     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


# plot ####

tiff(file = "Mangrove hypotheses.tiff", height = 8, width = 8, res = 600, units = "in", compression = "zip+p")

par(mfrow = c(2,2), omi = c(1,0.5,0.1,0.1), mar = c(2,2,2,2))

# Productivity ####

xp<-c(1,3,5,7,11,13,15,17)
prod1<-c(5,6,5,8,5,6.5,5,6.5)

src<-c("FL","FL","BZ","BZ","FL","FL","BZ","BZ")
trt<-c("A","W","A","W","A","W","A","W")
pdf<-data.frame(xp, prod1, src, trt)
rm(xp, prod1, src, trt, df)

plot(1 ~ 0, xlim = c(0,18), ylim = c(3,10), pch = NA, xaxt="n",yaxt="n")
legend("topleft", "a)", adj = 1, bty = "n", cex = 1.3)
# legend("bottomright", pch = c(17,25), col = "black", pt.bg = "black", c("Subtropical","Tropical"), cex = 1.3, bty = "n", pt.cex = c(1.4,1.1))
# legend("bottomleft", pch = 15, pt.cex = 1.5, col = c("dodgerblue4","firebrick"), c("Ambient","Warmed"), cex = 1.3, bty = "n")

points(prod1 ~ xp, dat = pdf[1:2,], type = "l", lty = 2, lwd = 2, col = "grey50")
points(prod1 ~ xp, data = pdf[1,], pch = 17, col = "dodgerblue4", cex = 3)
points(prod1 ~ xp, data = pdf[2,], pch = 17, col = "firebrick", cex = 3)
points(prod1 ~ xp, dat = pdf[3:4,], type = "l", lty = 1, lwd = 2, col = "grey50")
points(prod1 ~ xp, data = pdf[3,], pch = 25, bg = "dodgerblue4", col = "dodgerblue4", cex = 3)
points(prod1 ~ xp, data = pdf[4,], pch = 25, bg = "firebrick", col = "firebrick", cex = 3)
abline(v = 9, lty = 2)
points(prod1 ~ xp, dat = pdf[5:6,], type = "l", lty = 2, lwd = 2, col = "grey50")
points(prod1 ~ xp, data = pdf[5,], pch = 17, col = "dodgerblue4", cex = 3)
points(prod1 ~ xp, data = pdf[6,], pch = 17, col = "firebrick", cex = 3)
points(prod1 ~ xp, dat = pdf[7:8,], type = "l", lty = 1, lwd = 2, col = "grey50")
points(prod1 ~ xp, data = pdf[7,], pch = 25, bg = "dodgerblue4", col = "dodgerblue4", cex = 3)
points(prod1 ~ xp, data = pdf[8,], pch = 25, bg = "firebrick", col = "firebrick", cex = 3)


mtext(side = 2, expression(Productivity), cex = 1.25, padj = -0.5)
# mtext(side = 1, expression(Population), cex = 1.25, padj = 4.5)
axis(2, at = c(3,9), labels = c("-","+"), cex.axis = 2, las = 2)
axis(1, at = c(2,6,12,16), labels = c("Subtropical","Tropical","Subtropical","Tropical"), las = 2)

text(4, 9.5, "Genetic")
text(4, 9, "differentiation")
text(14, 9.5, "No genetic")
text(14, 9, "differentiation")

rm(pdf)

# Seasonal ####

# rm(list=ls())
plot(1~0,xlim=c(0,100),ylim=c(-10,100),pch=NA,axes=F,xlab="",ylab="")

t5<-seq(10,80,10); r<-rep(50,8)
df<-data.frame(t5,r); rm(t5,r)
points(df$r ~ df$t5, type = "l", lty = 1, lwd = 2, col = "dodgerblue4")
points(r ~ t5, data = df[c(1,8),], pch = 25, cex = 2, col = "dodgerblue4", bg = "dodgerblue4")

t5<-seq(20,90,10); r<-rep(49,8)
df<-data.frame(t5,r); rm(t5,r)
points(df$r ~ df$t5, type = "l", lty = 1, lwd = 2, col = "firebrick")
points(r ~ t5, data = df[c(1,8),], pch = 25, cex = 2, col = "firebrick", bg = "firebrick")

t5<-seq(10,80,10); r<-seq(70,30,-40/7)
df<-data.frame(t5,r); rm(t5,r)
points(df$r ~ df$t5, type = "l", lty = 2, lwd = 2, col = "dodgerblue4")
points(r ~ t5, data = df[c(1,8),], pch = 17, cex = 2, col = "dodgerblue4")

t5<-seq(20,90,10); r<-seq(65,25,-40/7)
df<-data.frame(t5,r); rm(t5,r)
points(df$r ~ df$t5, type = "l", lty = 2, lwd = 2, col = "firebrick")
points(r ~ t5, data = df[c(1,8),], pch = 17, cex = 2, col = "firebrick")

legend("topleft", "b)", adj = 1, bty = "n", cex = 1.3)
# legend("topright", lty = c(2,1), pch = c(17,25), pt.bg = "black", pt.cex = c(1.4,1.1), lwd = 2, c("Subtropical","Tropical"), cex = 1.3, bty = "n")
# legend("bottomleft", pch = 15, pt.cex = 1.5, col = c("dodgerblue4","firebrick"), c("Ambient","Warmed"), cex = 1.3, bty = "n")

mtext(side = 1, expression(Prevailing~Temperature~(degree*C)), cex = 1.25, padj = 1)
mtext(side = 2, expression(Respiration~at~25*degree*C), cex = 1.25, padj = -0.5)

arrows(70,10,70,20, length = 0.1)
text(70, 0, "Seasonal acclimation")

arrows(70,75,70,65, length = 0.1)
text(70, 85, "No seasonal acclimation")

# arrows(70,70,70,40, length = 0.1)
# text(70, 75, "Acclimation to warming")
box()


# legend ####

plot(1~0, axes = F, pch = NA)

legend("center", horiz = F, c("Subtropical","Tropical","Ambient","Warmed"),
       pch = c(17,25,15,15), col = c("black","black","dodgerblue4","firebrick"), pt.bg = "black",
       cex = 2, lty = c(2,1,NA,NA), lwd = 2, pt.cex = c(2.4,2.1,2.1,2.1))

# Frost ####

xp<-c(1,3,5,7,11,13,15,17)
prod1<-c(7,3,5,1,7,3,7,3)
src<-c("FL","FL","BZ","BZ","FL","FL","BZ","BZ")
trt<-c("A","W","A","W","A","W","A","W")
pdf<-data.frame(xp, prod1, src, trt)
rm(xp, prod1, src, trt)

plot(1 ~ 0, xlim = c(0,18), ylim = c(0,10), pch = NA, xaxt="n",yaxt="n")
legend("topleft", "c)", adj = 1, bty = "n", cex = 1.3)

points(prod1 ~ xp, dat = pdf[1:2,], type = "l", lty = 2, lwd = 2, col = "grey50")
points(prod1 ~ xp, data = pdf[1,], pch = 17, col = "dodgerblue4", cex = 3)
points(prod1 ~ xp, data = pdf[2,], pch = 17, col = "firebrick", cex = 3)
points(prod1 ~ xp, dat = pdf[3:4,], type = "l", lty = 1, lwd = 2, col = "grey50")
points(prod1 ~ xp, data = pdf[3,], pch = 25, bg = "dodgerblue4", col = "dodgerblue4", cex = 3)
points(prod1 ~ xp, data = pdf[4,], pch = 25, bg = "firebrick", col = "firebrick", cex = 3)
abline(v = 9, lty = 2)
points(prod1 ~ xp, dat = pdf[5:6,], type = "l", lty = 2, lwd = 2, col = "grey50")
points(prod1 ~ xp, data = pdf[5,], pch = 17, col = "dodgerblue4", cex = 3)
points(prod1 ~ xp, data = pdf[6,], pch = 17, col = "firebrick", cex = 3)
points(prod1 ~ xp, dat = pdf[7:8,], type = "l", lty = 1, lwd = 2, col = "grey50")
points(prod1 ~ xp, data = pdf[7,], pch = 25, bg = "dodgerblue4", col = "dodgerblue4", cex = 3)
points(prod1 ~ xp, data = pdf[8,], pch = 25, bg = "firebrick", col = "firebrick", cex = 3)

mtext(side = 2, expression(Freeze~Tolerance), cex = 1.25, padj = -0.75)
mtext(side = 1, expression(Population), cex = 1.25, padj = 5)
axis(2, at = c(1,9), labels = c("-","+"), cex.axis = 2, las = 2)
axis(1, at = c(2,6,12,16), labels = c("Subtropical","Tropical","Subtropical","Tropical"), las = 2)

text(4, 9, "Genetic")
text(4, 8.5, "differentiation")
text(14, 9, "No genetic")
text(14, 8.5, "differentiation")


# off ####
dev.off()