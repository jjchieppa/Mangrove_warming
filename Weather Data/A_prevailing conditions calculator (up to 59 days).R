# start ####

library(lattice);library(doBy); library(lubridate)
library(minpack.lm); library(effects); library(plotrix)
library(car); library(sjPlot); library(suncalc)
library(chron); library(reshape)

rm(list=ls())
dev.off()
cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

work<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp" # work
deets<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Data Analysis/Model Outputs/deets"
envi<-"C:/Users/jjc0022/OneDrive - Auburn University/Other Projects/Belize Florida Mangroves Exp/Weather Data"

# data in ####

setwd(deets)
source<-read.csv("MangroveRvT_LPRmod_Rarea and Rmass at 25C.csv")
source<-subset(source, KEEP == "Y")
source$Date<-as.Date(source$Date, format = "%m/%d/%Y")
setwd(work)
trts<-read.csv("Mangrove Treatment Assignments.csv")
source<-merge(source, trts, by = "UserIDs_in", all = F)
rm(trts)
source$Treatment<-as.factor(substr(source$Treatment, 1, 4))

source$Date<-as.Date(source$Date, format = "%Y-%m-%d") # Double checks date format!
source$MoYr<-floor_date(source$Date, "month")
source$MoYr<-source$MoYr+25
source$LMA_g.m2<-source$LMA_kg.m2*1000
source$Timepoint<-as.factor(source$MoYr)

# env data in ####

setwd(envi)

env<-read.csv("Mangrove RVT weather data_all.csv")
env$Treatment<-as.factor(env$Treatment)
env$Time<-times(env$Time)
env$Date<-as.Date(env$Date, format = "%m/%d/%Y")
env$SVP_kPa<-(610.78*(2.71828^(env$Temp_C/(env$Temp_C+238.3) * 17.2694)))/1000
env$VPD_kPa<-env$SVP_kPa * (1 - (env$RH_perc/100))
env$Timestamp2<-as.POSIXct(paste(env$Date, env$Time), format="%Y-%m-%d %H:%M:%S")
sun<-getSunlightPosition(date = as.POSIXct(env$Timestamp2, tz = "UTC"), lat = 30.259630, lon = -81.504205)
env<-cbind(env,sun); rm(sun)
env$sDate<-as.factor(env$Date)

# env_warm<-subset(env, Treatment == "Warm")
# env_ambi<-subset(env, Treatment == "Ambi")
# new_warm_env<-subset(env_warm, Block == "")
# new_ambi_env<-subset(env_ambi, Block == "")
new_env<-data.frame(Date=as.numeric(),
                       Treatment=as.numeric(),
                       variable=as.numeric(),
                       value=as.numeric())

source$sDate<-as.factor(source$Date)
# warm.r<-subset(source, Treatment == "Ambi")
# ambi.r<-subset(source, Treatment == "Warm")
# rm(source)

# warm.r.dates<-unique(warm.r$sDate)
# ambi.r.dates<-unique(ambi.r$sDate)
dates<-unique(source$sDate)

# warm loop ####

for (dates in unique(source$sDate)){
  dat<-subset(env, env$sDate == dates)
  mnd<-min(dat$Date, na.rm = T)
  # dates ####
  dat01<-subset(env, env$Date == mnd-1)
  dat02<-subset(env, env$Date == mnd-2)
  dat03<-subset(env, env$Date == mnd-3)
  dat04<-subset(env, env$Date == mnd-4)
  dat05<-subset(env, env$Date == mnd-5)
  dat06<-subset(env, env$Date == mnd-6)
  dat07<-subset(env, env$Date == mnd-7)
  dat08<-subset(env, env$Date == mnd-8)
  dat09<-subset(env, env$Date == mnd-9)
  dat10<-subset(env, env$Date == mnd-10)
  dat11<-subset(env, env$Date == mnd-11)
  dat12<-subset(env, env$Date == mnd-12)
  dat13<-subset(env, env$Date == mnd-13)
  dat14<-subset(env, env$Date == mnd-14)
  dat15<-subset(env, env$Date == mnd-15)
  dat16<-subset(env, env$Date == mnd-16)
  dat17<-subset(env, env$Date == mnd-17)
  dat18<-subset(env, env$Date == mnd-18)
  dat19<-subset(env, env$Date == mnd-19)
  dat20<-subset(env, env$Date == mnd-20)
  dat21<-subset(env, env$Date == mnd-21)
  dat22<-subset(env, env$Date == mnd-22)
  dat23<-subset(env, env$Date == mnd-23)
  dat24<-subset(env, env$Date == mnd-24)
  dat25<-subset(env, env$Date == mnd-25)
  dat26<-subset(env, env$Date == mnd-26)
  dat27<-subset(env, env$Date == mnd-27)
  dat28<-subset(env, env$Date == mnd-28)
  dat29<-subset(env, env$Date == mnd-29)
  dat30<-subset(env, env$Date == mnd-30)
  dat31<-subset(env, env$Date == mnd-31)
  dat32<-subset(env, env$Date == mnd-32)
  dat33<-subset(env, env$Date == mnd-33)
  dat34<-subset(env, env$Date == mnd-34)
  dat35<-subset(env, env$Date == mnd-35)
  dat36<-subset(env, env$Date == mnd-36)
  dat37<-subset(env, env$Date == mnd-37)
  dat38<-subset(env, env$Date == mnd-38)
  dat39<-subset(env, env$Date == mnd-39)
  # dat40<-subset(env, env$Date == mnd-40)
  # dat41<-subset(env, env$Date == mnd-41)
  # dat42<-subset(env, env$Date == mnd-42)
  # dat43<-subset(env, env$Date == mnd-43)
  # dat44<-subset(env, env$Date == mnd-44)
  # dat45<-subset(env, env$Date == mnd-45)
  # dat46<-subset(env, env$Date == mnd-46)
  # dat47<-subset(env, env$Date == mnd-47)
  # dat48<-subset(env, env$Date == mnd-48)
  # dat49<-subset(env, env$Date == mnd-49)
  # dat50<-subset(env, env$Date == mnd-50)
  # dat51<-subset(env, env$Date == mnd-51)
  # dat52<-subset(env, env$Date == mnd-52)
  # dat53<-subset(env, env$Date == mnd-53)
  # dat54<-subset(env, env$Date == mnd-54)
  # dat55<-subset(env, env$Date == mnd-55)
  # dat56<-subset(env, env$Date == mnd-56)
  # dat57<-subset(env, env$Date == mnd-57)
  # dat58<-subset(env, env$Date == mnd-58)
  # dat59<-subset(env, env$Date == mnd-59)
  # end dates ####
  # binding ####
  t01<-dat01
  t02<-rbind(t01,dat02)
  t03<-rbind(t02,dat03)
  t04<-rbind(t03,dat04)
  t05<-rbind(t04,dat05)
  t06<-rbind(t05,dat06)
  t07<-rbind(t06,dat07)
  t08<-rbind(t07,dat08)
  t09<-rbind(t08,dat09)
  t10<-rbind(t09,dat10)
  t11<-rbind(t10,dat11)
  t12<-rbind(t11,dat12)
  t13<-rbind(t12,dat13)
  t14<-rbind(t13,dat14)
  t15<-rbind(t14,dat15)
  t16<-rbind(t15,dat16)
  t17<-rbind(t16,dat17)
  t18<-rbind(t17,dat18)
  t19<-rbind(t18,dat19)
  t20<-rbind(t19,dat20)
  t21<-rbind(t20,dat21)
  t22<-rbind(t21,dat22)
  t23<-rbind(t22,dat23)
  t24<-rbind(t23,dat24)
  t25<-rbind(t24,dat25)
  t26<-rbind(t25,dat26)
  t27<-rbind(t26,dat27)
  t28<-rbind(t27,dat28)
  t29<-rbind(t28,dat29)
  t30<-rbind(t29,dat30)
  t31<-rbind(t30,dat31)
  t32<-rbind(t31,dat32)
  t33<-rbind(t32,dat33)
  t34<-rbind(t33,dat34)
  t35<-rbind(t34,dat35)
  t36<-rbind(t35,dat36)
  t37<-rbind(t36,dat37)
  t38<-rbind(t37,dat38)
  t39<-rbind(t38,dat39)
  # t40<-rbind(t39,dat40)
  # t41<-rbind(t40,dat41)
  # t42<-rbind(t41,dat42)
  # t43<-rbind(t42,dat43)
  # t44<-rbind(t43,dat44)
  # t45<-rbind(t44,dat45)
  # t46<-rbind(t45,dat46)
  # t47<-rbind(t46,dat47)
  # t48<-rbind(t47,dat48)
  # t49<-rbind(t48,dat49)
  # t50<-rbind(t49,dat50)
  # t51<-rbind(t50,dat51)
  # t52<-rbind(t51,dat52)
  # t53<-rbind(t52,dat53)
  # t54<-rbind(t53,dat54)
  # t55<-rbind(t54,dat55)
  # t56<-rbind(t55,dat56)
  # t57<-rbind(t56,dat57)
  # t58<-rbind(t57,dat58)
  # t59<-rbind(t58,dat59)
  # end binding ####
  # day-night-date ####
  t01$Date<-max(t01$Date, na.rm = T)+1; d01<-subset(t01, altitude > 0); n01<-subset(t01, altitude < 0)
  t02$Date<-max(t02$Date, na.rm = T)+1; d02<-subset(t02, altitude > 0); n02<-subset(t02, altitude < 0)
  t03$Date<-max(t03$Date, na.rm = T)+1; d03<-subset(t03, altitude > 0); n03<-subset(t03, altitude < 0)
  t04$Date<-max(t04$Date, na.rm = T)+1; d04<-subset(t04, altitude > 0); n04<-subset(t04, altitude < 0)
  t05$Date<-max(t05$Date, na.rm = T)+1; d05<-subset(t05, altitude > 0); n05<-subset(t05, altitude < 0)
  t06$Date<-max(t06$Date, na.rm = T)+1; d06<-subset(t06, altitude > 0); n06<-subset(t06, altitude < 0)
  t07$Date<-max(t07$Date, na.rm = T)+1; d07<-subset(t07, altitude > 0); n07<-subset(t07, altitude < 0)
  t08$Date<-max(t08$Date, na.rm = T)+1; d08<-subset(t08, altitude > 0); n08<-subset(t08, altitude < 0)
  t09$Date<-max(t09$Date, na.rm = T)+1; d09<-subset(t09, altitude > 0); n09<-subset(t09, altitude < 0)

  t10$Date<-max(t10$Date, na.rm = T)+1; d10<-subset(t10, altitude > 0); n10<-subset(t10, altitude < 0)
  t11$Date<-max(t11$Date, na.rm = T)+1; d11<-subset(t11, altitude > 0); n11<-subset(t11, altitude < 0)
  t12$Date<-max(t12$Date, na.rm = T)+1; d12<-subset(t12, altitude > 0); n12<-subset(t12, altitude < 0)
  t13$Date<-max(t13$Date, na.rm = T)+1; d13<-subset(t13, altitude > 0); n13<-subset(t13, altitude < 0)
  t14$Date<-max(t14$Date, na.rm = T)+1; d14<-subset(t14, altitude > 0); n14<-subset(t14, altitude < 0)
  t15$Date<-max(t15$Date, na.rm = T)+1; d15<-subset(t15, altitude > 0); n15<-subset(t15, altitude < 0)
  t16$Date<-max(t16$Date, na.rm = T)+1; d16<-subset(t16, altitude > 0); n16<-subset(t16, altitude < 0)
  t17$Date<-max(t17$Date, na.rm = T)+1; d17<-subset(t17, altitude > 0); n17<-subset(t17, altitude < 0)
  t18$Date<-max(t18$Date, na.rm = T)+1; d18<-subset(t18, altitude > 0); n18<-subset(t18, altitude < 0)
  t19$Date<-max(t19$Date, na.rm = T)+1; d19<-subset(t19, altitude > 0); n19<-subset(t19, altitude < 0)

  t20$Date<-max(t20$Date, na.rm = T)+1; d20<-subset(t20, altitude > 0); n20<-subset(t20, altitude < 0)
  t21$Date<-max(t21$Date, na.rm = T)+1; d21<-subset(t21, altitude > 0); n21<-subset(t21, altitude < 0)
  t22$Date<-max(t22$Date, na.rm = T)+1; d22<-subset(t22, altitude > 0); n22<-subset(t22, altitude < 0)
  t23$Date<-max(t23$Date, na.rm = T)+1; d23<-subset(t23, altitude > 0); n23<-subset(t23, altitude < 0)
  t24$Date<-max(t24$Date, na.rm = T)+1; d24<-subset(t24, altitude > 0); n24<-subset(t24, altitude < 0)
  t25$Date<-max(t25$Date, na.rm = T)+1; d25<-subset(t25, altitude > 0); n25<-subset(t25, altitude < 0)
  t26$Date<-max(t26$Date, na.rm = T)+1; d26<-subset(t26, altitude > 0); n26<-subset(t26, altitude < 0)
  t27$Date<-max(t27$Date, na.rm = T)+1; d27<-subset(t27, altitude > 0); n27<-subset(t27, altitude < 0)
  t28$Date<-max(t28$Date, na.rm = T)+1; d28<-subset(t28, altitude > 0); n28<-subset(t28, altitude < 0)
  t29$Date<-max(t29$Date, na.rm = T)+1; d29<-subset(t29, altitude > 0); n29<-subset(t29, altitude < 0)

  t30$Date<-max(t30$Date, na.rm = T)+1; d30<-subset(t30, altitude > 0); n30<-subset(t30, altitude < 0)
  t31$Date<-max(t31$Date, na.rm = T)+1; d31<-subset(t31, altitude > 0); n31<-subset(t31, altitude < 0)
  t32$Date<-max(t32$Date, na.rm = T)+1; d32<-subset(t32, altitude > 0); n32<-subset(t32, altitude < 0)
  t33$Date<-max(t33$Date, na.rm = T)+1; d33<-subset(t33, altitude > 0); n33<-subset(t33, altitude < 0)
  t34$Date<-max(t34$Date, na.rm = T)+1; d34<-subset(t34, altitude > 0); n34<-subset(t34, altitude < 0)
  t35$Date<-max(t35$Date, na.rm = T)+1; d35<-subset(t35, altitude > 0); n35<-subset(t35, altitude < 0)
  t36$Date<-max(t36$Date, na.rm = T)+1; d36<-subset(t36, altitude > 0); n36<-subset(t36, altitude < 0)
  t37$Date<-max(t37$Date, na.rm = T)+1; d37<-subset(t37, altitude > 0); n37<-subset(t37, altitude < 0)
  t38$Date<-max(t38$Date, na.rm = T)+1; d38<-subset(t38, altitude > 0); n38<-subset(t38, altitude < 0)
  t39$Date<-max(t39$Date, na.rm = T)+1; d39<-subset(t39, altitude > 0); n39<-subset(t39, altitude < 0)

  # t40$Date<-max(t20$Date, na.rm = T)+1; d40<-subset(t40, altitude > 0); n40<-subset(t40, altitude < 0)
  # t41$Date<-max(t21$Date, na.rm = T)+1; d41<-subset(t41, altitude > 0); n41<-subset(t41, altitude < 0)
  # t42$Date<-max(t22$Date, na.rm = T)+1; d42<-subset(t42, altitude > 0); n42<-subset(t42, altitude < 0)
  # t43$Date<-max(t23$Date, na.rm = T)+1; d43<-subset(t43, altitude > 0); n43<-subset(t43, altitude < 0)
  # t44$Date<-max(t24$Date, na.rm = T)+1; d44<-subset(t44, altitude > 0); n44<-subset(t44, altitude < 0)
  # t45$Date<-max(t25$Date, na.rm = T)+1; d45<-subset(t45, altitude > 0); n45<-subset(t45, altitude < 0)
  # t46$Date<-max(t26$Date, na.rm = T)+1; d46<-subset(t46, altitude > 0); n46<-subset(t46, altitude < 0)
  # t47$Date<-max(t27$Date, na.rm = T)+1; d47<-subset(t47, altitude > 0); n47<-subset(t47, altitude < 0)
  # t48$Date<-max(t28$Date, na.rm = T)+1; d48<-subset(t48, altitude > 0); n48<-subset(t48, altitude < 0)
  # t49$Date<-max(t29$Date, na.rm = T)+1; d49<-subset(t49, altitude > 0); n49<-subset(t49, altitude < 0)
  # 
  # t50$Date<-max(t20$Date, na.rm = T)+1; d50<-subset(t50, altitude > 0); n50<-subset(t50, altitude < 0)
  # t51$Date<-max(t21$Date, na.rm = T)+1; d51<-subset(t51, altitude > 0); n51<-subset(t51, altitude < 0)
  # t52$Date<-max(t22$Date, na.rm = T)+1; d52<-subset(t52, altitude > 0); n52<-subset(t52, altitude < 0)
  # t53$Date<-max(t23$Date, na.rm = T)+1; d53<-subset(t53, altitude > 0); n53<-subset(t53, altitude < 0)
  # t54$Date<-max(t24$Date, na.rm = T)+1; d54<-subset(t54, altitude > 0); n54<-subset(t54, altitude < 0)
  # t55$Date<-max(t25$Date, na.rm = T)+1; d55<-subset(t55, altitude > 0); n55<-subset(t55, altitude < 0)
  # t56$Date<-max(t26$Date, na.rm = T)+1; d56<-subset(t56, altitude > 0); n56<-subset(t56, altitude < 0)
  # t57$Date<-max(t27$Date, na.rm = T)+1; d57<-subset(t57, altitude > 0); n57<-subset(t57, altitude < 0)
  # t58$Date<-max(t28$Date, na.rm = T)+1; d58<-subset(t58, altitude > 0); n58<-subset(t58, altitude < 0)
  # t59$Date<-max(t29$Date, na.rm = T)+1; d59<-subset(t59, altitude > 0); n59<-subset(t59, altitude < 0)
  
  # end day-night-date ####
  # total summaries ####
  t01<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t01)
  t02<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t02)
  t03<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t03)
  t04<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t04)
  t05<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t05)
  t06<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t06)
  t07<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t07)
  t08<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t08)
  t09<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t09)
  
  t10<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t10)
  t11<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t11)
  t12<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t12)
  t13<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t13)
  t14<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t14)
  t15<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t15)
  t16<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t16)
  t17<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t17)
  t18<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t18)
  t19<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t19)
  
  t20<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t20)
  t21<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t21)
  t22<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t22)
  t23<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t23)
  t24<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t24)
  t25<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t25)
  t26<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t26)
  t27<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t27)
  t28<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t28)
  t29<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t29)
  
  t30<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t30)
  t31<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t31)
  t32<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t32)
  t33<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t33)
  t34<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t34)
  t35<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t35)
  t36<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t36)
  t37<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t37)
  t38<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t38)
  t39<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t39)
  
  # t40<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t40)
  # t41<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t41)
  # t42<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t42)
  # t43<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t43)
  # t44<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t44)
  # t45<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t45)
  # t46<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t46)
  # t47<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t47)
  # t48<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t48)
  # t49<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t49)
  # 
  # t50<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t50)
  # t51<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t51)
  # t52<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t52)
  # t53<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t53)
  # t54<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t54)
  # t55<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t55)
  # t56<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t56)
  # t57<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t57)
  # t58<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t58)
  # t59<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, t59)
  
  names(t01)[3]<-"T_t01x"; names(t01)[4]<-"V_t01x"; names(t01)[5]<-"T_t01m"; names(t01)[6]<-"V_t01m"
  names(t02)[3]<-"T_t02x"; names(t02)[4]<-"V_t02x"; names(t02)[5]<-"T_t02m"; names(t02)[6]<-"V_t02m"
  names(t03)[3]<-"T_t03x"; names(t03)[4]<-"V_t03x"; names(t03)[5]<-"T_t03m"; names(t03)[6]<-"V_t03m"
  names(t04)[3]<-"T_t04x"; names(t04)[4]<-"V_t04x"; names(t04)[5]<-"T_t04m"; names(t04)[6]<-"V_t04m"
  names(t05)[3]<-"T_t05x"; names(t05)[4]<-"V_t05x"; names(t05)[5]<-"T_t05m"; names(t05)[6]<-"V_t05m"
  names(t06)[3]<-"T_t06x"; names(t06)[4]<-"V_t06x"; names(t06)[5]<-"T_t06m"; names(t06)[6]<-"V_t06m"
  names(t07)[3]<-"T_t07x"; names(t07)[4]<-"V_t07x"; names(t07)[5]<-"T_t07m"; names(t07)[6]<-"V_t07m"
  names(t08)[3]<-"T_t08x"; names(t08)[4]<-"V_t08x"; names(t08)[5]<-"T_t08m"; names(t08)[6]<-"V_t08m"
  names(t09)[3]<-"T_t09x"; names(t09)[4]<-"V_t09x"; names(t09)[5]<-"T_t09m"; names(t09)[6]<-"V_t09m"
  
  names(t10)[3]<-"T_t10x"; names(t10)[4]<-"V_t10x"; names(t10)[5]<-"T_t10m"; names(t10)[6]<-"V_t10m"
  names(t11)[3]<-"T_t11x"; names(t11)[4]<-"V_t11x"; names(t11)[5]<-"T_t11m"; names(t11)[6]<-"V_t11m"
  names(t12)[3]<-"T_t12x"; names(t12)[4]<-"V_t12x"; names(t12)[5]<-"T_t12m"; names(t12)[6]<-"V_t12m"
  names(t13)[3]<-"T_t13x"; names(t13)[4]<-"V_t13x"; names(t13)[5]<-"T_t13m"; names(t13)[6]<-"V_t13m"
  names(t14)[3]<-"T_t14x"; names(t14)[4]<-"V_t14x"; names(t14)[5]<-"T_t14m"; names(t14)[6]<-"V_t14m"
  names(t15)[3]<-"T_t15x"; names(t15)[4]<-"V_t15x"; names(t15)[5]<-"T_t15m"; names(t15)[6]<-"V_t15m"
  names(t16)[3]<-"T_t16x"; names(t16)[4]<-"V_t16x"; names(t16)[5]<-"T_t16m"; names(t16)[6]<-"V_t16m"
  names(t17)[3]<-"T_t17x"; names(t17)[4]<-"V_t17x"; names(t17)[5]<-"T_t17m"; names(t17)[6]<-"V_t17m"
  names(t18)[3]<-"T_t18x"; names(t18)[4]<-"V_t18x"; names(t18)[5]<-"T_t18m"; names(t18)[6]<-"V_t18m"
  names(t19)[3]<-"T_t19x"; names(t19)[4]<-"V_t19x"; names(t19)[5]<-"T_t19m"; names(t19)[6]<-"V_t19m"
  
  names(t20)[3]<-"T_t20x"; names(t20)[4]<-"V_t20x"; names(t20)[5]<-"T_t20m"; names(t20)[6]<-"V_t20m"
  names(t21)[3]<-"T_t21x"; names(t21)[4]<-"V_t21x"; names(t21)[5]<-"T_t21m"; names(t21)[6]<-"V_t21m"
  names(t22)[3]<-"T_t22x"; names(t22)[4]<-"V_t22x"; names(t22)[5]<-"T_t22m"; names(t22)[6]<-"V_t22m"
  names(t23)[3]<-"T_t23x"; names(t23)[4]<-"V_t23x"; names(t23)[5]<-"T_t23m"; names(t23)[6]<-"V_t23m"
  names(t24)[3]<-"T_t24x"; names(t24)[4]<-"V_t24x"; names(t24)[5]<-"T_t24m"; names(t24)[6]<-"V_t24m"
  names(t25)[3]<-"T_t25x"; names(t25)[4]<-"V_t25x"; names(t25)[5]<-"T_t25m"; names(t25)[6]<-"V_t25m"
  names(t26)[3]<-"T_t26x"; names(t26)[4]<-"V_t26x"; names(t26)[5]<-"T_t26m"; names(t26)[6]<-"V_t26m"
  names(t27)[3]<-"T_t27x"; names(t27)[4]<-"V_t27x"; names(t27)[5]<-"T_t27m"; names(t27)[6]<-"V_t27m"
  names(t28)[3]<-"T_t28x"; names(t28)[4]<-"V_t28x"; names(t28)[5]<-"T_t28m"; names(t28)[6]<-"V_t28m"
  names(t29)[3]<-"T_t29x"; names(t29)[4]<-"V_t29x"; names(t29)[5]<-"T_t29m"; names(t29)[6]<-"V_t29m"
  
  names(t30)[3]<-"T_t30x"; names(t30)[4]<-"V_t30x"; names(t30)[5]<-"T_t30m"; names(t30)[6]<-"V_t30m"
  names(t31)[3]<-"T_t31x"; names(t31)[4]<-"V_t31x"; names(t31)[5]<-"T_t31m"; names(t31)[6]<-"V_t31m"
  names(t32)[3]<-"T_t32x"; names(t32)[4]<-"V_t32x"; names(t32)[5]<-"T_t32m"; names(t32)[6]<-"V_t32m"
  names(t33)[3]<-"T_t33x"; names(t33)[4]<-"V_t33x"; names(t33)[5]<-"T_t33m"; names(t33)[6]<-"V_t33m"
  names(t34)[3]<-"T_t34x"; names(t34)[4]<-"V_t34x"; names(t34)[5]<-"T_t34m"; names(t34)[6]<-"V_t34m"
  names(t35)[3]<-"T_t35x"; names(t35)[4]<-"V_t35x"; names(t35)[5]<-"T_t35m"; names(t35)[6]<-"V_t35m"
  names(t36)[3]<-"T_t36x"; names(t36)[4]<-"V_t36x"; names(t36)[5]<-"T_t36m"; names(t36)[6]<-"V_t36m"
  names(t37)[3]<-"T_t37x"; names(t37)[4]<-"V_t37x"; names(t37)[5]<-"T_t37m"; names(t37)[6]<-"V_t37m"
  names(t38)[3]<-"T_t38x"; names(t38)[4]<-"V_t38x"; names(t38)[5]<-"T_t38m"; names(t38)[6]<-"V_t38m"
  names(t39)[3]<-"T_t39x"; names(t39)[4]<-"V_t39x"; names(t39)[5]<-"T_t39m"; names(t39)[6]<-"V_t39m"
  
  # names(t40)[3]<-"T_t40x"; names(t40)[4]<-"V_t40x"; names(t40)[5]<-"T_t40m"; names(t40)[6]<-"V_t40m"
  # names(t41)[3]<-"T_t41x"; names(t41)[4]<-"V_t41x"; names(t41)[5]<-"T_t41m"; names(t41)[6]<-"V_t41m"
  # names(t42)[3]<-"T_t42x"; names(t42)[4]<-"V_t42x"; names(t42)[5]<-"T_t42m"; names(t42)[6]<-"V_t42m"
  # names(t43)[3]<-"T_t43x"; names(t43)[4]<-"V_t43x"; names(t43)[5]<-"T_t43m"; names(t43)[6]<-"V_t43m"
  # names(t44)[3]<-"T_t44x"; names(t44)[4]<-"V_t44x"; names(t44)[5]<-"T_t44m"; names(t44)[6]<-"V_t44m"
  # names(t45)[3]<-"T_t45x"; names(t45)[4]<-"V_t45x"; names(t45)[5]<-"T_t45m"; names(t45)[6]<-"V_t45m"
  # names(t46)[3]<-"T_t46x"; names(t46)[4]<-"V_t46x"; names(t46)[5]<-"T_t46m"; names(t46)[6]<-"V_t46m"
  # names(t47)[3]<-"T_t47x"; names(t47)[4]<-"V_t47x"; names(t47)[5]<-"T_t47m"; names(t47)[6]<-"V_t47m"
  # names(t48)[3]<-"T_t48x"; names(t48)[4]<-"V_t48x"; names(t48)[5]<-"T_t48m"; names(t48)[6]<-"V_t48m"
  # names(t49)[3]<-"T_t49x"; names(t49)[4]<-"V_t49x"; names(t49)[5]<-"T_t49m"; names(t49)[6]<-"V_t49m"
  # 
  # names(t50)[3]<-"T_t50x"; names(t50)[4]<-"V_t50x"; names(t50)[5]<-"T_t50m"; names(t50)[6]<-"V_t50m"
  # names(t51)[3]<-"T_t51x"; names(t51)[4]<-"V_t51x"; names(t51)[5]<-"T_t51m"; names(t51)[6]<-"V_t51m"
  # names(t52)[3]<-"T_t52x"; names(t52)[4]<-"V_t52x"; names(t52)[5]<-"T_t52m"; names(t52)[6]<-"V_t52m"
  # names(t53)[3]<-"T_t53x"; names(t53)[4]<-"V_t53x"; names(t53)[5]<-"T_t53m"; names(t53)[6]<-"V_t53m"
  # names(t54)[3]<-"T_t54x"; names(t54)[4]<-"V_t54x"; names(t54)[5]<-"T_t54m"; names(t54)[6]<-"V_t54m"
  # names(t55)[3]<-"T_t55x"; names(t55)[4]<-"V_t55x"; names(t55)[5]<-"T_t55m"; names(t55)[6]<-"V_t55m"
  # names(t56)[3]<-"T_t56x"; names(t56)[4]<-"V_t56x"; names(t56)[5]<-"T_t56m"; names(t56)[6]<-"V_t56m"
  # names(t57)[3]<-"T_t57x"; names(t57)[4]<-"V_t57x"; names(t57)[5]<-"T_t57m"; names(t57)[6]<-"V_t57m"
  # names(t58)[3]<-"T_t58x"; names(t58)[4]<-"V_t58x"; names(t58)[5]<-"T_t58m"; names(t58)[6]<-"V_t58m"
  # names(t59)[3]<-"T_t59x"; names(t59)[4]<-"V_t59x"; names(t59)[5]<-"T_t59m"; names(t59)[6]<-"V_t59m"
  
  t01<-melt(t01, c("Date","Treatment"))
  t02<-melt(t02, c("Date","Treatment"))
  t03<-melt(t03, c("Date","Treatment"))
  t04<-melt(t04, c("Date","Treatment"))
  t05<-melt(t05, c("Date","Treatment"))
  t06<-melt(t06, c("Date","Treatment"))
  t07<-melt(t07, c("Date","Treatment"))
  t08<-melt(t08, c("Date","Treatment"))
  t09<-melt(t09, c("Date","Treatment"))
  
  t10<-melt(t10, c("Date","Treatment"))
  t11<-melt(t11, c("Date","Treatment"))
  t12<-melt(t12, c("Date","Treatment"))
  t13<-melt(t13, c("Date","Treatment"))
  t14<-melt(t14, c("Date","Treatment"))
  t15<-melt(t15, c("Date","Treatment"))
  t16<-melt(t16, c("Date","Treatment"))
  t17<-melt(t17, c("Date","Treatment"))
  t18<-melt(t18, c("Date","Treatment"))
  t19<-melt(t19, c("Date","Treatment"))
  
  t20<-melt(t20, c("Date","Treatment"))
  t21<-melt(t21, c("Date","Treatment"))
  t22<-melt(t22, c("Date","Treatment"))
  t23<-melt(t23, c("Date","Treatment"))
  t24<-melt(t24, c("Date","Treatment"))
  t25<-melt(t25, c("Date","Treatment"))
  t26<-melt(t26, c("Date","Treatment"))
  t27<-melt(t27, c("Date","Treatment"))
  t28<-melt(t28, c("Date","Treatment"))
  t29<-melt(t29, c("Date","Treatment"))
  
  t30<-melt(t30, c("Date","Treatment"))
  t31<-melt(t31, c("Date","Treatment"))
  t32<-melt(t32, c("Date","Treatment"))
  t33<-melt(t33, c("Date","Treatment"))
  t34<-melt(t34, c("Date","Treatment"))
  t35<-melt(t35, c("Date","Treatment"))
  t36<-melt(t36, c("Date","Treatment"))
  t37<-melt(t37, c("Date","Treatment"))
  t38<-melt(t38, c("Date","Treatment"))
  t39<-melt(t39, c("Date","Treatment"))
  
  # t40<-melt(t40, c("Date","Treatment"))
  # t41<-melt(t41, c("Date","Treatment"))
  # t42<-melt(t42, c("Date","Treatment"))
  # t43<-melt(t43, c("Date","Treatment"))
  # t44<-melt(t44, c("Date","Treatment"))
  # t45<-melt(t45, c("Date","Treatment"))
  # t46<-melt(t46, c("Date","Treatment"))
  # t47<-melt(t47, c("Date","Treatment"))
  # t48<-melt(t48, c("Date","Treatment"))
  # t49<-melt(t49, c("Date","Treatment"))
  # 
  # t50<-melt(t50, c("Date","Treatment"))
  # t51<-melt(t51, c("Date","Treatment"))
  # t52<-melt(t52, c("Date","Treatment"))
  # t53<-melt(t53, c("Date","Treatment"))
  # t54<-melt(t54, c("Date","Treatment"))
  # t55<-melt(t55, c("Date","Treatment"))
  # t56<-melt(t56, c("Date","Treatment"))
  # t57<-melt(t57, c("Date","Treatment"))
  # t58<-melt(t58, c("Date","Treatment"))
  # t59<-melt(t59, c("Date","Treatment"))
  
  # end total summaries ####
  # day summaries ####
  d01<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d01)
  d02<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d02)
  d03<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d03)
  d04<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d04)
  d05<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d05)
  d06<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d06)
  d07<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d07)
  d08<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d08)
  d09<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d09)
  
  d10<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d10)
  d11<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d11)
  d12<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d12)
  d13<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d13)
  d14<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d14)
  d15<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d15)
  d16<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d16)
  d17<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d17)
  d18<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d18)
  d19<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d19)
  
  d20<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d20)
  d21<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d21)
  d22<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d22)
  d23<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d23)
  d24<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d24)
  d25<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d25)
  d26<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d26)
  d27<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d27)
  d28<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d28)
  d29<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d29)
  
  d30<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d30)
  d31<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d31)
  d32<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d32)
  d33<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d33)
  d34<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d34)
  d35<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d35)
  d36<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d36)
  d37<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d37)
  d38<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d38)
  d39<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d39)
  
  # d40<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d40)
  # d41<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d41)
  # d42<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d42)
  # d43<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d43)
  # d44<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d44)
  # d45<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d45)
  # d46<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d46)
  # d47<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d47)
  # d48<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d48)
  # d49<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d49)
  # 
  # d50<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d50)
  # d51<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d51)
  # d52<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d52)
  # d53<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d53)
  # d54<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d54)
  # d55<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d55)
  # d56<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d56)
  # d57<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d57)
  # d58<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d58)
  # d59<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, d59)
  
  names(d01)[3]<-"T_d01x"; names(d01)[4]<-"V_d01x"; names(d01)[5]<-"T_d01m"; names(d01)[6]<-"V_d01m"
  names(d02)[3]<-"T_d02x"; names(d02)[4]<-"V_d02x"; names(d02)[5]<-"T_d02m"; names(d02)[6]<-"V_d02m"
  names(d03)[3]<-"T_d03x"; names(d03)[4]<-"V_d03x"; names(d03)[5]<-"T_d03m"; names(d03)[6]<-"V_d03m"
  names(d04)[3]<-"T_d04x"; names(d04)[4]<-"V_d04x"; names(d04)[5]<-"T_d04m"; names(d04)[6]<-"V_d04m"
  names(d05)[3]<-"T_d05x"; names(d05)[4]<-"V_d05x"; names(d05)[5]<-"T_d05m"; names(d05)[6]<-"V_d05m"
  names(d06)[3]<-"T_d06x"; names(d06)[4]<-"V_d06x"; names(d06)[5]<-"T_d06m"; names(d06)[6]<-"V_d06m"
  names(d07)[3]<-"T_d07x"; names(d07)[4]<-"V_d07x"; names(d07)[5]<-"T_d07m"; names(d07)[6]<-"V_d07m"
  names(d08)[3]<-"T_d08x"; names(d08)[4]<-"V_d08x"; names(d08)[5]<-"T_d08m"; names(d08)[6]<-"V_d08m"
  names(d09)[3]<-"T_d09x"; names(d09)[4]<-"V_d09x"; names(d09)[5]<-"T_d09m"; names(d09)[6]<-"V_d09m"
  
  names(d10)[3]<-"T_d10x"; names(d10)[4]<-"V_d10x"; names(d10)[5]<-"T_d10m"; names(d10)[6]<-"V_d10m"
  names(d11)[3]<-"T_d11x"; names(d11)[4]<-"V_d11x"; names(d11)[5]<-"T_d11m"; names(d11)[6]<-"V_d11m"
  names(d12)[3]<-"T_d12x"; names(d12)[4]<-"V_d12x"; names(d12)[5]<-"T_d12m"; names(d12)[6]<-"V_d12m"
  names(d13)[3]<-"T_d13x"; names(d13)[4]<-"V_d13x"; names(d13)[5]<-"T_d13m"; names(d13)[6]<-"V_d13m"
  names(d14)[3]<-"T_d14x"; names(d14)[4]<-"V_d14x"; names(d14)[5]<-"T_d14m"; names(d14)[6]<-"V_d14m"
  names(d15)[3]<-"T_d15x"; names(d15)[4]<-"V_d15x"; names(d15)[5]<-"T_d15m"; names(d15)[6]<-"V_d15m"
  names(d16)[3]<-"T_d16x"; names(d16)[4]<-"V_d16x"; names(d16)[5]<-"T_d16m"; names(d16)[6]<-"V_d16m"
  names(d17)[3]<-"T_d17x"; names(d17)[4]<-"V_d17x"; names(d17)[5]<-"T_d17m"; names(d17)[6]<-"V_d17m"
  names(d18)[3]<-"T_d18x"; names(d18)[4]<-"V_d18x"; names(d18)[5]<-"T_d18m"; names(d18)[6]<-"V_d18m"
  names(d19)[3]<-"T_d19x"; names(d19)[4]<-"V_d19x"; names(d19)[5]<-"T_d19m"; names(d19)[6]<-"V_d19m"
  
  names(d20)[3]<-"T_d20x"; names(d20)[4]<-"V_d20x"; names(d20)[5]<-"T_d20m"; names(d20)[6]<-"V_d20m"
  names(d21)[3]<-"T_d21x"; names(d21)[4]<-"V_d21x"; names(d21)[5]<-"T_d21m"; names(d21)[6]<-"V_d21m"
  names(d22)[3]<-"T_d22x"; names(d22)[4]<-"V_d22x"; names(d22)[5]<-"T_d22m"; names(d22)[6]<-"V_d22m"
  names(d23)[3]<-"T_d23x"; names(d23)[4]<-"V_d23x"; names(d23)[5]<-"T_d23m"; names(d23)[6]<-"V_d23m"
  names(d24)[3]<-"T_d24x"; names(d24)[4]<-"V_d24x"; names(d24)[5]<-"T_d24m"; names(d24)[6]<-"V_d24m"
  names(d25)[3]<-"T_d25x"; names(d25)[4]<-"V_d25x"; names(d25)[5]<-"T_d25m"; names(d25)[6]<-"V_d25m"
  names(d26)[3]<-"T_d26x"; names(d26)[4]<-"V_d26x"; names(d26)[5]<-"T_d26m"; names(d26)[6]<-"V_d26m"
  names(d27)[3]<-"T_d27x"; names(d27)[4]<-"V_d27x"; names(d27)[5]<-"T_d27m"; names(d27)[6]<-"V_d27m"
  names(d28)[3]<-"T_d28x"; names(d28)[4]<-"V_d28x"; names(d28)[5]<-"T_d28m"; names(d28)[6]<-"V_d28m"
  names(d29)[3]<-"T_d29x"; names(d29)[4]<-"V_d29x"; names(d29)[5]<-"T_d29m"; names(d29)[6]<-"V_d29m"
  
  names(d30)[3]<-"T_d30x"; names(d30)[4]<-"V_d30x"; names(d30)[5]<-"T_d30m"; names(d30)[6]<-"V_d30m"
  names(d31)[3]<-"T_d31x"; names(d31)[4]<-"V_d31x"; names(d31)[5]<-"T_d31m"; names(d31)[6]<-"V_d31m"
  names(d32)[3]<-"T_d32x"; names(d32)[4]<-"V_d32x"; names(d32)[5]<-"T_d32m"; names(d32)[6]<-"V_d32m"
  names(d33)[3]<-"T_d33x"; names(d33)[4]<-"V_d33x"; names(d33)[5]<-"T_d33m"; names(d33)[6]<-"V_d33m"
  names(d34)[3]<-"T_d34x"; names(d34)[4]<-"V_d34x"; names(d34)[5]<-"T_d34m"; names(d34)[6]<-"V_d34m"
  names(d35)[3]<-"T_d35x"; names(d35)[4]<-"V_d35x"; names(d35)[5]<-"T_d35m"; names(d35)[6]<-"V_d35m"
  names(d36)[3]<-"T_d36x"; names(d36)[4]<-"V_d36x"; names(d36)[5]<-"T_d36m"; names(d36)[6]<-"V_d36m"
  names(d37)[3]<-"T_d37x"; names(d37)[4]<-"V_d37x"; names(d37)[5]<-"T_d37m"; names(d37)[6]<-"V_d37m"
  names(d38)[3]<-"T_d38x"; names(d38)[4]<-"V_d38x"; names(d38)[5]<-"T_d38m"; names(d38)[6]<-"V_d38m"
  names(d39)[3]<-"T_d39x"; names(d39)[4]<-"V_d39x"; names(d39)[5]<-"T_d39m"; names(d39)[6]<-"V_d39m"
  
  # names(d40)[3]<-"T_d40x"; names(d40)[4]<-"V_d40x"; names(d40)[5]<-"T_d40m"; names(d40)[6]<-"V_d40m"
  # names(d41)[3]<-"T_d41x"; names(d41)[4]<-"V_d41x"; names(d41)[5]<-"T_d41m"; names(d41)[6]<-"V_d41m"
  # names(d42)[3]<-"T_d42x"; names(d42)[4]<-"V_d42x"; names(d42)[5]<-"T_d42m"; names(d42)[6]<-"V_d42m"
  # names(d43)[3]<-"T_d43x"; names(d43)[4]<-"V_d43x"; names(d43)[5]<-"T_d43m"; names(d43)[6]<-"V_d43m"
  # names(d44)[3]<-"T_d44x"; names(d44)[4]<-"V_d44x"; names(d44)[5]<-"T_d44m"; names(d44)[6]<-"V_d44m"
  # names(d45)[3]<-"T_d45x"; names(d45)[4]<-"V_d45x"; names(d45)[5]<-"T_d45m"; names(d45)[6]<-"V_d45m"
  # names(d46)[3]<-"T_d46x"; names(d46)[4]<-"V_d46x"; names(d46)[5]<-"T_d46m"; names(d46)[6]<-"V_d46m"
  # names(d47)[3]<-"T_d47x"; names(d47)[4]<-"V_d47x"; names(d47)[5]<-"T_d47m"; names(d47)[6]<-"V_d47m"
  # names(d48)[3]<-"T_d48x"; names(d48)[4]<-"V_d48x"; names(d48)[5]<-"T_d48m"; names(d48)[6]<-"V_d48m"
  # names(d49)[3]<-"T_d49x"; names(d49)[4]<-"V_d49x"; names(d49)[5]<-"T_d49m"; names(d49)[6]<-"V_d49m"
  # 
  # names(d50)[3]<-"T_d50x"; names(d50)[4]<-"V_d50x"; names(d50)[5]<-"T_d50m"; names(d50)[6]<-"V_d50m"
  # names(d51)[3]<-"T_d51x"; names(d51)[4]<-"V_d51x"; names(d51)[5]<-"T_d51m"; names(d51)[6]<-"V_d51m"
  # names(d52)[3]<-"T_d52x"; names(d52)[4]<-"V_d52x"; names(d52)[5]<-"T_d52m"; names(d52)[6]<-"V_d52m"
  # names(d53)[3]<-"T_d53x"; names(d53)[4]<-"V_d53x"; names(d53)[5]<-"T_d53m"; names(d53)[6]<-"V_d53m"
  # names(d54)[3]<-"T_d54x"; names(d54)[4]<-"V_d54x"; names(d54)[5]<-"T_d54m"; names(d54)[6]<-"V_d54m"
  # names(d55)[3]<-"T_d55x"; names(d55)[4]<-"V_d55x"; names(d55)[5]<-"T_d55m"; names(d55)[6]<-"V_d55m"
  # names(d56)[3]<-"T_d56x"; names(d56)[4]<-"V_d56x"; names(d56)[5]<-"T_d56m"; names(d56)[6]<-"V_d56m"
  # names(d57)[3]<-"T_d57x"; names(d57)[4]<-"V_d57x"; names(d57)[5]<-"T_d57m"; names(d57)[6]<-"V_d57m"
  # names(d58)[3]<-"T_d58x"; names(d58)[4]<-"V_d58x"; names(d58)[5]<-"T_d58m"; names(d58)[6]<-"V_d58m"
  # names(d59)[3]<-"T_d59x"; names(d59)[4]<-"V_d59x"; names(d59)[5]<-"T_d59m"; names(d59)[6]<-"V_d59m"
  
  d01<-melt(d01, c("Date","Treatment"))
  d02<-melt(d02, c("Date","Treatment"))
  d03<-melt(d03, c("Date","Treatment"))
  d04<-melt(d04, c("Date","Treatment"))
  d05<-melt(d05, c("Date","Treatment"))
  d06<-melt(d06, c("Date","Treatment"))
  d07<-melt(d07, c("Date","Treatment"))
  d08<-melt(d08, c("Date","Treatment"))
  d09<-melt(d09, c("Date","Treatment"))
  
  d10<-melt(d10, c("Date","Treatment"))
  d11<-melt(d11, c("Date","Treatment"))
  d12<-melt(d12, c("Date","Treatment"))
  d13<-melt(d13, c("Date","Treatment"))
  d14<-melt(d14, c("Date","Treatment"))
  d15<-melt(d15, c("Date","Treatment"))
  d16<-melt(d16, c("Date","Treatment"))
  d17<-melt(d17, c("Date","Treatment"))
  d18<-melt(d18, c("Date","Treatment"))
  d19<-melt(d19, c("Date","Treatment"))
  
  d20<-melt(d20, c("Date","Treatment"))
  d21<-melt(d21, c("Date","Treatment"))
  d22<-melt(d22, c("Date","Treatment"))
  d23<-melt(d23, c("Date","Treatment"))
  d24<-melt(d24, c("Date","Treatment"))
  d25<-melt(d25, c("Date","Treatment"))
  d26<-melt(d26, c("Date","Treatment"))
  d27<-melt(d27, c("Date","Treatment"))
  d28<-melt(d28, c("Date","Treatment"))
  d29<-melt(d29, c("Date","Treatment"))
  
  d30<-melt(d30, c("Date","Treatment"))
  d31<-melt(d31, c("Date","Treatment"))
  d32<-melt(d32, c("Date","Treatment"))
  d33<-melt(d33, c("Date","Treatment"))
  d34<-melt(d34, c("Date","Treatment"))
  d35<-melt(d35, c("Date","Treatment"))
  d36<-melt(d36, c("Date","Treatment"))
  d37<-melt(d37, c("Date","Treatment"))
  d38<-melt(d38, c("Date","Treatment"))
  d39<-melt(d39, c("Date","Treatment"))
  
  # d40<-melt(d40, c("Date","Treatment"))
  # d41<-melt(d41, c("Date","Treatment"))
  # d42<-melt(d42, c("Date","Treatment"))
  # d43<-melt(d43, c("Date","Treatment"))
  # d44<-melt(d44, c("Date","Treatment"))
  # d45<-melt(d45, c("Date","Treatment"))
  # d46<-melt(d46, c("Date","Treatment"))
  # d47<-melt(d47, c("Date","Treatment"))
  # d48<-melt(d48, c("Date","Treatment"))
  # d49<-melt(d49, c("Date","Treatment"))
  # 
  # d50<-melt(d50, c("Date","Treatment"))
  # d51<-melt(d51, c("Date","Treatment"))
  # d52<-melt(d52, c("Date","Treatment"))
  # d53<-melt(d53, c("Date","Treatment"))
  # d54<-melt(d54, c("Date","Treatment"))
  # d55<-melt(d55, c("Date","Treatment"))
  # d56<-melt(d56, c("Date","Treatment"))
  # d57<-melt(d57, c("Date","Treatment"))
  # d58<-melt(d58, c("Date","Treatment"))
  # d59<-melt(d59, c("Date","Treatment"))
  
  # end day summaries ####
  # night summaries ####
  n01<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n01)
  n02<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n02)
  n03<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n03)
  n04<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n04)
  n05<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n05)
  n06<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n06)
  n07<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n07)
  n08<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n08)
  n09<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n09)
  
  n10<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n10)
  n11<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n11)
  n12<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n12)
  n13<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n13)
  n14<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n14)
  n15<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n15)
  n16<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n16)
  n17<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n17)
  n18<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n18)
  n19<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n19)
  
  n20<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n20)
  n21<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n21)
  n22<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n22)
  n23<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n23)
  n24<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n24)
  n25<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n25)
  n26<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n26)
  n27<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n27)
  n28<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n28)
  n29<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n29)
  
  n30<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n30)
  n31<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n31)
  n32<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n32)
  n33<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n33)
  n34<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n34)
  n35<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n35)
  n36<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n36)
  n37<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n37)
  n38<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n38)
  n39<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n39)
  
  # n40<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n40)
  # n41<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n41)
  # n42<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n42)
  # n43<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n43)
  # n44<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n44)
  # n45<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n45)
  # n46<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n46)
  # n47<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n47)
  # n48<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n48)
  # n49<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n49)
  # 
  # n50<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n50)
  # n51<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n51)
  # n52<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n52)
  # n53<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n53)
  # n54<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n54)
  # n55<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n55)
  # n56<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n56)
  # n57<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n57)
  # n58<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n58)
  # n59<-summaryBy(Temp_C + VPD_kPa ~ Date + Treatment, FUN = c(mean, median), na.rm = T, n59)
  
  names(n01)[3]<-"T_n01x"; names(n01)[4]<-"V_n01x"; names(n01)[5]<-"T_n01m"; names(n01)[6]<-"V_n01m"
  names(n02)[3]<-"T_n02x"; names(n02)[4]<-"V_n02x"; names(n02)[5]<-"T_n02m"; names(n02)[6]<-"V_n02m"
  names(n03)[3]<-"T_n03x"; names(n03)[4]<-"V_n03x"; names(n03)[5]<-"T_n03m"; names(n03)[6]<-"V_n03m"
  names(n04)[3]<-"T_n04x"; names(n04)[4]<-"V_n04x"; names(n04)[5]<-"T_n04m"; names(n04)[6]<-"V_n04m"
  names(n05)[3]<-"T_n05x"; names(n05)[4]<-"V_n05x"; names(n05)[5]<-"T_n05m"; names(n05)[6]<-"V_n05m"
  names(n06)[3]<-"T_n06x"; names(n06)[4]<-"V_n06x"; names(n06)[5]<-"T_n06m"; names(n06)[6]<-"V_n06m"
  names(n07)[3]<-"T_n07x"; names(n07)[4]<-"V_n07x"; names(n07)[5]<-"T_n07m"; names(n07)[6]<-"V_n07m"
  names(n08)[3]<-"T_n08x"; names(n08)[4]<-"V_n08x"; names(n08)[5]<-"T_n08m"; names(n08)[6]<-"V_n08m"
  names(n09)[3]<-"T_n09x"; names(n09)[4]<-"V_n09x"; names(n09)[5]<-"T_n09m"; names(n09)[6]<-"V_n09m"
  
  names(n10)[3]<-"T_n10x"; names(n10)[4]<-"V_n10x"; names(n10)[5]<-"T_n10m"; names(n10)[6]<-"V_n10m"
  names(n11)[3]<-"T_n11x"; names(n11)[4]<-"V_n11x"; names(n11)[5]<-"T_n11m"; names(n11)[6]<-"V_n11m"
  names(n12)[3]<-"T_n12x"; names(n12)[4]<-"V_n12x"; names(n12)[5]<-"T_n12m"; names(n12)[6]<-"V_n12m"
  names(n13)[3]<-"T_n13x"; names(n13)[4]<-"V_n13x"; names(n13)[5]<-"T_n13m"; names(n13)[6]<-"V_n13m"
  names(n14)[3]<-"T_n14x"; names(n14)[4]<-"V_n14x"; names(n14)[5]<-"T_n14m"; names(n14)[6]<-"V_n14m"
  names(n15)[3]<-"T_n15x"; names(n15)[4]<-"V_n15x"; names(n15)[5]<-"T_n15m"; names(n15)[6]<-"V_n15m"
  names(n16)[3]<-"T_n16x"; names(n16)[4]<-"V_n16x"; names(n16)[5]<-"T_n16m"; names(n16)[6]<-"V_n16m"
  names(n17)[3]<-"T_n17x"; names(n17)[4]<-"V_n17x"; names(n17)[5]<-"T_n17m"; names(n17)[6]<-"V_n17m"
  names(n18)[3]<-"T_n18x"; names(n18)[4]<-"V_n18x"; names(n18)[5]<-"T_n18m"; names(n18)[6]<-"V_n18m"
  names(n19)[3]<-"T_n19x"; names(n19)[4]<-"V_n19x"; names(n19)[5]<-"T_n19m"; names(n19)[6]<-"V_n19m"
  
  names(n20)[3]<-"T_n20x"; names(n20)[4]<-"V_n20x"; names(n20)[5]<-"T_n20m"; names(n20)[6]<-"V_n20m"
  names(n21)[3]<-"T_n21x"; names(n21)[4]<-"V_n21x"; names(n21)[5]<-"T_n21m"; names(n21)[6]<-"V_n21m"
  names(n22)[3]<-"T_n22x"; names(n22)[4]<-"V_n22x"; names(n22)[5]<-"T_n22m"; names(n22)[6]<-"V_n22m"
  names(n23)[3]<-"T_n23x"; names(n23)[4]<-"V_n23x"; names(n23)[5]<-"T_n23m"; names(n23)[6]<-"V_n23m"
  names(n24)[3]<-"T_n24x"; names(n24)[4]<-"V_n24x"; names(n24)[5]<-"T_n24m"; names(n24)[6]<-"V_n24m"
  names(n25)[3]<-"T_n25x"; names(n25)[4]<-"V_n25x"; names(n25)[5]<-"T_n25m"; names(n25)[6]<-"V_n25m"
  names(n26)[3]<-"T_n26x"; names(n26)[4]<-"V_n26x"; names(n26)[5]<-"T_n26m"; names(n26)[6]<-"V_n26m"
  names(n27)[3]<-"T_n27x"; names(n27)[4]<-"V_n27x"; names(n27)[5]<-"T_n27m"; names(n27)[6]<-"V_n27m"
  names(n28)[3]<-"T_n28x"; names(n28)[4]<-"V_n28x"; names(n28)[5]<-"T_n28m"; names(n28)[6]<-"V_n28m"
  names(n29)[3]<-"T_n29x"; names(n29)[4]<-"V_n29x"; names(n29)[5]<-"T_n29m"; names(n29)[6]<-"V_n29m"
  
  names(n30)[3]<-"T_n30x"; names(n30)[4]<-"V_n30x"; names(n30)[5]<-"T_n30m"; names(n30)[6]<-"V_n30m"
  names(n31)[3]<-"T_n31x"; names(n31)[4]<-"V_n31x"; names(n31)[5]<-"T_n31m"; names(n31)[6]<-"V_n31m"
  names(n32)[3]<-"T_n32x"; names(n32)[4]<-"V_n32x"; names(n32)[5]<-"T_n32m"; names(n32)[6]<-"V_n32m"
  names(n33)[3]<-"T_n33x"; names(n33)[4]<-"V_n33x"; names(n33)[5]<-"T_n33m"; names(n33)[6]<-"V_n33m"
  names(n34)[3]<-"T_n34x"; names(n34)[4]<-"V_n34x"; names(n34)[5]<-"T_n34m"; names(n34)[6]<-"V_n34m"
  names(n35)[3]<-"T_n35x"; names(n35)[4]<-"V_n35x"; names(n35)[5]<-"T_n35m"; names(n35)[6]<-"V_n35m"
  names(n36)[3]<-"T_n36x"; names(n36)[4]<-"V_n36x"; names(n36)[5]<-"T_n36m"; names(n36)[6]<-"V_n36m"
  names(n37)[3]<-"T_n37x"; names(n37)[4]<-"V_n37x"; names(n37)[5]<-"T_n37m"; names(n37)[6]<-"V_n37m"
  names(n38)[3]<-"T_n38x"; names(n38)[4]<-"V_n38x"; names(n38)[5]<-"T_n38m"; names(n38)[6]<-"V_n38m"
  names(n39)[3]<-"T_n39x"; names(n39)[4]<-"V_n39x"; names(n39)[5]<-"T_n39m"; names(n39)[6]<-"V_n39m"
  
  # names(n40)[3]<-"T_n40x"; names(n40)[4]<-"V_n40x"; names(n40)[5]<-"T_n40m"; names(n40)[6]<-"V_n40m"
  # names(n41)[3]<-"T_n41x"; names(n41)[4]<-"V_n41x"; names(n41)[5]<-"T_n41m"; names(n41)[6]<-"V_n41m"
  # names(n42)[3]<-"T_n42x"; names(n42)[4]<-"V_n42x"; names(n42)[5]<-"T_n42m"; names(n42)[6]<-"V_n42m"
  # names(n43)[3]<-"T_n43x"; names(n43)[4]<-"V_n43x"; names(n43)[5]<-"T_n43m"; names(n43)[6]<-"V_n43m"
  # names(n44)[3]<-"T_n44x"; names(n44)[4]<-"V_n44x"; names(n44)[5]<-"T_n44m"; names(n44)[6]<-"V_n44m"
  # names(n45)[3]<-"T_n45x"; names(n45)[4]<-"V_n45x"; names(n45)[5]<-"T_n45m"; names(n45)[6]<-"V_n45m"
  # names(n46)[3]<-"T_n46x"; names(n46)[4]<-"V_n46x"; names(n46)[5]<-"T_n46m"; names(n46)[6]<-"V_n46m"
  # names(n47)[3]<-"T_n47x"; names(n47)[4]<-"V_n47x"; names(n47)[5]<-"T_n47m"; names(n47)[6]<-"V_n47m"
  # names(n48)[3]<-"T_n48x"; names(n48)[4]<-"V_n48x"; names(n48)[5]<-"T_n48m"; names(n48)[6]<-"V_n48m"
  # names(n49)[3]<-"T_n49x"; names(n49)[4]<-"V_n49x"; names(n49)[5]<-"T_n49m"; names(n49)[6]<-"V_n49m"
  # 
  # names(n50)[3]<-"T_n50x"; names(n50)[4]<-"V_n50x"; names(n50)[5]<-"T_n50m"; names(n50)[6]<-"V_n50m"
  # names(n51)[3]<-"T_n51x"; names(n51)[4]<-"V_n51x"; names(n51)[5]<-"T_n51m"; names(n51)[6]<-"V_n51m"
  # names(n52)[3]<-"T_n52x"; names(n52)[4]<-"V_n52x"; names(n52)[5]<-"T_n52m"; names(n52)[6]<-"V_n52m"
  # names(n53)[3]<-"T_n53x"; names(n53)[4]<-"V_n53x"; names(n53)[5]<-"T_n53m"; names(n53)[6]<-"V_n53m"
  # names(n54)[3]<-"T_n54x"; names(n54)[4]<-"V_n54x"; names(n54)[5]<-"T_n54m"; names(n54)[6]<-"V_n54m"
  # names(n55)[3]<-"T_n55x"; names(n55)[4]<-"V_n55x"; names(n55)[5]<-"T_n55m"; names(n55)[6]<-"V_n55m"
  # names(n56)[3]<-"T_n56x"; names(n56)[4]<-"V_n56x"; names(n56)[5]<-"T_n56m"; names(n56)[6]<-"V_n56m"
  # names(n57)[3]<-"T_n57x"; names(n57)[4]<-"V_n57x"; names(n57)[5]<-"T_n57m"; names(n57)[6]<-"V_n57m"
  # names(n58)[3]<-"T_n58x"; names(n58)[4]<-"V_n58x"; names(n58)[5]<-"T_n58m"; names(n58)[6]<-"V_n58m"
  # names(n59)[3]<-"T_n59x"; names(n59)[4]<-"V_n59x"; names(n59)[5]<-"T_n59m"; names(n59)[6]<-"V_n59m"
  
  n01<-melt(n01, c("Date","Treatment"))
  n02<-melt(n02, c("Date","Treatment"))
  n03<-melt(n03, c("Date","Treatment"))
  n04<-melt(n04, c("Date","Treatment"))
  n05<-melt(n05, c("Date","Treatment"))
  n06<-melt(n06, c("Date","Treatment"))
  n07<-melt(n07, c("Date","Treatment"))
  n08<-melt(n08, c("Date","Treatment"))
  n09<-melt(n09, c("Date","Treatment"))
  
  n10<-melt(n10, c("Date","Treatment"))
  n11<-melt(n11, c("Date","Treatment"))
  n12<-melt(n12, c("Date","Treatment"))
  n13<-melt(n13, c("Date","Treatment"))
  n14<-melt(n14, c("Date","Treatment"))
  n15<-melt(n15, c("Date","Treatment"))
  n16<-melt(n16, c("Date","Treatment"))
  n17<-melt(n17, c("Date","Treatment"))
  n18<-melt(n18, c("Date","Treatment"))
  n19<-melt(n19, c("Date","Treatment"))
  
  n20<-melt(n20, c("Date","Treatment"))
  n21<-melt(n21, c("Date","Treatment"))
  n22<-melt(n22, c("Date","Treatment"))
  n23<-melt(n23, c("Date","Treatment"))
  n24<-melt(n24, c("Date","Treatment"))
  n25<-melt(n25, c("Date","Treatment"))
  n26<-melt(n26, c("Date","Treatment"))
  n27<-melt(n27, c("Date","Treatment"))
  n28<-melt(n28, c("Date","Treatment"))
  n29<-melt(n29, c("Date","Treatment"))
  
  n30<-melt(n30, c("Date","Treatment"))
  n31<-melt(n31, c("Date","Treatment"))
  n32<-melt(n32, c("Date","Treatment"))
  n33<-melt(n33, c("Date","Treatment"))
  n34<-melt(n34, c("Date","Treatment"))
  n35<-melt(n35, c("Date","Treatment"))
  n36<-melt(n36, c("Date","Treatment"))
  n37<-melt(n37, c("Date","Treatment"))
  n38<-melt(n38, c("Date","Treatment"))
  n39<-melt(n39, c("Date","Treatment"))
  
  # n40<-melt(n40, c("Date","Treatment"))
  # n41<-melt(n41, c("Date","Treatment"))
  # n42<-melt(n42, c("Date","Treatment"))
  # n43<-melt(n43, c("Date","Treatment"))
  # n44<-melt(n44, c("Date","Treatment"))
  # n45<-melt(n45, c("Date","Treatment"))
  # n46<-melt(n46, c("Date","Treatment"))
  # n47<-melt(n47, c("Date","Treatment"))
  # n48<-melt(n48, c("Date","Treatment"))
  # n49<-melt(n49, c("Date","Treatment"))
  # 
  # n50<-melt(n50, c("Date","Treatment"))
  # n51<-melt(n51, c("Date","Treatment"))
  # n52<-melt(n52, c("Date","Treatment"))
  # n53<-melt(n53, c("Date","Treatment"))
  # n54<-melt(n54, c("Date","Treatment"))
  # n55<-melt(n55, c("Date","Treatment"))
  # n56<-melt(n56, c("Date","Treatment"))
  # n57<-melt(n57, c("Date","Treatment"))
  # n58<-melt(n58, c("Date","Treatment"))
  # n59<-melt(n59, c("Date","Treatment"))
  # end night summaries ####
  # binding ####
  
  new_env<-rbind(new_env,t01,t02,t03,t04,t05,t06,t07,t08,t09,
                 t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,
                 t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,
                 t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,
                 
                 d01,d02,d03,d04,d05,d06,d07,d08,d09,
                 d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,
                 d20,d21,d22,d23,d24,d25,d26,d27,d28,d29,
                 d30,d31,d32,d33,d34,d35,d36,d37,d38,d39,
                 
                 n01,n02,n03,n04,n05,n06,n07,n08,n09,
                 n10,n11,n12,n13,n14,n15,n16,n17,n18,n19,
                 n20,n21,n22,n23,n24,n25,n26,n27,n28,n29,
                 n30,n31,n32,n33,n34,n35,n36,n37,n38,n39)
  # new_env<-rbind(new_env,t01,t02,t03,t04,t05,t06,t07,t08,t09,
  #                t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,
  #                t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,
  #                t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,
  #                t40,t41,t42,t43,t44,t45,t46,t47,t48,t49,
  #                t50,t51,t52,t53,t54,t55,t56,t57,t58,t59,
  #                
  #                d01,d02,d03,d04,d05,d06,d07,d08,d09,
  #                d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,
  #                d20,d21,d22,d23,d24,d25,d26,d27,d28,d29,
  #                d30,d31,d32,d33,d34,d35,d36,d37,d38,d39,
  #                d40,d41,d42,d43,d44,d45,d46,d47,d48,d49,
  #                d50,d51,d52,d53,d54,d55,d56,d57,d58,d59,
  #                
  #                n01,n02,n03,n04,n05,n06,n07,n08,n09,
  #                n10,n11,n12,n13,n14,n15,n16,n17,n18,n19,
  #                n20,n21,n22,n23,n24,n25,n26,n27,n28,n29,
  #                n30,n31,n32,n33,n34,n35,n36,n37,n38,n39,
  #                n40,n41,n42,n43,n44,n45,n46,n47,n48,n49,
  #                n50,n51,n52,n53,n54,n55,n56,n57,n58,n59)
  
  # end binding ####
  rm(mnd)
  
}

# remove all and out####

rm(list=setdiff(ls(), "new_env"))
new_env<-cast(new_env, Date+Treatment~variable, mean) 
new_env<-new_env[complete.cases(new_env$T_t01x),]

# write.csv(new_env, "Prevailing env (39 days).csv", row.names = F)
