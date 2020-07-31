# Load needed libraries ---------------------------------------------------
my_packages<-c("reshape2","MASS","tgp","plyr","nlme","ggplot2","ggthemes","RColorBrewer","gdata")
for (package in my_packages) {
  if (!require(package, character.only=T, quietly=T)) {
    try(install.packages(package))
    library(package, character.only=T)
  }
}

# Data import and ANPP calculation from cover -----------------------------
# Read data from plan cover files
cover09<- read.csv("C:/Users/lgherar1.ASURITE/Dropbox (ASU)/lgherar1/desktop/current/Papers/Legacy over time/data/cover09_Rev.csv")
cover10<- read.csv("C:/Users/lgherar1.ASURITE/Dropbox (ASU)/lgherar1/desktop/current/Papers/Legacy over time/data/cover10_Rev.csv")
cover11<- read.csv("C:/Users/lgherar1.ASURITE/Dropbox (ASU)/lgherar1/desktop/current/Papers/Legacy over time/data/cover11_Rev.csv")
cover12<- read.csv("C:/Users/lgherar1.ASURITE/Dropbox (ASU)/lgherar1/desktop/current/Papers/Legacy over time/data/cover12_Rev.csv")
cover13<- read.csv("C:/Users/lgherar1.ASURITE/Dropbox (ASU)/lgherar1/desktop/current/Papers/Legacy over time/data/cover13_Rev.csv")
cover14<- read.csv("C:/Users/lgherar1.ASURITE/Dropbox (ASU)/lgherar1/desktop/current/Papers/Legacy over time/data/cover14_Rev.csv")
cover15<- read.csv("C:/Users/lgherar1.ASURITE/Dropbox (ASU)/lgherar1/desktop/current/Papers/Legacy over time/data/cover15_Rev.csv")
cover16<- read.csv("C:/Users/lgherar1.ASURITE/Dropbox (ASU)/lgherar1/desktop/current/Papers/Legacy over time/data/cover16_Rev.csv")
cover17<- read.csv("C:/Users/lgherar1.ASURITE/Dropbox (ASU)/lgherar1/desktop/current/Papers/Legacy over time/data/cover17_Rev.csv")
cover18<- read.csv("C:/Users/lgherar1.ASURITE/Dropbox (ASU)/lgherar1/desktop/current/Papers/Legacy over time/data/cover18_Rev.csv")
cover19<- read.csv("C:/Users/lgherar1.ASURITE/Dropbox (ASU)/lgherar1/desktop/current/Papers/Legacy over time/data/cover19.csv")
# Read treatments
treat<- read.csv("C:/Users/lgherar1.ASURITE/Dropbox (ASU)/lgherar1/desktop/current/Papers/Legacy over time/data/treat.csv")

# Convert cover to ANPP using coefficients from biomass calibration
anpp09<-data.frame(Pgrass=cover09[,2]*480.48,Agrass=cover09[,3]*128.12,prgl=cover09[,4]*0.00004,
                   Sshrub=cover09[,5]*261.56,forb=(cover09[,6]+cover09[,7])*138.44,
                   litter=cover09[,8],bare=cover09[,9],sd=cover09[,10],plot=cover09[,1])
anpp09$total<-anpp09[,1]+anpp09[,2]+anpp09[,3]+anpp09[,4]+anpp09[,5]
anpp09$year<-2009

anpp10<-data.frame(Pgrass=cover10[,2]*480.48,Agrass=cover10[,3]*128.12,prgl=cover10[,4]*0.00004,
                   Sshrub=cover10[,5]*261.56,forb=(cover10[,6]+cover10[,7])*138.44,
                   litter=cover10[,8],bare=cover10[,9],sd=cover10[,10],plot=cover10[,1])
anpp10$total<-anpp10[,1]+anpp10[,2]+anpp10[,3]+anpp10[,4]+anpp10[,5]
anpp10$year<-2010

anpp11<-data.frame(Pgrass=cover11[,2]*480.48,Agrass=cover11[,3]*128.12,prgl=cover11[,4]*0.00004,
                   Sshrub=cover11[,5]*261.56,forb=(cover11[,6]+cover11[,7])*138.44,
                   litter=cover11[,8],bare=cover11[,9],sd=cover11[,10],plot=cover11[,1])
anpp11$total<-anpp11[,1]+anpp11[,2]+anpp11[,3]+anpp11[,4]+anpp11[,5]
anpp11$year<-2011

anpp12<-data.frame(Pgrass=cover12[,2]*480.48,Agrass=cover12[,3]*128.12,prgl=cover12[,4]*0.00004,
                   Sshrub=cover12[,5]*261.56,forb=(cover12[,6]+cover12[,7])*138.44,
                   litter=cover12[,8],bare=cover12[,9],sd=cover12[,10],plot=cover12[,1])
anpp12$total<-anpp12[,1]+anpp12[,2]+anpp12[,3]+anpp12[,4]+anpp12[,5]
anpp12$year<-2012

anpp13<-data.frame(Pgrass=cover13[,2]*480.48,Agrass=cover13[,3]*128.12,prgl=cover13[,4]*0.00004,
                   Sshrub=cover13[,5]*261.56,forb=(cover13[,6]+cover13[,7])*138.44,
                   litter=cover13[,8],bare=cover13[,9],sd=cover13[,10],plot=cover13[,1])
anpp13$total<-anpp13[,1]+anpp13[,2]+anpp13[,3]+anpp13[,4]+anpp13[,5]
anpp13$year<-2013

anpp14<-data.frame(Pgrass=cover14[,2]*480.48,Agrass=cover14[,3]*128.12,prgl=cover14[,4]*0.00004,
                   Sshrub=cover14[,5]*261.56,forb=(cover14[,6]+cover14[,7])*138.44,
                   litter=cover14[,8],bare=cover14[,9],sd=cover14[,10],plot=cover14[,1])
anpp14$total<-anpp14[,1]+anpp14[,2]+anpp14[,3]+anpp14[,4]+anpp14[,5]
anpp14$year<-2014

anpp15<-data.frame(Pgrass=cover15[,2]*480.48,Agrass=cover15[,3]*128.12,prgl=cover15[,4]*0.00004,
                   Sshrub=cover15[,5]*261.56,forb=(cover15[,6]+cover15[,7])*138.44,
                   litter=cover15[,8],bare=cover15[,9],sd=cover15[,10],plot=cover15[,1])
anpp15$total<-anpp15[,1]+anpp15[,2]+anpp15[,3]+anpp15[,4]+anpp15[,5]
anpp15$year<-2015

anpp16<-data.frame(Pgrass=cover16[,2]*480.48,Agrass=cover16[,3]*128.12,prgl=cover16[,4]*0.00004,
                   Sshrub=cover16[,5]*261.56,forb=(cover16[,6]+cover16[,7])*138.44,
                   litter=cover16[,8],bare=cover16[,9],sd=cover16[,10],plot=cover16[,1])
anpp16$total<-anpp16[,1]+anpp16[,2]+anpp16[,3]+anpp16[,4]+anpp16[,5]
anpp16$year<-2016

anpp17<-data.frame(Pgrass=cover17[,2]*480.48,Agrass=cover17[,3]*128.12,prgl=cover17[,4]*0.00004,
                   Sshrub=cover17[,5]*261.56,forb=(cover17[,6]+cover17[,7])*138.44,
                   litter=cover17[,8],bare=cover17[,9],sd=cover17[,10],plot=cover17[,1])
anpp17$total<-anpp17[,1]+anpp17[,2]+anpp17[,3]+anpp17[,4]+anpp17[,5]
anpp17$year<-2017

anpp18<-data.frame(Pgrass=cover18[,2]*480.48,Agrass=cover18[,3]*128.12,prgl=cover18[,4]*0.00004,
                   Sshrub=cover18[,5]*261.56,forb=(cover18[,6]+cover18[,7])*138.44,
                   litter=cover18[,8],bare=cover18[,9],sd=cover18[,10],plot=cover18[,1])
anpp18$total<-anpp18[,1]+anpp18[,2]+anpp18[,3]+anpp18[,4]+anpp18[,5]
anpp18$year<-2018

anpp19<-data.frame(Pgrass=cover19[,2]*480.48,Agrass=cover19[,3]*128.12,prgl=cover19[,4]*0.00004,
                   Sshrub=cover19[,5]*261.56,forb=(cover19[,6]+cover19[,7])*138.44,
                   litter=cover19[,8],bare=cover19[,9],sd=cover19[,10],plot=cover19[,1])
anpp19$total<-anpp19[,1]+anpp19[,2]+anpp19[,3]+anpp19[,4]+anpp19[,5]
anpp19$year<-2019

ANPP<-rbind(anpp09,anpp10,anpp11,anpp12,anpp13,anpp14,anpp15,anpp16,anpp17,anpp18,anpp19)
ANPP$rare<-ANPP$Agrass+ANPP$Sshrub+ANPP$forb

ANPP<-merge(ANPP,treat,by="plot")

ANPP$ppt2009<-ANPP$t2009*73.7
ANPP$ppt2010<-ANPP$t2010*128.8
ANPP$ppt2011<-ANPP$t2011*95.0
ANPP$ppt2012<-ANPP$t2012*65.6
ANPP$ppt2013<-ANPP$t2013*210.5
ANPP$ppt2014<-ANPP$t2014*191.9
ANPP$ppt2015<-ANPP$t2015*154.94
ANPP$ppt2016<-ANPP$t2016*123.7
ANPP$ppt2017<-ANPP$t2017*214.12
ANPP$ppt2018<-ANPP$t2018*177.04
ANPP$ppt2019<-ANPP$t2019*160.53

# Calculate mean PPT for each treatment
ANPP$meanPPT<-(ANPP$ppt2009+ANPP$ppt2010+ANPP$ppt2011+ANPP$ppt2012+ANPP$ppt2013+ANPP$ppt2014+ANPP$ppt2015+ANPP$ppt2016+ANPP$ppt2017+ANPP$ppt2018+ANPP$ppt2019)/11

# Calculate ppt SD
for (i in 1:length(ANPP[,1])){
  ANPP$pptsd[i]<-sd(c(ANPP[i,"ppt2009"],ANPP[i,"ppt2010"],ANPP[i,"ppt2011"],ANPP[i,"ppt2012"]
                      ,ANPP[i,"ppt2013"],ANPP[i,"ppt2014"],ANPP[i,"ppt2015"],ANPP[i,"ppt2016"]
                      ,ANPP[i,"ppt2017"],ANPP[i,"ppt2018"],ANPP[i,"ppt2019"]))}

# Calculate ppt CV
ANPP$pptCV<-(ANPP$pptsd/ANPP$meanPPT)*100
yearPPT<-data.frame(year=c(2009:2019),ppt=c(73.7,128.8,95,65.6,210.5,191.9,154.94,123.7,214.12,177.04,160.53))
ANPP<-merge(ANPP,yearPPT,by="year")
# write.csv(ANPP,"data/ANPPmetadata14.csv")

# Rename treatments as 50% and 80% increased variability. This groups treatments starting from drought and treatments starting from irrigation.
treats<-sort(unique(ANPP$pptCV))
treat<-c("ambient","50%inc","80%inc")

ANPP$treat<-ifelse(ANPP$pptCV==treats[1],treat[1],NA)
ANPP$treat<-ifelse(ANPP$pptCV==treats[2],treat[2],ANPP$treat)
ANPP$treat<-ifelse(ANPP$pptCV==treats[3],treat[2],ANPP$treat)
ANPP$treat<-ifelse(ANPP$pptCV==treats[4],treat[3],ANPP$treat)
ANPP$treat<-ifelse(ANPP$pptCV==treats[5],treat[3],ANPP$treat)

ANPP$treat_ppt<-c(ANPP$ppt2009[1:50],
                  ANPP$ppt2010[51:100],
                  ANPP$ppt2011[101:150],
                  ANPP$ppt2012[151:200],
                  ANPP$ppt2013[201:250],
                  ANPP$ppt2014[251:300],
                  ANPP$ppt2015[301:350],
                  ANPP$ppt2016[351:400],
                  ANPP$ppt2017[401:450],
                  ANPP$ppt2018[451:500],
                  ANPP$ppt2019[501:550])

ANPP$total_WUE<-ANPP$total/ANPP$treat_ppt
ANPP$Pgrass_WUE<-ANPP$Pgrass/ANPP$treat_ppt
ANPP$prgl_WUE<-ANPP$prgl/ANPP$treat_ppt
ANPP$gr_sh<-ANPP$prgl/ANPP$total
# ANPP$treat<-as.factor(ANPP$treat)
# ANPP<-within(ANPP,treat<-relevel(treat,ref=3))
keep(list = list("ANPP","treat"),sure=T)
