# Load needed libraries ---------------------------------------------------
my_packages<-c("reshape2","MASS","tgp","plyr","nlme","ggplot2","ggthemes","RColorBrewer")
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

# Read treatments
treat<- read.csv("C:/Users/lgherar1.ASURITE/Dropbox (ASU)/lgherar1/desktop/current/Papers/Legacy over time/data/treat.csv")

# Convert cover to ANPP using coefficients from biomass calibration
anpp09<-data.frame(Pgrass=cover09[,2]*480.48,Agrass=cover09[,3]*128.12,prgl=cover09[,4]*0.00004,
                   Sshrub=cover09[,5]*261.56,forb=cover09[,6]*138.44,
                   litter=cover09[,7],bare=cover09[,8],sd=cover09[,9],plot=cover09[,1])
anpp09$total<-anpp09[,1]+anpp09[,2]+anpp09[,3]+anpp09[,4]+anpp09[,5]
anpp09$year<-2009

anpp10<-data.frame(Pgrass=cover10[,2]*480.48,Agrass=cover10[,3]*128.12,prgl=cover10[,4]*0.00004,
                   Sshrub=cover10[,5]*261.56,forb=cover10[,6]*138.44,
                   litter=cover10[,7],bare=cover10[,8],sd=cover10[,9],plot=cover10[,1])
anpp10$total<-anpp10[,1]+anpp10[,2]+anpp10[,3]+anpp10[,4]+anpp10[,5]
anpp10$year<-2010

anpp11<-data.frame(Pgrass=cover11[,2]*480.48,Agrass=cover11[,3]*128.12,prgl=cover11[,4]*0.00004,
                   Sshrub=cover11[,5]*261.56,forb=cover11[,6]*138.44,
                   litter=cover11[,7],bare=cover11[,8],sd=cover11[,9],plot=cover11[,1])
anpp11$total<-anpp11[,1]+anpp11[,2]+anpp11[,3]+anpp11[,4]+anpp11[,5]
anpp11$year<-2011

anpp12<-data.frame(Pgrass=cover12[,2]*480.48,Agrass=cover12[,3]*128.12,prgl=cover12[,4]*0.00004,
                   Sshrub=cover12[,5]*261.56,forb=cover12[,6]*138.44,
                   litter=cover12[,7],bare=cover12[,8],sd=cover12[,9],plot=cover12[,1])
anpp12$total<-anpp12[,1]+anpp12[,2]+anpp12[,3]+anpp12[,4]+anpp12[,5]
anpp12$year<-2012

anpp13<-data.frame(Pgrass=cover13[,2]*480.48,Agrass=cover13[,3]*128.12,prgl=cover13[,4]*0.00004,
                   Sshrub=cover13[,5]*261.56,forb=cover13[,6]*138.44,
                   litter=cover13[,7],bare=cover13[,8],sd=cover13[,9],plot=cover13[,1])
anpp13$total<-anpp13[,1]+anpp13[,2]+anpp13[,3]+anpp13[,4]+anpp13[,5]
anpp13$year<-2013

anpp14<-data.frame(Pgrass=cover14[,2]*480.48,Agrass=cover14[,3]*128.12,prgl=cover14[,4]*0.00004,
                   Sshrub=cover14[,5]*261.56,forb=cover14[,6]*138.44,
                   litter=cover14[,7],bare=cover14[,8],sd=cover14[,9],plot=cover14[,1])
anpp14$total<-anpp14[,1]+anpp14[,2]+anpp14[,3]+anpp14[,4]+anpp14[,5]
anpp14$year<-2014

anpp15<-data.frame(Pgrass=cover15[,2]*480.48,Agrass=cover15[,3]*128.12,prgl=cover15[,4]*0.00004,
                   Sshrub=cover15[,5]*261.56,forb=cover15[,6]*138.44,
                   litter=cover15[,7],bare=cover15[,8],sd=cover15[,9],plot=cover15[,1])
anpp15$total<-anpp15[,1]+anpp15[,2]+anpp15[,3]+anpp15[,4]+anpp15[,5]
anpp15$year<-2015

anpp16<-data.frame(Pgrass=cover16[,2]*480.48,Agrass=cover16[,3]*128.12,prgl=cover16[,4]*0.00004,
                   Sshrub=cover16[,5]*261.56,forb=cover16[,6]*138.44,
                   litter=cover16[,7],bare=cover16[,8],sd=cover16[,9],plot=cover16[,1])
anpp16$total<-anpp16[,1]+anpp16[,2]+anpp16[,3]+anpp16[,4]+anpp16[,5]
anpp16$year<-2016



ANPP<-rbind(anpp09,anpp10,anpp11,anpp12,anpp13,anpp14,anpp15,anpp16)
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

# Calculate mean PPT for each treatment
ANPP$meanPPT<-(ANPP$ppt2009+ANPP$ppt2010+ANPP$ppt2011+ANPP$ppt2012+ANPP$ppt2013+ANPP$ppt2014+ANPP$ppt2015+ANPP$ppt2016)/8

# Calculate ppt SD
for (i in 1:400){
  ANPP$pptsd[i]<-sd(c(ANPP[i,21],ANPP[i,22],ANPP[i,23],ANPP[i,24],ANPP[i,25],ANPP[i,26],ANPP[i,27],ANPP[i,28]))}

# Calculate ppt CV
ANPP$pptCV<-(ANPP$pptsd/ANPP$meanPPT)*100
yearPPT<-data.frame(year=c(2009:2016),ppt=c(73.7,128.8,95,65.6,210.5,191.9,154.94,123.7))
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
                  ANPP$ppt2016[351:400])
ANPP$total_WUE<-ANPP$total/ANPP$treat_ppt
ANPP$Pgrass_WUE<-ANPP$Pgrass/ANPP$treat_ppt
ANPP$prgl_WUE<-ANPP$prgl/ANPP$treat_ppt
ANPP$gr_sh<-ANPP$prgl/ANPP$total
# ANPP$treat<-as.factor(ANPP$treat)
# ANPP<-within(ANPP,treat<-relevel(treat,ref=3))

# ANPP data for figures ------------------------------------------------------
mean_treat_ANPP<-aggregate(ANPP,by=list(ANPP$year,ANPP$treat),FUN=mean,na.rm=T)
mean_treat_ANPP<-mean_treat_ANPP[,-35]
mean_treat_ANPP <- rename(mean_treat_ANPP, c(Group.1="year",Group.2="treat"))

seANPP<-aggregate(ANPP,by=list(ANPP$year,ANPP$treat),FUN=sd,na.rm=T)
seANPP<-seANPP[,-c(3:4,10:12,15:36)]
seANPP[,c(3:13)]<-seANPP[,3:13]/sqrt(10)
seANPP <- rename(seANPP, c(Group.1="year",Group.2="treat"))

treatments<-treat

#           blue         red       
colo<-c(1,"#377EB8",   "#E41A1C")
# colo<-c("#377EB8","#E41A1C",1)

# Figure 1 ----------------------------------------------------------------
png("figures/Fig1_A_B_V1.png",res = 350, pointsize = 22,height=12,width=7,units= "in")
par(las=1,lwd=1,cex.lab=1,mgp=c(11.75,0.35,0),oma=c(3,3,0.5,.5),tck=-0.02,mfrow=c(4,1),family="sans",xpd=NA,lwd=2)
# total ANPP
dis<-c(0,0,0,0,0,1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5,6,6,6,6,6)/20
dist<-c(0,1,2,3,4,5)/2000
d=50
for (i in 1:5){
  df<-subset(mean_treat_ANPP,mean_treat_ANPP$treat == treatments[i])
  if (i==1) {#tiff("figures/total_2year_ANPP.tiff",res = 250, pointsize = 12,height=3,width=4.5,units= "in")
    par(las=1,lwd=2,cex.axis=1,cex.lab=0.95,mgp=c(111.5,0.35,0),mar=c(0.75,1.5,0.5,0),tck=-0.015)
    plot(df$year,df$total,type="l",ylim=c(0,230),xlim=c(2009,2016),col=colo[i],ylab="" ,xlab="year period (years)",xaxt="n",bty="l",lab=c(4,4,7))
    for(x in 1 : length(mean_treat_ANPP$year))
    {arrows(mean_treat_ANPP$year[x]+dis[x]/d,mean_treat_ANPP$total[x]-seANPP$total[x],
            mean_treat_ANPP$year[x]+dis[x]/d,mean_treat_ANPP$total[x]+seANPP$total[x],code=3, angle=90,length=.03,lwd=1)}
    points(df$year+dist[i]/d,df$total,pch=16,col=colo[i],cex=1.25)
  }
  else{lines(df$year+dist[i]/d,df$total,col=colo[i],lty=if(i>=4){2})
    points(df$year+dist[i]/d,df$total,pch=16,col=colo[i],cex=1.25)
  }
}
mtext("Total",3,line=-0.5)
text(2009,230,"a)",cex=1.25,font=2)

for (i in 1:3){ 
  # i=1
  df<-subset(mean_treat_ANPP,mean_treat_ANPP$treat == treatments[i])
  if (i==1) {#tiff("figures/Pgrass_2year_ANPP.tiff",res = 250, pointsize = 12,height=3,width=4.5,units= "in")
    par(las=1,lwd=2,cex.axis=1,cex.lab=0.95,mgp=c(111.5,0.35,0),mar=c(0.75,1.5,0.5,0),tck=-0.015)
    plot(df$year,df$Pgrass,type="l",ylim=c(0,200),xlim=c(2009,2016),col=colo[i],ylab="" ,xlab="year period (years)",xaxt="n",bty="l",lab=c(5,4,7))
    for(x in 1 : length(mean_treat_ANPP$year))
    {arrows(mean_treat_ANPP$year[x],mean_treat_ANPP$Pgrass[x]-seANPP$Pgrass[x],
            mean_treat_ANPP$year[x],mean_treat_ANPP$Pgrass[x]+seANPP$Pgrass[x],code=3, angle=90,length=.03,lwd=1)}
    points(df$year,df$Pgrass,pch=16,col=colo[i],cex=1.25)
  } 
  else{lines(df$year+dist[i]/d,df$Pgrass,col=colo[i],lty=if(i>=4){2})
    points(df$year+dist[i]/d,df$Pgrass,pch=16,col=colo[i],cex=1.25)
  }
}

mtext("Perennial grass species",3,line=-0.5)
text(2009,200,"b)",cex=1.25,font=2)



for (i in 1:5){ 
  df<-subset(mean_treat_ANPP,mean_treat_ANPP$treat == treatments[i])
  if (i==1) {#tiff("figures/rare_2year_ANPP.tiff",res = 250, pointsize = 12,height=3,width=4.5,units= "in")
    par(las=1,lwd=2,cex.axis=1,cex.lab=0.95,mgp=c(111.5,0.35,0),mar=c(0.75,1.5,0.5,0),tck=-0.015)
    plot(df$year,df$rare,type="l",ylim=c(0,80),xlim=c(2009,2016),col=colo[i],ylab="" ,xlab="year period (years)",xaxt="n",bty="l",lab=c(5,4,7))
    for(x in 1 : length(mean_treat_ANPP$year))
    {arrows(mean_treat_ANPP$year[x],mean_treat_ANPP$rare[x]-seANPP$rare[x],
            mean_treat_ANPP$year[x],mean_treat_ANPP$rare[x]+seANPP$rare[x],code=3, angle=90,length=.03,lwd=1)}
    points(df$year,df$rare,pch=16,col=colo[i],cex=1.25)
  } 
  else{lines(df$year+dist[i]/d,df$rare,col=colo[i],lty=if(i>=4){2})
    points(df$year+dist[i]/d,df$rare,pch=16,col=colo[i],cex=1.25)
  }
}

mtext("Rare species",3,line=-0.5)
text(2009,80,"c)",cex=1.25,font=2)

for (i in 1:5){ 
  df<-subset(mean_treat_ANPP,mean_treat_ANPP$treat == treatments[i])
  if (i==1) {#tiff("figures/prgl_2year_ANPP.tiff",res = 250, pointsize = 12,height=3,width=4.5,units= "in")
    par(las=1,lwd=2,cex.axis=1,cex.lab=0.95,mgp=c(111.5,0.35,0),mar=c(0.75,1.5,0.5,0),tck=-0.015)
    plot(df$year,df$prgl,type="l",ylim=c(0,80),xlim=c(2009,2016),col=colo[i],ylab="" ,xlab="year period (years)",xaxt="n",bty="l",lab=c(5,4,7))
    for(x in 1 : length(mean_treat_ANPP$year))
    {arrows(mean_treat_ANPP$year[x],mean_treat_ANPP$prgl[x]-seANPP$prgl[x],
            mean_treat_ANPP$year[x],mean_treat_ANPP$prgl[x]+seANPP$prgl[x],code=3, angle=90,length=.03,lwd=1)}
    points(df$year,df$prgl,pch=16,col=colo[i],cex=1.25)
  } 
  else{lines(df$year+dist[i]/d,df$prgl,col=colo[i],lty=if(i>=4){2})
    points(df$year+dist[i]/d,df$prgl,pch=16,col=colo[i],cex=1.25)
  }
}

mtext("Shrub species",3,line=-0.5)
text(2009,80,"d)",cex=1.25,font=2)

# legend(2009,80,col=colo,
#        legend=c("ambient variability","increased variability +/-50% CV",
#                 "increased variability +/-80% CV")
#        ,lwd=2,lty=c(1,1,1),cex=0.9,bty="n",seg.len=4)
axis(side=1,at=c(2009:2016),labels=c(2009:2016),line=0,cex.axis=1)

mtext("Year",1,line=2.25,cex=1.35)

mtext(expression(paste("ANPP (g  ",m^-2," ",yr^-1,")",sep=" ")),2,las=0,outer=T,cex=1.35,line=0.5)



dev.off()

# Figure 2 ----------------------------------------------------------------
png("figures/Fig2_V1.png",res = 350, pointsize = 22,height=11,width=6,units= "in")
par(las=1,lwd=1,cex.lab=1,mgp=c(11.75,0.35,0),oma=c(3,3,0.5,.5),tck=-0.02,mfrow=c(3,1),family="sans",xpd=NA,lwd=2)

for (i in 1:5){
  df<-subset(mean_treat_ANPP,mean_treat_ANPP$treat == treatments[i])
  if (i==1) {#tiff("figures/total_WUE_2year_ANPP.tiff",res = 250, pointsize = 12,height=3,width=4.5,units= "in")
    par(las=1,lwd=2,cex.axis=1,cex.lab=0.95,mgp=c(111.5,0.35,0),mar=c(0.75,1.5,0.5,0),tck=-0.015)
    plot(df$year,df$total_WUE,type="l",ylim=c(0,3),xlim=c(2009,2016),col=colo[i],ylab="" ,xlab="year period (years)",xaxt="n",bty="l",lab=c(5,7,7))
    for(x in 1 : length(mean_treat_ANPP$year))
    {arrows(mean_treat_ANPP$year[x],mean_treat_ANPP$total_WUE[x]-seANPP$total_WUE[x],
            mean_treat_ANPP$year[x],mean_treat_ANPP$total_WUE[x]+seANPP$total_WUE[x],code=3, angle=90,length=.05,lwd=1)}
    points(df$year,df$total_WUE,pch=16,col=colo[i],cex=1.25)
  }
  else{lines(df$year,df$total_WUE,col=colo[i],lty=if(i>=4){2})
    points(df$year,df$total_WUE,pch=16,col=colo[i],cex=1.25)
  }
}

mtext("Total",3,line=-0.25)
text(2009,3,"a)",cex=1.25,font=2)

for (i in 1:5){
  df<-subset(mean_treat_ANPP,mean_treat_ANPP$treat == treatments[i])
  if (i==1) {#tiff("figures/Pgrass_WUE_2year_ANPP.tiff",res = 250, pointsize = 12,height=3,width=4.5,units= "in")
    par(las=1,lwd=2,cex.axis=1,cex.lab=0.95,mgp=c(111.5,0.35,0),mar=c(0.75,1.5,0.5,0),tck=-0.015)
    plot(df$year,df$Pgrass_WUE,type="l",ylim=c(0,2.5),xlim=c(2009,2016),col=colo[i],ylab="" ,xlab="year period (years)",xaxt="n",bty="l",lab=c(5,7,7))
    for(x in 1 : length(mean_treat_ANPP$year))
    {arrows(mean_treat_ANPP$year[x],mean_treat_ANPP$Pgrass_WUE[x]-seANPP$Pgrass_WUE[x],
            mean_treat_ANPP$year[x],mean_treat_ANPP$Pgrass_WUE[x]+seANPP$Pgrass_WUE[x],code=3, angle=90,length=.05,lwd=1)}
    points(df$year,df$Pgrass_WUE,pch=16,col=colo[i],cex=1.25)
  }
  else{lines(df$year,df$Pgrass_WUE,col=colo[i],lty=if(i>=4){2})
    points(df$year,df$Pgrass_WUE,pch=16,col=colo[i],cex=1.25)
  }
}

mtext("Perennial grass species",3,line=-0.25)
text(2009,2.5,"b)",cex=1.25,font=2)

legend(2011.5,2.5,col=colo,
       legend=c("ambient variability 40% CV","increased variability 70% CV",
                "increased variability 100% CV" )
       ,lwd=2,lty=c(1,1,1),pch=16,cex=0.9,bty="n",seg.len=3)

for (i in 1:5){
  df<-subset(mean_treat_ANPP,mean_treat_ANPP$treat == treatments[i])
  if (i==1) {#tiff("figures/prgl_WUE_2year_ANPP.tiff",res = 250, pointsize = 12,height=3,width=4.5,units= "in")
    par(las=1,lwd=2,cex.axis=1,cex.lab=0.95,mgp=c(111.5,0.35,0),mar=c(0.75,1.5,0.5,0),tck=-0.015)
    plot(df$year,df$prgl_WUE,type="l",ylim=c(0,1.75),xlim=c(2009,2016),col=colo[i],ylab="" ,xlab="year period (years)",xaxt="n",bty="l",lab=c(5,4,7))
    for(x in 1 : length(mean_treat_ANPP$year))
    {arrows(mean_treat_ANPP$year[x],mean_treat_ANPP$prgl_WUE[x]-seANPP$prgl_WUE[x],
            mean_treat_ANPP$year[x],mean_treat_ANPP$prgl_WUE[x]+seANPP$prgl_WUE[x],code=3, angle=90,length=.05,lwd=1)}
    points(df$year,df$prgl_WUE,pch=16,col=colo[i],cex=1.25)
  }
  else{lines(df$year,df$prgl_WUE,col=colo[i],lty=if(i>=4){2})
    points(df$year,df$prgl_WUE,pch=16,col=colo[i],cex=1.25)
  }
}
mtext("Shrub species",3,line=-0.25)
text(2009,1.75,"c)",cex=1.25,font=2)
axis(side=1,at=c(2009:2016),labels=c(2009:2016),line=0,cex.axis=1)

mtext("Year",1,line=2.25,cex=1.35)

mtext(expression(paste("WUE (g  ",m^-2," ",mm^-1,")",sep=" ")),2,las=0,outer=T,cex=1.35,line=0.5)
dev.off()


# Legacy calculation ------------------------------------------------------
# where Legacy = treat ANPP - mean Control ANPP
lm_total<-lm(total~treat_ppt,data=ANPP[ANPP$treat=="ambient",])
summary(lm_total)
plot(total~treat_ppt,data=ANPP[ANPP$treat=="ambient",])
abline(lm_total)
lm_Pgrass<-lm(Pgrass~treat_ppt,data=ANPP[ANPP$treat=="ambient",])
summary(lm_Pgrass)
lm_shrub<-lm(prgl~treat_ppt,data=ANPP[ANPP$treat=="ambient",])
summary(lm_shrub)
lm_rare<-lm(rare~treat_ppt,data=ANPP[ANPP$treat=="ambient",])
summary(lm_rare)

ANPP$expANPP_total<-summary(lm_total)$coeff[1,1]+ANPP$treat_ppt*summary(lm_total)$coeff[2,1]
ANPP$expANPP_Pgrass<-summary(lm_Pgrass)$coeff[1,1]+ANPP$treat_ppt*summary(lm_Pgrass)$coeff[2,1]
ANPP$expANPP_shrub<-summary(lm_shrub)$coeff[1,1]+ANPP$treat_ppt*summary(lm_shrub)$coeff[2,1]
ANPP$expANPP_rare<-summary(lm_rare)$coeff[1,1]+ANPP$treat_ppt*summary(lm_rare)$coeff[2,1]

lagdf<-ANPP[,c(1,3,5,12,11,29,31,32,33,38:42)]
lagdf$lag_total<-lagdf$total-lagdf$expANPP_total
lagdf$lag_Pgrass<-lagdf$Pgrass-lagdf$expANPP_Pgrass
lagdf$lag_prgl<-lagdf$prgl-lagdf$expANPP_shrub
lagdf$lag_rare<-lagdf$rare-lagdf$expANPP_rare

# Legacy data for figures -------------------------------------------------
lagdf$treat<-as.factor(lagdf$treat)

abs_leg<-aggregate(abs(lagdf[,-9]),by=list(lagdf$year,lagdf$treat),FUN=mean,na.rm=T)
abs_leg <- rename(abs_leg, c(Group.1="year",Group.2="treat"))
abs_leg$var_type<-ifelse(abs_leg$treat=="ambient","ambient","increased")

agg_abs_leg<-aggregate(abs_leg[,-c(1:3,20)],by=list(abs_leg$var_type,abs_leg$year),FUN=mean,na.rm=T)
agg_abs_leg <- rename(agg_abs_leg, c(Group.1="var_type",Group.2="year"))
agg_abs_leg$gr_sh<-agg_abs_leg$Pgrass/(agg_abs_leg$prgl)

abs_leg_dif<-agg_abs_leg[agg_abs_leg$var_type=="increased",c(1:6)] 
abs_leg_dif[,c(3:6)]<-abs_leg_dif[,c(3:6)] - agg_abs_leg[agg_abs_leg$var_type=="ambient",c(3:6)]

selagdf<-aggregate(abs(lagdf[,-9]),by=list(lagdf$year,lagdf$treat),FUN=sd,na.rm=T)
selagdf<-selagdf[,-c(3,8:14)]
selagdf[,3:10]<-selagdf[,3:10]/sqrt(10)
selagdf <- rename(selagdf, c(Group.1="year",Group.2="treat"))

# Figure 3 absolute legacies Grass and Shrub ------------------------------------------------------------------------
png("figures/Fig3_V1.png",res = 350, pointsize = 12,height=8,width=6,units= "in")
par(las=1,lwd=1,cex.lab=1,mgp=c(11.75,0.35,0),oma=c(3,3,0.5,.5),tck=-0.02,mfrow=c(2,1),family="sans",xpd=NA,lwd=2)

for (i in 1:3){
  df<-subset(abs_leg,abs_leg$treat == treatments[i])
  if (i==1) {#tiff("figures/lag_Pgrass_2year_ANPP.tiff",res = 250, pointsize = 12,height=3,width=4.5,units= "in")
    par(las=1,lwd=2,cex.axis=1,cex.lab=0.95,mgp=c(111.5,0.35,0),mar=c(0.75,1.5,0.5,0),tck=-0.015)
    plot(df$year,df$lag_Pgrass,type="l",ylim=c(0,150),xlim=c(2009,2016),col=colo[i],ylab="" ,xlab="year period (years)",xaxt="n",bty="l",lab=c(5,7,7))
    for(x in 1 : length(abs_leg$year))
    {arrows(abs_leg$year[x],abs_leg$lag_Pgrass[x]-selagdf$lag_Pgrass[x],
            abs_leg$year[x],abs_leg$lag_Pgrass[x]+selagdf$lag_Pgrass[x],code=3, angle=90,length=.05,lwd=1)}
    points(df$year,df$lag_Pgrass,pch=16,col=colo[i],cex=1.25)
  }
  else{lines(df$year,df$lag_Pgrass,col=colo[i],lty=if(i>=4){2})
    points(df$year,df$lag_Pgrass,pch=16,col=colo[i],cex=1.25)
  }
}

mtext("Perennial grass species",3,line=-0.25)
text(2009,150,"a)",cex=1.25,font=2)

legend(2009,142,col=colo,
       legend=c("ambient variability 40% CV","increased variability 70% CV",
                "increased variability 100% CV" )
       ,lwd=2,lty=c(1,1,1),pch=16,cex=0.9,bty="n",seg.len=3)

for (i in 1:3){
  df<-subset(abs_leg,abs_leg$treat == treatments[i])
  if (i==1) {#tiff("figures/lag_prgl_2year_ANPP.tiff",res = 250, pointsize = 12,height=3,width=4.5,units= "in")
    par(las=1,lwd=2,cex.axis=1,cex.lab=0.95,mgp=c(111.5,0.35,0),mar=c(0.75,1.5,0.5,0),tck=-0.015)
    plot(df$year,df$lag_prgl,type="l",ylim=c(0,55),xlim=c(2009,2016),col=colo[i],ylab="" ,xlab="year period (years)",xaxt="n",bty="l",lab=c(5,7,7))
    for(x in 1 : length(abs_leg$year))
    {arrows(abs_leg$year[x],abs_leg$lag_prgl[x]-selagdf$lag_prgl[x],
            abs_leg$year[x],abs_leg$lag_prgl[x]+selagdf$lag_prgl[x],code=3, angle=90,length=.05,lwd=1)}
    points(df$year,df$lag_prgl,pch=16,col=colo[i],cex=1.25)
  }
  else{lines(df$year,df$lag_prgl,col=colo[i],lty=if(i>=4){2})
    points(df$year,df$lag_prgl,pch=16,col=colo[i],cex=1.25)
  }
}
mtext("Shrub species",3,line=-0.25)
text(2009,55,"b)",cex=1.25,font=2)
axis(side=1,at=c(2009:2016),labels=c(2009:2016),line=0,cex.axis=1)

mtext("Year",1,line=2.25,cex=1.35)

mtext(expression(paste("Absolute Legacy effect (g  ",m^-2," ",yr^-1,")",sep=" ")),2,las=0,outer=T,cex=1.35,line=0.5)
dev.off()


# Absolute legacy total ---------------------------------------------------
png("figures/Fig3_total_V2.png",res = 350, pointsize = 12,height=6,width=5,units= "in")
par(las=1,lwd=1,cex.lab=1.2,mgp=c(2,0.4,0),mar=c(0,0,0.5,0.5),oma=c(3.5,4,0.5,.5),tck=-0.02,mfrow=c(2,1),family="sans",xpd=NA,lwd=2)

layout(matrix(c(1,2),2,1),widths=c(1,2.25),heights = c(2,4))
par(bty="l")
plot(gr_sh~year,data=mean_treat_ANPP[mean_treat_ANPP$treat=="ambient",],type="l",ylim=c(0,1),lwd=2,lty=1,xaxt="n",ylab="",xlab="")
mtext(text = expression(paste("% Shrub ANPP",sep=" ")),side = 2,line = 2.35,las=0)
# mtext(text = expression(paste("Treated - Ambient (g  ",m^-2," ",yr^-1,")",sep=" ")),side = 2,line = 1.25,las=0,cex=0.8)
lines(gr_sh~year,data=mean_treat_ANPP[mean_treat_ANPP$treat=="80%inc",],lwd=2,col=colo[3])
lines(gr_sh~year,data=mean_treat_ANPP[mean_treat_ANPP$treat=="50%inc",],lwd=2,col=colo[2])
for(x in 1 : length(mean_treat_ANPP$year))
{arrows(mean_treat_ANPP$year[x],mean_treat_ANPP$gr_sh[x]-seANPP$gr_sh[x],
        mean_treat_ANPP$year[x],mean_treat_ANPP$gr_sh[x]+seANPP$gr_sh[x],code=3, angle=90,length=.05,lwd=1)}

points(gr_sh~year,data=mean_treat_ANPP[mean_treat_ANPP$treat=="ambient",],pch=16,col=colo[1],cex=1.25)
points(gr_sh~year,data=mean_treat_ANPP[mean_treat_ANPP$treat=="50%inc",],pch=16,col=colo[2],cex=1.25)
points(gr_sh~year,data=mean_treat_ANPP[mean_treat_ANPP$treat=="80%inc",],pch=16,col=colo[3],cex=1.25)

for (i in 1:3){
  df<-subset(abs_leg,abs_leg$treat == treatments[i])
  if (i==1) {#tiff("figures/lag_total_2year_ANPP.tiff",res = 250, pointsize = 12,height=3,width=4.5,units= "in")
    # par(las=1,lwd=2,cex.axis=1,cex.lab=1.1,mgp=c(1.75,0.35,0),mar=c(3.5,3.5,0,0.5),tck=-0.015)
    plot(df$year,df$lag_total,type="l",ylim=c(0,150),xlim=c(2009,2016),col=colo[i],ylab=expression(paste("Absolute legacy effect (g  ",m^-2," ",yr^-1,")",sep=" ")),xlab="",xaxt="n",bty="l",lab=c(5,5,7))
    for(x in 1 : length(abs_leg$year))
    {arrows(abs_leg$year[x],abs_leg$lag_total[x]-selagdf$lag_total[x],
            abs_leg$year[x],abs_leg$lag_total[x]+selagdf$lag_total[x],code=3, angle=90,length=.05,lwd=1)}
    points(df$year,df$lag_total,pch=16,col=colo[i],cex=1.25)
  }
  else{lines(df$year,df$lag_total,col=colo[i],lty=if(i>=4){2})
    points(df$year,df$lag_total,pch=16,col=colo[i],cex=1.25)
  }
}
# # lines(gr_sh~year,data=agg_abs_leg[agg_abs_leg$var_type=="increased",],type="l",lty=2,lwd=4)
# abline(h=mean(agg_abs_leg$gr_sh[agg_abs_leg$var_type=="ambient"]))
# axis(side=4,at=agg_abs_leg$gr_sh,labels=round(agg_abs_leg$gr_sh,0),line=0,cex.axis=1)
axis(side=1,at=c(2009:2016),labels=c(2009:2016),line=0,cex.axis=1)
mtext("Year",1,line=2.25,cex=1.35)
dev.off()

# Data for positive and neative legacy figures ----------------------------
# Total
total_pos_leg<-aggregate(lagdf[lagdf$lag_total>0,"lag_total"],by=list(lagdf$year[lagdf$lag_total>0],lagdf$treat[lagdf$lag_total>0]),FUN=mean,na.rm=T)
total_pos_leg <- rename(total_pos_leg, c(Group.1="year",Group.2="treat",x="lag_total"))
total_pos_leg$var_type<-ifelse(total_pos_leg$treat=="ambient","ambient","increased")

se_total_pos_leg<-aggregate(lagdf[lagdf$lag_total>0,"lag_total"],by=list(lagdf$year[lagdf$lag_total>0],lagdf$treat[lagdf$lag_total>0]),FUN=sd,na.rm=T)
se_total_pos_leg[,3]<-se_total_pos_leg[,3]/sqrt(10)
se_total_pos_leg <- rename(se_total_pos_leg, c(Group.1="year",Group.2="treat",x="lag_total"))

total_neg_leg<-aggregate(lagdf[lagdf$lag_total<0,"lag_total"],by=list(lagdf$year[lagdf$lag_total<0],lagdf$treat[lagdf$lag_total<0]),FUN=mean,na.rm=T)
total_neg_leg <- rename(total_neg_leg, c(Group.1="year",Group.2="treat",x="lag_total"))
total_neg_leg$var_type<-ifelse(total_neg_leg$treat=="ambient","ambient","increased")

se_total_neg_leg<-aggregate(lagdf[lagdf$lag_total<0,"lag_total"],by=list(lagdf$year[lagdf$lag_total<0],lagdf$treat[lagdf$lag_total<0]),FUN=sd,na.rm=T)
se_total_neg_leg[,3]<-se_total_neg_leg[,3]/sqrt(10)
se_total_neg_leg <- rename(se_total_neg_leg, c(Group.1="year",Group.2="treat",x="lag_total"))

# Pgrass
Pgrass_pos_leg<-aggregate(lagdf[lagdf$lag_Pgrass>0,"lag_Pgrass"],by=list(lagdf$year[lagdf$lag_Pgrass>0],lagdf$treat[lagdf$lag_Pgrass>0]),FUN=mean,na.rm=T)
Pgrass_pos_leg <- rename(Pgrass_pos_leg, c(Group.1="year",Group.2="treat",x="lag_Pgrass"))
Pgrass_pos_leg$var_type<-ifelse(Pgrass_pos_leg$treat=="ambient","ambient","increased")

se_Pgrass_pos_leg<-aggregate(lagdf[lagdf$lag_Pgrass>0,"lag_Pgrass"],by=list(lagdf$year[lagdf$lag_Pgrass>0],lagdf$treat[lagdf$lag_Pgrass>0]),FUN=sd,na.rm=T)
se_Pgrass_pos_leg[,3]<-se_Pgrass_pos_leg[,3]/sqrt(10)
se_Pgrass_pos_leg <- rename(se_Pgrass_pos_leg, c(Group.1="year",Group.2="treat",x="lag_Pgrass"))

Pgrass_neg_leg<-aggregate(lagdf[lagdf$lag_Pgrass<0,"lag_Pgrass"],by=list(lagdf$year[lagdf$lag_Pgrass<0],lagdf$treat[lagdf$lag_Pgrass<0]),FUN=mean,na.rm=T)
Pgrass_neg_leg <- rename(Pgrass_neg_leg, c(Group.1="year",Group.2="treat",x="lag_Pgrass"))
Pgrass_neg_leg$var_type<-ifelse(Pgrass_neg_leg$treat=="ambient","ambient","increased")

se_Pgrass_neg_leg<-aggregate(lagdf[lagdf$lag_Pgrass<0,"lag_Pgrass"],by=list(lagdf$year[lagdf$lag_Pgrass<0],lagdf$treat[lagdf$lag_Pgrass<0]),FUN=sd,na.rm=T)
se_Pgrass_neg_leg[,3]<-se_Pgrass_neg_leg[,3]/sqrt(10)
se_Pgrass_neg_leg <- rename(se_Pgrass_neg_leg, c(Group.1="year",Group.2="treat",x="lag_Pgrass"))

# Shrub
prgl_pos_leg<-aggregate(lagdf[lagdf$lag_prgl>0,"lag_prgl"],by=list(lagdf$year[lagdf$lag_prgl>0],lagdf$treat[lagdf$lag_prgl>0]),FUN=mean,na.rm=T)
prgl_pos_leg <- rename(prgl_pos_leg, c(Group.1="year",Group.2="treat",x="lag_prgl"))
prgl_pos_leg$var_type<-ifelse(prgl_pos_leg$treat=="ambient","ambient","increased")

se_prgl_pos_leg<-aggregate(lagdf[lagdf$lag_prgl>0,"lag_prgl"],by=list(lagdf$year[lagdf$lag_prgl>0],lagdf$treat[lagdf$lag_prgl>0]),FUN=sd,na.rm=T)
se_prgl_pos_leg[,3]<-se_prgl_pos_leg[,3]/sqrt(10)
se_prgl_pos_leg <- rename(se_prgl_pos_leg, c(Group.1="year",Group.2="treat",x="lag_prgl"))

prgl_neg_leg<-aggregate(lagdf[lagdf$lag_prgl<0,"lag_prgl"],by=list(lagdf$year[lagdf$lag_prgl<0],lagdf$treat[lagdf$lag_prgl<0]),FUN=mean,na.rm=T)
prgl_neg_leg <- rename(prgl_neg_leg, c(Group.1="year",Group.2="treat",x="lag_prgl"))
prgl_neg_leg$var_type<-ifelse(prgl_neg_leg$treat=="ambient","ambient","increased")

se_prgl_neg_leg<-aggregate(lagdf[lagdf$lag_prgl<0,"lag_prgl"],by=list(lagdf$year[lagdf$lag_prgl<0],lagdf$treat[lagdf$lag_prgl<0]),FUN=sd,na.rm=T)
se_prgl_neg_leg[,3]<-se_prgl_neg_leg[,3]/sqrt(10)
se_prgl_neg_leg <- rename(se_prgl_neg_leg, c(Group.1="year",Group.2="treat",x="lag_prgl"))

# Figure 4 positive and negative legacies ---------------------------------

png("figures/Fig4_positive_negative_legacy_V1.png",res = 350, pointsize = 12,height=5,width=6,units= "in")
par(las=1,lwd=1,cex.lab=1,mgp=c(11.75,0.35,0),oma=c(3.5,4.5,0.5,.5),tck=-0.02,mfrow=c(2,1),family="sans",xpd=T,lwd=2)

for (i in 1:5){
  df<-subset(total_pos_leg,total_pos_leg$treat == treatments[i])
  if (i==1) {#tiff("figures/lag_total_2year_ANPP.tiff",res = 250, pointsize = 12,height=3,width=4.5,units= "in")
    par(las=1,lwd=2,cex.axis=1,cex.lab=1.35,mgp=c(1.75,0.35,0),mar=c(0,0,0.5,0.5),tck=-0.015)
    plot(df$year,df$lag_total,type="l",ylim=c(0,140),xlim=c(2009,2016),col=colo[i],ylab="",xlab="",xaxt="n",bty="l",lab=c(5,7,7),yaxs="i")
    
    for(x in 1 : length(total_pos_leg$year))
    {
      arrows(total_pos_leg$year[x],total_pos_leg$lag_total[x]-se_total_pos_leg$lag_total[x],
            total_pos_leg$year[x],total_pos_leg$lag_total[x]+se_total_pos_leg$lag_total[x],code=3, angle=90,length=.05,lwd=1)
      }
    points(df$year,df$lag_total,pch=16,col=colo[i],cex=1.25)
  }
  else{lines(df$year,df$lag_total,col=colo[i],lty=if(i>=4){2})
    points(df$year,df$lag_total,pch=16,col=colo[i],cex=1.25)
  }
}
mtext("Total ANPP",3,line=-0.25)
for (i in 1:5){
  df<-subset(total_neg_leg,total_neg_leg$treat == treatments[i])
  if (i==1) {#tiff("figures/lag_total_2year_ANPP.tiff",res = 250, pointsize = 12,height=3,width=4.5,units= "in")
    par(las=1,lwd=2,cex.axis=1,cex.lab=1.5,mgp=c(1.75,0.35,0),mar=c(0,0,0,0.5),tck=-0.015)
    plot(df$year,df$lag_total,type="l",ylim=c(-150,0),xlim=c(2009,2016),col=colo[i],ylab="",xlab="",xaxt="n",bty="l",lab=c(5,7,7),yaxs="i")
    for(x in 1 : length(total_neg_leg$year))
      {arrows(total_neg_leg$year[x],total_neg_leg$lag_total[x]-se_total_neg_leg$lag_total[x],
              total_neg_leg$year[x],total_neg_leg$lag_total[x]+se_total_neg_leg$lag_total[x],code=3, angle=90,length=.05,lwd=1)}
      points(df$year,df$lag_total,pch=16,col=colo[i],cex=1.25)
  }
  else{lines(df$year,df$lag_total,col=colo[i],lty=if(i>=4){2})
    points(df$year,df$lag_total,pch=16,col=colo[i],cex=1.25)
  }
}
axis(side=1,at=c(2009:2016),labels=c(2009:2016),line=0,cex.axis=1)
mtext("Year",1,line=1.65,cex=1.3)
mtext(expression(paste("Legacy effect (g  ",m^-2," ",yr^-1,")",sep=" ")),2,2,adj=10,cex=1.3,las=0)
mtext(expression(paste("Legacy effect (g  ",m^-2," ",yr^-1,")",sep=" ")),2,2,las=0,adj=8,outer=F,cex=1)
dev.off()

png("figures/Fig4_positive_negative_Pgrass_legacy_V1.png",res = 350, pointsize = 12,height=5,width=6,units= "in")
par(las=1,lwd=1,cex.lab=1,mgp=c(11.75,0.35,0),oma=c(3.5,4.5,0.5,.5),tck=-0.02,mfrow=c(2,1),family="sans",xpd=T,lwd=2)

for (i in 1:5){
  df<-subset(Pgrass_pos_leg,Pgrass_pos_leg$treat == treatments[i])
  if (i==1) {#tiff("figures/lag_Pgrass_2year_ANPP.tiff",res = 250, pointsize = 12,height=3,width=4.5,units= "in")
    par(las=1,lwd=2,cex.axis=1,cex.lab=1.35,mgp=c(1.75,0.35,0),mar=c(0,0,0.5,0.5),tck=-0.015)
    plot(df$year,df$lag_Pgrass,type="l",ylim=c(0,140),xlim=c(2009,2016),col=colo[i],ylab="",xlab="",xaxt="n",bty="l",lab=c(5,7,7),yaxs="i")
    for(x in 1 : length(Pgrass_pos_leg$year))
      {arrows(Pgrass_pos_leg$year[x],Pgrass_pos_leg$lag_Pgrass[x]-se_Pgrass_pos_leg$lag_Pgrass[x],
              Pgrass_pos_leg$year[x],Pgrass_pos_leg$lag_Pgrass[x]+se_Pgrass_pos_leg$lag_Pgrass[x],code=3, angle=90,length=.05,lwd=1)}
      points(df$year,df$lag_Pgrass,pch=16,col=colo[i],cex=1.25)
  }
  else{lines(df$year,df$lag_Pgrass,col=colo[i],lty=if(i>=4){2})
    points(df$year,df$lag_Pgrass,pch=16,col=colo[i],cex=1.25)
  }
}
mtext("Grass ANPP",3,line=-0.25)
for (i in 1:5){
  df<-subset(Pgrass_neg_leg,Pgrass_neg_leg$treat == treatments[i])
  if (i==1) {#tiff("figures/lag_Pgrass_2year_ANPP.tiff",res = 250, pointsize = 12,height=3,width=4.5,units= "in")
    par(las=1,lwd=2,cex.axis=1,cex.lab=1.5,mgp=c(1.75,0.35,0),mar=c(0,0,0,0.5),tck=-0.015)
    plot(df$year,df$lag_Pgrass,type="l",ylim=c(-140,0),xlim=c(2009,2016),col=colo[i],ylab="",xlab="",xaxt="n",bty="l",lab=c(5,7,7),yaxs="i")
    for(x in 1 : length(Pgrass_neg_leg$year))
      {arrows(Pgrass_neg_leg$year[x],Pgrass_neg_leg$lag_Pgrass[x]-se_Pgrass_neg_leg$lag_Pgrass[x],
              Pgrass_neg_leg$year[x],Pgrass_neg_leg$lag_Pgrass[x]+se_Pgrass_neg_leg$lag_Pgrass[x],code=3, angle=90,length=.05,lwd=1)}
      points(df$year,df$lag_Pgrass,pch=16,col=colo[i],cex=1.25)
  }
  else{lines(df$year,df$lag_Pgrass,col=colo[i],lty=if(i>=4){2})
    points(df$year,df$lag_Pgrass,pch=16,col=colo[i],cex=1.25)
  }
}
axis(side=1,at=c(2009:2016),labels=c(2009:2016),line=0,cex.axis=1)
mtext("Year",1,line=1.65,cex=1.3)
mtext(expression(paste("Legacy effect (g  ",m^-2," ",yr^-1,")",sep=" ")),2,2,adj=10,cex=1.3,las=0)
mtext(expression(paste("Legacy effect (g  ",m^-2," ",yr^-1,")",sep=" ")),2,2,las=0,adj=8,outer=F,cex=1)
dev.off()

png("figures/Fig4_positive_negative_prgl_legacy_V1.png",res = 350, pointsize = 12,height=5,width=6,units= "in")
par(las=1,lwd=1,cex.lab=1,mgp=c(11.75,0.35,0),oma=c(3.5,4.5,0.5,.5),tck=-0.02,mfrow=c(2,1),family="sans",xpd=T,lwd=2)

for (i in 1:5){
  df<-subset(prgl_pos_leg,prgl_pos_leg$treat == treatments[i])
  if (i==1) {#tiff("figures/lag_prgl_2year_ANPP.tiff",res = 250, pointsize = 12,height=3,width=4.5,units= "in")
    par(las=1,lwd=2,cex.axis=1,cex.lab=1.35,mgp=c(1.75,0.35,0),mar=c(0,0,0.5,0.5),tck=-0.015)
    plot(df$year,df$lag_prgl,type="l",ylim=c(0,60),xlim=c(2009,2016),col=colo[i],ylab="",xlab="",xaxt="n",bty="l",lab=c(5,7,7),yaxs="i")
    for(x in 1 : length(prgl_pos_leg$year))
      {arrows(prgl_pos_leg$year[x],prgl_pos_leg$lag_prgl[x]-se_prgl_pos_leg$lag_prgl[x],
              prgl_pos_leg$year[x],prgl_pos_leg$lag_prgl[x]+se_prgl_pos_leg$lag_prgl[x],code=3, angle=90,length=.05,lwd=1)}
      points(df$year,df$lag_prgl,pch=16,col=colo[i],cex=1.25)
  }
  else{lines(df$year,df$lag_prgl,col=colo[i],lty=if(i>=4){2})
    points(df$year,df$lag_prgl,pch=16,col=colo[i],cex=1.25)
  }
}
mtext("Shrub ANPP",3,line=-0.25)
for (i in 1:5){
  df<-subset(prgl_neg_leg,prgl_neg_leg$treat == treatments[i])
  if (i==1) {#tiff("figures/lag_prgl_2year_ANPP.tiff",res = 250, pointsize = 12,height=3,width=4.5,units= "in")
    par(las=1,lwd=2,cex.axis=1,cex.lab=1.5,mgp=c(1.75,0.35,0),mar=c(0,0,0,0.5),tck=-0.015)
    plot(df$year,df$lag_prgl,type="l",ylim=c(-60,0),xlim=c(2009,2016),col=colo[i],ylab="",xlab="",xaxt="n",bty="l",lab=c(5,7,7),yaxs="i")
    for(x in 1 : length(prgl_neg_leg$year))
      {arrows(prgl_neg_leg$year[x],prgl_neg_leg$lag_prgl[x]-se_prgl_neg_leg$lag_prgl[x],
              prgl_neg_leg$year[x],prgl_neg_leg$lag_prgl[x]+se_prgl_neg_leg$lag_prgl[x],code=3, angle=90,length=.05,lwd=1)}
      points(df$year,df$lag_prgl,pch=16,col=colo[i],cex=1.25)
  }
  else{lines(df$year,df$lag_prgl,col=colo[i],lty=if(i>=4){2})
    points(df$year,df$lag_prgl,pch=16,col=colo[i],cex=1.25)
  }
}
axis(side=1,at=c(2009:2016),labels=c(2009:2016),line=0,cex.axis=1)
mtext("Year",1,line=1.65,cex=1.3)
mtext(expression(paste("Legacy effect (g  ",m^-2," ",yr^-1,")",sep=" ")),2,2,adj=10,cex=1.3,las=0)
mtext(expression(paste("Legacy effect (g  ",m^-2," ",yr^-1,")",sep=" ")),2,2,las=0,adj=8,outer=F,cex=1)
dev.off()


# Autocorrelation plot ----------------------------------------------------

# CHECK ON THIS!
AC_dt<-ANPP[,c(1,2,33,3,5,12,11,31)]
AC<-data.frame()
for (i in 1:50){
  # i=1
temp<-subset(AC_dt,AC_dt$plot==i)
temp$AC_total<-NA
temp$AC_Pgrass<-NA
temp$AC_prgl<-NA
temp$AC_rare<-NA
temp$period<-NA
  for (yr in 1:7){
    # yr=1
  temp$AC_total[yr+1]<-acf(temp$total[c(yr:(yr+1))],lag.max = 1,plot=F)[[1]][2]
  temp$AC_Pgrass[yr+1]<-acf(temp$Pgrass[c(yr:(yr+1))],lag.max = 1,plot=F)[[1]][2]
  temp$AC_prgl[yr+1]<-acf(temp$prgl[c(yr:(yr+1))],lag.max = 1,plot=F)[[1]][2]
  temp$AC_rare[yr+1]<-acf(temp$rare[c(yr:(yr+1))],lag.max = 1,plot=F)[[1]][2]
  temp$period[yr+1]<-yr
  }
AC<-rbind(AC,temp)
}

mean_AC<-aggregate(AC[,-3],by=list(AC$period,AC$treat),FUN=mean,na.rm=T)
mean_AC <- rename(mean_AC, c(Group.1="period",Group.2="treat"))

seAC<-aggregate(AC[,-3],by=list(AC$period,AC$treat),FUN=sd,na.rm=T)
seAC[,10:13]<-seAC[,10:13]/sqrt(10)
seAC <- rename(seAC, c(Group.1="period",Group.2="treat"))

png("figures/Fig5_Autocorrelation_V1.png",res = 350, pointsize = 12,height=5,width=6,units= "in")
par(las=1,lwd=1,cex.lab=1,mgp=c(11.75,0.35,0),mar=c(0,0.25,0,0.25),oma=c(3.5,4.5,0.5,.5),tck=-0.02,mfrow=c(2,1),family="sans",xpd=T,lwd=2)

plot(AC_total~period,data=mean_AC[mean_AC$treat=="ambient",],cex=1.5,type="n",lwd=2,ylim=c(-0.8,0),xaxt="n")
# abline(lm(AC_total~period,data=mean_AC[mean_AC$treat=="ambient",]))
for(i in 1:3){
  # i=4
  lines(AC_total~period,data=mean_AC[mean_AC$treat==treatments[i],],col=colo[i],lwd=2)
  points(AC_total~period,data=mean_AC[mean_AC$treat==treatments[i],],pch=16,col=colo[i],cex=1.5)
# abline(lm(AC_total~period,data=mean_AC[mean_AC$treat==treatments[i],]),col=i)
}

plot(AC_prgl~period,data=mean_AC[mean_AC$treat=="ambient",],cex=1.5,type="n",lwd=2,ylim=c(-0.8,0))
# abline(lm(AC_prgl~period,data=mean_AC[mean_AC$treat=="ambient",]))
for(i in 1:5){
  # i=4
  lines(AC_prgl~period,data=mean_AC[mean_AC$treat==treatments[i],],col=colo[i],lwd=2)
  points(AC_prgl~period,data=mean_AC[mean_AC$treat==treatments[i],],pch=16,col=colo[i],cex=1.5)
  # abline(lm(AC_total~period,data=mean_AC[mean_AC$treat==treatments[i],]),col=i)
}
axis(side=1,at=c(1:6),labels=c(1:6),line=0,cex.axis=1)
mtext("Period",1,line=1.65,cex=1.3)
mtext(expression(paste("Autocorrelation")),2,2.5,adj=2,cex=1.3,las=0)
dev.off()




#  ------------------------------------------------------------------------


aa<-subset(ANPP,ANPP$pptCV==treats[4]&ANPP$year==2014)


plot(total~year,data=lag_dt[lag_dt$treat=="+80%inc",])
plot(Pgrass~year,data=lag_dt[lag_dt$treat=="-80%inc",])
# Total ANPP statistical analyses -----------------------------------------

# Repeated Measures ANOVA

# With precipitation as covariate
aov1<-aov(total~ppt+treat*year+Error(plot),data=ANPP)
summary(aov1)

#First three years
aov1<-aov(total~treat*year+Error(plot),data=ANPP[ANPP$year<2012,])
summary(aov1)
#Last three years
aov1<-aov(total~treat*year+Error(plot),data=ANPP[ANPP$year>2011,])
summary(aov1)


# Perennial Grass ANPP statistical analyses -----------------------------------------

# Repeated Measures ANOVA
aov1<-aov(Pgrass~treat*year+Error(plot),data=ANPP[ANPP$year<2014,])
summary(aov1)

# Shrub ANPP statistical analyses -----------------------------------------

# Repeated Measures ANOVA
aov1<-aov(prgl~treat*year+Error(plot),data=ANPP[ANPP$year<2014,])
summary(aov1)

# Rare species ANPP statistical analyses -----------------------------------------

# Repeated Measures ANOVA
aov1<-aov(rare~treat*year+Error(plot),data=ANPP)
summary(aov1)


