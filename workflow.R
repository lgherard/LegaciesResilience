source("data_cleanning.R")

# ANPP data for figures ------------------------------------------------------
mean_treat_ANPP<-aggregate(ANPP,by=list(ANPP$year,ANPP$treat),FUN=mean,na.rm=T)
mean_treat_ANPP<-mean_treat_ANPP[,-39]
mean_treat_ANPP <- rename(mean_treat_ANPP, c(Group.1="year",Group.2="treat"))

seANPP<-aggregate(ANPP,by=list(ANPP$year,ANPP$treat),FUN=sd,na.rm=T)
seANPP<-seANPP[,-c(3:4,10:12,15:40)]
seANPP[,c(3:13)]<-seANPP[,3:13]/sqrt(10)
seANPP <- rename(seANPP, c(Group.1="year",Group.2="treat"))

treatments<-treat
#           blue         red       
colo<-c(1,"#377EB8",   "#E41A1C")
# colo<-c("#377EB8","#E41A1C",1)

# Figure 1 ----------------------------------------------------------------
png("C:/Users/lgherar1.ASURITE/Dropbox (ASU)/lgherar1/desktop/current/Papers/Legacy over time/figures/Fig1_A_B_V2.png",res = 350, pointsize = 22,height=12,width=7.5,units= "in")
par(las=1,lwd=1,cex.lab=1,mgp=c(11.75,0.35,0),oma=c(3,3,.5,.5),tck=-0.02,mfrow=c(4,1),family="sans",xpd=NA,lwd=2)
# total ANPP
dis<-c(0,0,0,0,0,1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5,6,6,6,6,6)/20
dist<-c(0,1,2,3,4,5)/2000
d=50
for (i in 1:5){
  df<-subset(mean_treat_ANPP,mean_treat_ANPP$treat == treatments[i])
  if (i==1) {#tiff("figures/total_2year_ANPP.tiff",res = 250, pointsize = 12,height=3,width=4.5,units= "in")
    par(las=1,lwd=2,cex.axis=1,cex.lab=0.95,mgp=c(111.5,0.35,0),mar=c(0.75,1.5,1.5,0),tck=-0.015)
    plot(df$year,df$total,type="l",ylim=c(0,250),xlim=c(2009,2018),col=colo[i],ylab="" ,xlab="year period (years)",xaxt="n",bty="l",lab=c(4,4,7))
    for(x in 1 : length(mean_treat_ANPP$year))
    {arrows(mean_treat_ANPP$year[x]+dis[x]/d,mean_treat_ANPP$total[x]-seANPP$total[x],
            mean_treat_ANPP$year[x]+dis[x]/d,mean_treat_ANPP$total[x]+seANPP$total[x],code=3, angle=90,length=.05,lwd=1)}
    points(df$year+dist[i]/d,df$total,pch=16,col=colo[i],cex=1.25)
  }
  else{lines(df$year+dist[i]/d,df$total,col=colo[i],lty=if(i>=4){2})
    points(df$year+dist[i]/d,df$total,pch=16,col=colo[i],cex=1.25)
  }
}

mtext("Total",3,line=0.5)
text(2009,250,"a)",cex=1.25,font=2)

for (i in 1:3){ 
  # i=1
  df<-subset(mean_treat_ANPP,mean_treat_ANPP$treat == treatments[i])
  if (i==1) {#tiff("figures/Pgrass_2year_ANPP.tiff",res = 250, pointsize = 12,height=3,width=4.5,units= "in")
    plot(df$year,df$Pgrass,type="l",ylim=c(0,200),xlim=c(2009,2018),col=colo[i],ylab="" ,xlab="year period (years)",xaxt="n",bty="l",lab=c(5,4,7))
    for(x in 1 : length(mean_treat_ANPP$year))
    {arrows(mean_treat_ANPP$year[x],mean_treat_ANPP$Pgrass[x]-seANPP$Pgrass[x],
            mean_treat_ANPP$year[x],mean_treat_ANPP$Pgrass[x]+seANPP$Pgrass[x],code=3, angle=90,length=.05,lwd=1)}
    points(df$year,df$Pgrass,pch=16,col=colo[i],cex=1.25)
  } 
  else{lines(df$year+dist[i]/d,df$Pgrass,col=colo[i],lty=if(i>=4){2})
    points(df$year+dist[i]/d,df$Pgrass,pch=16,col=colo[i],cex=1.25)
  }
}

mtext("Perennial grass species",3,line=0.5)
text(2009,200,"b)",cex=1.25,font=2)

for (i in 1:5){ 
  df<-subset(mean_treat_ANPP,mean_treat_ANPP$treat == treatments[i])
  if (i==1) {#tiff("figures/prgl_2year_ANPP.tiff",res = 250, pointsize = 12,height=3,width=4.5,units= "in")
    plot(df$year,df$prgl,type="l",ylim=c(0,160),xlim=c(2009,2018),col=colo[i],ylab="" ,xlab="year period (years)",xaxt="n",bty="l",lab=c(5,4,7))
    for(x in 1 : length(mean_treat_ANPP$year))
    {arrows(mean_treat_ANPP$year[x],mean_treat_ANPP$prgl[x]-seANPP$prgl[x],
            mean_treat_ANPP$year[x],mean_treat_ANPP$prgl[x]+seANPP$prgl[x],code=3, angle=90,length=.05,lwd=1)}
    points(df$year,df$prgl,pch=16,col=colo[i],cex=1.25)
  } 
  else{lines(df$year+dist[i]/d,df$prgl,col=colo[i],lty=if(i>=4){2})
    points(df$year+dist[i]/d,df$prgl,pch=16,col=colo[i],cex=1.25)
  }
}

mtext("Shrub species",3,line=0.5)
text(2009,160,"c)",cex=1.25,font=2)

for (i in 1:5){ 
  df<-subset(mean_treat_ANPP,mean_treat_ANPP$treat == treatments[i])
  if (i==1) {#tiff("figures/rare_2year_ANPP.tiff",res = 250, pointsize = 12,height=3,width=4.5,units= "in")
    plot(df$year,df$rare,type="l",ylim=c(0,80),xlim=c(2009,2018),col=colo[i],ylab="" ,xlab="year period (years)",xaxt="n",bty="l",lab=c(5,4,7))
    for(x in 1 : length(mean_treat_ANPP$year))
    {arrows(mean_treat_ANPP$year[x],mean_treat_ANPP$rare[x]-seANPP$rare[x],
            mean_treat_ANPP$year[x],mean_treat_ANPP$rare[x]+seANPP$rare[x],code=3, angle=90,length=.05,lwd=1)}
    points(df$year,df$rare,pch=16,col=colo[i],cex=1.25)
  } 
  else{lines(df$year+dist[i]/d,df$rare,col=colo[i],lty=if(i>=4){2})
    points(df$year+dist[i]/d,df$rare,pch=16,col=colo[i],cex=1.25)
  }
}

mtext("Rare species",3,line=0.5)
text(2009,80,"d)",cex=1.25,font=2)

# legend(2009,80,col=colo,
#        legend=c("ambient variability","increased variability +/-50% CV",
#                 "increased variability +/-80% CV")
#        ,lwd=2,lty=c(1,1,1),cex=0.9,bty="n",seg.len=4)
axis(side=1,at=c(2009:2018),labels=c(2009:2018),line=0,cex.axis=1)

mtext("Year",1,line=2.25,cex=1.35)

mtext(expression(paste("ANPP (g  ",m^-2," ",yr^-1,")",sep=" ")),2,las=0,outer=T,cex=1.35,line=0.5)

dev.off()

# Figure 2 ----------------------------------------------------------------
png("C:/Users/lgherar1.ASURITE/Dropbox (ASU)/lgherar1/desktop/current/Papers/Legacy over time/figures/Fig2_V2.png",res = 350, pointsize = 22,height=11,width=7.5,units= "in")
par(las=1,lwd=1,cex.lab=1,mgp=c(11.75,0.35,0),oma=c(3,3,0.5,.5),tck=-0.02,mfrow=c(3,1),family="sans",xpd=NA,lwd=2)

for (i in 1:5){
  df<-subset(mean_treat_ANPP,mean_treat_ANPP$treat == treatments[i])
  if (i==1) {#tiff("figures/total_WUE_2year_ANPP.tiff",res = 250, pointsize = 12,height=3,width=4.5,units= "in")
    par(las=1,lwd=2,cex.axis=1,cex.lab=0.95,mgp=c(111.5,0.35,0),mar=c(0.75,1.5,0.5,0),tck=-0.015)
    plot(df$year,df$total_WUE,type="l",ylim=c(0,3),xlim=c(2009,2018),col=colo[i],ylab="" ,xlab="year period (years)",xaxt="n",bty="l",lab=c(5,7,7))
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
    plot(df$year,df$Pgrass_WUE,type="l",ylim=c(0,2.5),xlim=c(2009,2018),col=colo[i],ylab="" ,xlab="year period (years)",xaxt="n",bty="l",lab=c(5,7,7))
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
    plot(df$year,df$prgl_WUE,type="l",ylim=c(0,2.5),xlim=c(2009,2018),col=colo[i],ylab="" ,xlab="year period (years)",xaxt="n",bty="l",lab=c(5,4,7))
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
text(2009,2.5,"c)",cex=1.25,font=2)
axis(side=1,at=c(2009:2018),labels=c(2009:2018),line=0,cex.axis=1)

mtext("Year",1,line=2.25,cex=1.35)

mtext(expression(paste("WUE (g  ",m^-2," ",mm^-1," ",yr^-1,")",sep=" ")),2,las=0,outer=T,cex=1.35,line=0.5)
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

lagdf<-ANPP[,c(1,3,5,12,11,33,35,36,37,42:46)]
lagdf$lag_total<-lagdf$total - lagdf$expANPP_total
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
png("figures/Fig3_V2.png",res = 350, pointsize = 12,height=8,width=6,units= "in")
par(las=1,lwd=1,cex.lab=1,mgp=c(11.75,0.35,0),oma=c(3,3,0.5,.5),tck=-0.02,mfrow=c(2,1),family="sans",xpd=NA,lwd=2)

for (i in 1:3){
  df<-subset(abs_leg,abs_leg$treat == treatments[i])
  if (i==1) {#tiff("figures/lag_Pgrass_2year_ANPP.tiff",res = 250, pointsize = 12,height=3,width=4.5,units= "in")
    par(las=1,lwd=2,cex.axis=1,cex.lab=0.95,mgp=c(111.5,0.35,0),mar=c(0.75,1.5,0.5,0),tck=-0.015)
    plot(df$year,df$lag_Pgrass,type="l",ylim=c(0,150),xlim=c(2009,2018),col=colo[i],ylab="" ,xlab="year period (years)",xaxt="n",bty="l",lab=c(5,7,7))
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
    plot(df$year,df$lag_prgl,type="l",ylim=c(0,155),xlim=c(2009,2018),col=colo[i],ylab="" ,xlab="year period (years)",xaxt="n",bty="l",lab=c(5,7,7))
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
text(2009,155,"b)",cex=1.25,font=2)
axis(side=1,at=c(2009:2018),labels=c(2009:2018),line=0,cex.axis=1)

mtext("Year",1,line=2.25,cex=1.35)

mtext(expression(paste("Absolute Legacy effect (g  ",m^-2," ",yr^-1,")",sep=" ")),2,las=0,outer=T,cex=1.35,line=0.5)
dev.off()


# Absolute legacy total ---------------------------------------------------
png("C:/Users/lgherar1.ASURITE/Dropbox (ASU)/lgherar1/desktop/current/Papers/Legacy over time/figures/Fig3_total_V2.png",res = 350, pointsize = 12,height=6,width=5,units= "in")
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
    plot(df$year,df$lag_total,type="l",ylim=c(0,150),xlim=c(2009,2018),col=colo[i],ylab=expression(paste("Absolute legacy effect (g  ",m^-2," ",yr^-1,")",sep=" ")),xlab="",xaxt="n",bty="l",lab=c(5,5,7))
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
axis(side=1,at=c(2009:2018),labels=c(2009:2018),line=0,cex.axis=1)
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

png("figures/Fig4_positive_negative_legacy_V2.png",res = 350, pointsize = 12,height=5,width=6,units= "in")
par(las=1,lwd=1,cex.lab=1,mgp=c(11.75,0.35,0),oma=c(3.5,4.5,0.5,.5),tck=-0.02,mfrow=c(2,1),family="sans",xpd=T,lwd=2)

for (i in 1:5){
  df<-subset(total_pos_leg,total_pos_leg$treat == treatments[i])
  if (i==1) {#tiff("figures/lag_total_2year_ANPP.tiff",res = 250, pointsize = 12,height=3,width=4.5,units= "in")
    par(las=1,lwd=2,cex.axis=1,cex.lab=1.35,mgp=c(1.75,0.35,0),mar=c(0,0,0.5,0.5),tck=-0.015)
    plot(df$year,df$lag_total,type="l",ylim=c(0,140),xlim=c(2009,2018),col=colo[i],ylab="",xlab="",xaxt="n",bty="l",lab=c(5,7,7),yaxs="i")
    
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
    plot(df$year,df$lag_total,type="l",ylim=c(-150,0),xlim=c(2009,2018),col=colo[i],ylab="",xlab="",xaxt="n",bty="l",lab=c(5,7,7),yaxs="i")
    for(x in 1 : length(total_neg_leg$year))
      {arrows(total_neg_leg$year[x],total_neg_leg$lag_total[x]-se_total_neg_leg$lag_total[x],
              total_neg_leg$year[x],total_neg_leg$lag_total[x]+se_total_neg_leg$lag_total[x],code=3, angle=90,length=.05,lwd=1)}
      points(df$year,df$lag_total,pch=16,col=colo[i],cex=1.25)
  }
  else{lines(df$year,df$lag_total,col=colo[i],lty=if(i>=4){2})
    points(df$year,df$lag_total,pch=16,col=colo[i],cex=1.25)
  }
}
axis(side=1,at=c(2009:2018),labels=c(2009:2018),line=0,cex.axis=1)
mtext("Year",1,line=1.65,cex=1.3)
mtext(expression(paste("Legacy effect (g  ",m^-2," ",yr^-1,")",sep=" ")),2,2,adj=10,cex=1.3,las=0)
mtext(expression(paste("Legacy effect (g  ",m^-2," ",yr^-1,")",sep=" ")),2,2,las=0,adj=8,outer=F,cex=1)
dev.off()

png("figures/Fig4_positive_negative_Pgrass_legacy_V2.png",res = 350, pointsize = 12,height=5,width=6,units= "in")
par(las=1,lwd=1,cex.lab=1,mgp=c(11.75,0.35,0),oma=c(3.5,4.5,0.5,.5),tck=-0.02,mfrow=c(2,1),family="sans",xpd=T,lwd=2)

for (i in 1:5){
  df<-subset(Pgrass_pos_leg,Pgrass_pos_leg$treat == treatments[i])
  if (i==1) {#tiff("figures/lag_Pgrass_2year_ANPP.tiff",res = 250, pointsize = 12,height=3,width=4.5,units= "in")
    par(las=1,lwd=2,cex.axis=1,cex.lab=1.35,mgp=c(1.75,0.35,0),mar=c(0,0,0.5,0.5),tck=-0.015)
    plot(df$year,df$lag_Pgrass,type="l",ylim=c(0,140),xlim=c(2009,2018),col=colo[i],ylab="",xlab="",xaxt="n",bty="l",lab=c(5,7,7),yaxs="i")
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
    plot(df$year,df$lag_Pgrass,type="l",ylim=c(-140,0),xlim=c(2009,2018),col=colo[i],ylab="",xlab="",xaxt="n",bty="l",lab=c(5,7,7),yaxs="i")
    for(x in 1 : length(Pgrass_neg_leg$year))
      {arrows(Pgrass_neg_leg$year[x],Pgrass_neg_leg$lag_Pgrass[x]-se_Pgrass_neg_leg$lag_Pgrass[x],
              Pgrass_neg_leg$year[x],Pgrass_neg_leg$lag_Pgrass[x]+se_Pgrass_neg_leg$lag_Pgrass[x],code=3, angle=90,length=.05,lwd=1)}
      points(df$year,df$lag_Pgrass,pch=16,col=colo[i],cex=1.25)
  }
  else{lines(df$year,df$lag_Pgrass,col=colo[i],lty=if(i>=4){2})
    points(df$year,df$lag_Pgrass,pch=16,col=colo[i],cex=1.25)
  }
}
axis(side=1,at=c(2009:2018),labels=c(2009:2018),line=0,cex.axis=1)
mtext("Year",1,line=1.65,cex=1.3)
mtext(expression(paste("Legacy effect (g  ",m^-2," ",yr^-1,")",sep=" ")),2,2,adj=10,cex=1.3,las=0)
mtext(expression(paste("Legacy effect (g  ",m^-2," ",yr^-1,")",sep=" ")),2,2,las=0,adj=8,outer=F,cex=1)
dev.off()

png("figures/Fig4_positive_negative_prgl_legacy_V2.png",res = 350, pointsize = 12,height=5,width=6,units= "in")
par(las=1,lwd=1,cex.lab=1,mgp=c(11.75,0.35,0),oma=c(3.5,4.5,0.5,.5),tck=-0.02,mfrow=c(2,1),family="sans",xpd=T,lwd=2)

for (i in 1:5){
  df<-subset(prgl_pos_leg,prgl_pos_leg$treat == treatments[i])
  if (i==1) {#tiff("figures/lag_prgl_2year_ANPP.tiff",res = 250, pointsize = 12,height=3,width=4.5,units= "in")
    par(las=1,lwd=2,cex.axis=1,cex.lab=1.35,mgp=c(1.75,0.35,0),mar=c(0,0,0.5,0.5),tck=-0.015)
    plot(df$year,df$lag_prgl,type="l",ylim=c(0,140),xlim=c(2009,2018),col=colo[i],ylab="",xlab="",xaxt="n",bty="l",lab=c(5,7,7),yaxs="i")
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
    plot(df$year,df$lag_prgl,type="l",ylim=c(-140,0),xlim=c(2009,2018),col=colo[i],ylab="",xlab="",xaxt="n",bty="l",lab=c(5,7,7),yaxs="i")
    for(x in 1 : length(prgl_neg_leg$year))
      {arrows(prgl_neg_leg$year[x],prgl_neg_leg$lag_prgl[x]-se_prgl_neg_leg$lag_prgl[x],
              prgl_neg_leg$year[x],prgl_neg_leg$lag_prgl[x]+se_prgl_neg_leg$lag_prgl[x],code=3, angle=90,length=.05,lwd=1)}
      points(df$year,df$lag_prgl,pch=16,col=colo[i],cex=1.25)
  }
  else{lines(df$year,df$lag_prgl,col=colo[i],lty=if(i>=4){2})
    points(df$year,df$lag_prgl,pch=16,col=colo[i],cex=1.25)
  }
}
axis(side=1,at=c(2009:2018),labels=c(2009:2018),line=0,cex.axis=1)
mtext("Year",1,line=1.65,cex=1.3)
mtext(expression(paste("Legacy effect (g  ",m^-2," ",yr^-1,")",sep=" ")),2,2,adj=10,cex=1.3,las=0)
mtext(expression(paste("Legacy effect (g  ",m^-2," ",yr^-1,")",sep=" ")),2,2,las=0,adj=8,outer=F,cex=1)
dev.off()


# Autocorrelation plot ----------------------------------------------------

treats<-sort(unique(ANPP$pptCV))
treatments<-c("ambient","-/+50%","+/-50%","-/+80%","+/-80%")

ANPP$treatment<-ifelse(ANPP$pptCV==treats[1],treatments[1],NA)
ANPP$treatment<-ifelse(ANPP$pptCV==treats[2],treatments[2],ANPP$treatment)
ANPP$treatment<-ifelse(ANPP$pptCV==treats[3],treatments[3],ANPP$treatment)
ANPP$treatment<-ifelse(ANPP$pptCV==treats[4],treatments[4],ANPP$treatment)
ANPP$treatment<-ifelse(ANPP$pptCV==treats[5],treatments[5],ANPP$treatment)

AR_dt<-ANPP[,c(1,2,3,5,12,11,43)]
AR1<-melt(AR_dt,id.vars = c("year","plot","treatment"))
AR1<-cast(AR1,plot+treatment+variable~year)
AR1<-AR1[AR1$variable!="rare",]
AR1result<-data.frame()
for (i in unique(AR1$variable)){
  
  temp0<-AR1[AR1$variable==i,]
  
    for(tr in unique(AR1$treatment)){
      
      temp<-temp0[temp0$treatment==tr,]
      
        for(col in c(4:12)){
  
           # AC<-summary(lm(temp[,col]~ temp[,col+1]))$coeff[2,1]
           AC<-cor(temp[,col], temp[,col+1])
           AC_SE<-summary(lm(temp[,col]~temp[,col+1]))$coeff[2,2]
           SD<-sd(temp[,col])
           temp1<-data.frame(treatment=tr,variable=i,year=2006+col,AC=AC,AC_SE=AC_SE,SD=SD)

           AR1result<-rbind(AR1result,temp1)           
        }
    }
  # AR1_total<-summary(lm(temp[temp$treat=="80%inc",12]~temp[temp$treat=="80%inc",11]))$coeff[2,1]
}
colo<-c(1,"#377EB8","#377EB8",   "#E41A1C",   "#E41A1C")

png("figures/Fig5_SD_V2.png",res = 350, pointsize = 12,height=5,width=6,units= "in")
par(las=1,lwd=1,cex.lab=1.5,mgp=c(2.75,.75,0),mar=c(4,4.25,0.5,0.5),tck=-0.01,xpd=F)

total<-AR1result[which(AR1result$variable=="total"),]
plot(SD~year,data=total,cex=1.5,type="n",lwd=2,ylim=c(0,100),xlab="Year",ylab="Standard Deviation among plots")
for(i in 1:5){
  # i=4
  lines(SD~year,data=total[total$treatment==treatments[i],],col=colo[i],lwd=2)
  points(SD~year,data=total[total$treatment==treatments[i],],pch=16,col=colo[i],cex=1.5)
  # abline(lm(SD_total~period,data=mean_SD[mean_SD$treat==treatments[i],]),col=i)
}
dev.off()


png("figures/Fig5_AC_V2.png",res = 350, pointsize = 12,height=5,width=6,units= "in")
par(las=1,lwd=1,cex.lab=1.5,mgp=c(2.75,.75,0),mar=c(4,4.25,0.5,0.5),tck=-0.01,xpd=F)

total<-AR1result[which(AR1result$variable=="total"),]
plot(AC~year,data=total,cex=1.5,type="n",lwd=2,xlab="Year",ylab="Autocorrelation Lag 1 year")
for(i in 1:5){
  # i=4
  lines(AC~year,data=total[total$treatment==treatments[i],],col=colo[i],lwd=2)
  points(AC~year,data=total[total$treatment==treatments[i],],pch=16,col=colo[i],cex=1.5)
  # abline(lm(AC_total~period,data=mean_AC[mean_AC$treat==treatments[i],]),col=i)
}
dev.off()


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


