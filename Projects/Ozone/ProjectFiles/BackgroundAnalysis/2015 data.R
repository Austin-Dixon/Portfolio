data<-read.csv("Ozone Data 2015.csv")
#Initial elimation of extreme outliers and modification of dates for more cohesive data
testingdata<-data[(data$Temp<200)&(data$RH<101)&(data$Ozone<1000 & data$Ozone>=0)&(data$WindSpeed<=100)&(data$WindDir<=360),]
testingdata$Rdate=as.POSIXct(testingdata$InstrumentDT,format="%m/%d/%Y %H:%M:%S")
testingdata$HTdif=testingdata$RH - testingdata$Temp #Relative humidity to Temperature difference
testingdata$VPD<-(1-testingdata$RH/100)*(6.11*10^((7.5*(testingdata$Temp-32)*(5/9))/(237.3+(testingdata$Temp-32)*(5/9))))#Vapor Pressure deficit
correlations<-cor(testingdata[c(1:7,18,19)])

#separation of data by location and averaging information to the nearest hour.
GrantO<-testingdata[testingdata$Latitude==38.325623,] #O for original (and saving time from replacements)
Grant<-aggregate(GrantO,list(cut(GrantO$Rdate,"1 hour")),mean) #condenses data to hourly averages 
Grant$Rdate<-as.POSIXct(Grant$Group.1)
GCorr<-cor(GrantO[c(1:7,18,19)])
Grant$Hour<- as.numeric(substr(Grant$Rdate, 12, 13))
GrantDL<-subset(Grant, Grant$Hour < 18 &
                Grant$Hour >9)

PlanetariumO<-testingdata[testingdata$Latitude==38.632801,]
Planetarium<-aggregate(PlanetariumO,list(cut(PlanetariumO$Rdate,"1 hour")),mean)
Planetarium$Rdate<-as.POSIXct(Planetarium$Group.1)
PCorr<-cor(PlanetariumO[c(1:7,18,19)])
Planetarium$Hour<- as.numeric(substr(Planetarium$Rdate, 12, 13))
PlanetDL<-subset(Planetarium, Planetarium$Hour < 18 &
                        Planetarium$Hour >9)

SWICO<-testingdata[testingdata$Latitude==38.310413,]
SWIC<-aggregate(SWICO,list(cut(SWICO$Rdate,"1 hour")),mean)
SWIC$Rdate<-as.POSIXct(SWIC$Group.1)
SCorr<-cor(SWICO[c(1:7,18,19)])
SWIC$Hour<- as.numeric(substr(SWIC$Rdate, 12, 13))
SWICDL<-subset(SWIC, SWIC$Hour < 18 &
                        SWIC$Hour >9)

#C=common; Entries where all locations share common instrument recording times
GrantC<-Grant[(Grant$Group.1 %in% SWIC$Group.1) & (Grant$Group.1 %in% Planetarium$Group.1),]
PlanetC<-Planetarium[(Planetarium$Group.1 %in% SWIC$Group.1) & (Planetarium$Group.1 %in% Grant$Group.1),]
SWICC<-SWIC[(SWIC$Group.1 %in% Grant$Group.1) & (SWIC$Group.1 %in% Planetarium$Group.1),]

#Quantitative variables used for later testing purposes; Rainfall is currently not used for
#the sake of graphic quality and lack of significance in correlation with the other variables
testvariables<-c('Ozone','Temp','Press','RH','WindDir','WindSpeed')

##########################################
#Individual plotting
starttime=as.POSIXct('2015-04-01 01:00:00')
endtime=as.POSIXct('2015-10-01 01:00:00')
GrantFrame=Grant[Grant$Rdate>starttime & Grant$Rdate<endtime,'Rdate']
SWICFrame=SWIC[SWIC$Rdate>starttime & SWIC$Rdate<endtime,'Rdate']
PlanetFrame=Planetarium[Planetarium$Rdate>starttime & Planetarium$Rdate<endtime,'Rdate']

par(mfrow=c(2,3))
for (stat in testvariables){
GrantStat=Grant[Grant$Rdate>starttime & Grant$Rdate<endtime,stat]
SWICStat=SWIC[SWIC$Rdate>starttime & SWIC$Rdate<endtime,stat]
PlanetStat=Planetarium[Planetarium$Rdate>starttime & Planetarium$Rdate<endtime,stat]
col_code=c('green','red','blue')[c(rep(1,length(GrantStat)),rep(2,length(SWICStat)),rep(3,length(PlanetStat)))]
plot(c(GrantStat,SWICStat,PlanetStat)~c(GrantFrame,SWICFrame,PlanetFrame),
     type="p",pch = 'o', col= col_code,
     xlab="Date",ylab=stat,main=c(stat,"2015"))
#legend("bottomright","(x,y)",legend=c('Grant','SWIC','Planetarium'),col=(c('green','red','blue')),
#       pch=20,cex=.25,pt.cex = 1.5)
}

###########################################
#Paired plotting
starttime=as.POSIXct('2015-04-01 01:00:00')
endtime=as.POSIXct('2015-10-01 01:00:00')
GrantFrame=GrantC[GrantC$Rdate>starttime & GrantC$Rdate<endtime,'Rdate']
SWICFrame=SWICC[SWICC$Rdate>starttime & SWICC$Rdate<endtime,'Rdate']
PlanetFrame=PlanetC[PlanetC$Rdate>starttime & PlanetC$Rdate<endtime,'Rdate']

par(mfrow=c(2,3))
for (stat in testvariables){
        GrantStat=GrantC[GrantC$Rdate>starttime & GrantC$Rdate<endtime,stat]
        SWICStat=SWICC[SWICC$Rdate>starttime & SWICC$Rdate<endtime,stat]
        PlanetStat=PlanetC[PlanetC$Rdate>starttime & PlanetC$Rdate<endtime,stat]
        col_code=c('green','red','blue')[c(rep(1,length(GrantStat)),rep(2,length(SWICStat)),rep(3,length(PlanetStat)))]
        plot(c(GrantStat,SWICStat,PlanetStat)~c(GrantFrame,SWICFrame,PlanetFrame),
             type="p",pch = 'o', col= col_code,
             xlab="Date",ylab=stat,main=c(stat,"2015"))
        #legend("bottomright","(x,y)",legend=c('Grant','SWIC','Planetarium'),col=(c('green','red','blue')),
        #       pch=20,cex=.25,pt.cex = 1.5)
}

#Attempt towards trying to fill in missing data
#1st: finding the differentials between location by attribute
#finding the means of each hour and subtracting from attribute for each location appears to
#output the smallest deviation

averages<-data.frame(Rdate=GrantC$Rdate,Ozone=(GrantC$Ozone+SWICC$Ozone+PlanetC$Ozone)/3)
averages$Temp<-(GrantC$Temp+SWICC$Temp+PlanetC$Temp)/3
averages$Press<-(GrantC$Press+SWICC$Press+PlanetC$Press)/3
averages$RH<-(GrantC$RH+SWICC$RH+PlanetC$RH)/3
averages$WindDir<-(GrantC$WindDir+SWICC$WindDir+PlanetC$WindDir)/3
averages$WindSpeed<-(GrantC$WindSpeed+SWICC$WindSpeed+PlanetC$WindSpeed)/3
averages$Rainfall<-(GrantC$Rainfall+SWICC$Rainfall+PlanetC$Rainfall)/3
averages$HTdif<-averages$RH - averages$Temp
averages$VPD<-(1-averages$RH/100)*(6.11*10^((7.5*(averages$Temp-32)*(5/9))/(237.3+(averages$Temp-32)*(5/9))))
ACorr<-cor(averages[2:10])

###########################################
#Paired plotting w/ averages
starttime=as.POSIXct('2015-06-07 01:00:00')
endtime=as.POSIXct('2015-06-15 01:00:00')
GrantFrame=GrantC[GrantC$Rdate>starttime & GrantC$Rdate<endtime,'Rdate']
SWICFrame=SWICC[SWICC$Rdate>starttime & SWICC$Rdate<endtime,'Rdate']
PlanetFrame=PlanetC[PlanetC$Rdate>starttime & PlanetC$Rdate<endtime,'Rdate']
AvgFrame=averages[averages$Rdate>starttime & averages$Rdate<endtime,'Rdate']

par(mfrow=c(2,3))
for (stat in testvariables){
        GrantStat=GrantC[GrantC$Rdate>starttime & GrantC$Rdate<endtime,stat]
        SWICStat=SWICC[SWICC$Rdate>starttime & SWICC$Rdate<endtime,stat]
        PlanetStat=PlanetC[PlanetC$Rdate>starttime & PlanetC$Rdate<endtime,stat]
        AvgStat=averages[averages$Rdate>starttime & averages$Rdate<endtime,stat]
        col_code=c('green','red','blue','black')[c(rep(1,length(GrantStat)),rep(2,length(SWICStat)),rep(3,length(PlanetStat)),rep(4,length(AvgStat)))]
        plot(c(GrantStat,SWICStat,PlanetStat,AvgStat)~c(GrantFrame,SWICFrame,PlanetFrame,AvgFrame),
             type="p",pch = 'o', col= col_code,
             xlab="Date",ylab=stat,main=c(stat,"2015"))
        #legend("bottomright","(x,y)",legend=c('Grant','SWIC','Planetarium'),col=(c('green','red','blue')),
        #       pch=20,cex=.25,pt.cex = 1.5)
}

#2nd: Finding the average difference from the mean recording of each variable for each location
difmeans<-data.frame(lapply(GrantC[2:8]-averages[2:8],mean))
difmeans[2,]<-lapply(SWICC[2:8]-averages[2:8],mean)
difmeans[3,]<-lapply(PlanetC[2:8]-averages[2:8],mean)

#3rd: Fill in missing data using what we know from SWIC's recordings and what we've obtained
#as our differences
averagesP<-averages #P for projections
GrantP<-Grant
SWICP<-SWIC
PlanetP<-Planetarium
for(i in 1:dim(SWIC)[1]){   #i represents each row/entry in the dataset
        if(SWIC$Rdate[i] %in% averagesP$Rdate == FALSE){ #Checks if a dated entry exists in one set but not the other
                averagesP[dim(averagesP)[1]+1,1]=SWIC$Rdate[i] #creates a new entry to end of data with corresponding dates
                for(j in 2:8){ #j represents the selected columns/attributes in the dataset
                        averagesP[dim(averagesP)[1],j]=SWIC[i,j]-difmeans[2,j-1]}} #predicts values for each attribute based on given data and differentials
        
        if(SWIC$Rdate[i] %in% GrantP$Rdate == FALSE){
                GrantP[dim(GrantP)[1]+1,'Rdate']=SWIC$Rdate[i]
                for(j in 2:8){
                        GrantP[dim(GrantP)[1],j]=ifelse(SWIC$Rdate[i] %in% Planetarium$Rdate,
                                mean(c(SWIC[i,j],Planetarium[which(SWIC$Rdate[i]==Planetarium$Rdate),j],averagesP[which(SWIC$Rdate[i]==averagesP$Rdate),j]))+difmeans[1,j-1],
                                SWIC[i,j]-difmeans[2,j-1]+difmeans[1,j-1])}}
        
        if(SWIC$Rdate[i] %in% PlanetP$Rdate == FALSE){
                PlanetP[dim(PlanetP)[1]+1,'Rdate']=SWIC$Rdate[i]
                for(j in 2:8){
                        PlanetP[dim(PlanetP)[1],j]=ifelse(SWIC$Rdate[i] %in% Grant$Rdate,
                                mean(c(SWIC[i,j],Grant[which(SWIC$Rdate[i]==Grant$Rdate,averagesP[which(SWIC$Rdate[i]==averagesP$Rdate),j]),j]))+difmeans[3,j-1],
                                SWIC[i,j]-difmeans[2,j-1]+difmeans[3,j-1])}}}

for(i in 1:dim(Grant)[1]){
        if(Grant$Rdate[i] %in% averagesP$Rdate == FALSE){
                averagesP[dim(averagesP)[1]+1,1]=Grant$Rdate[i]
                for(j in 2:8){
                        averagesP[dim(averagesP)[1],j]=Grant[i,j]-difmeans[1,j-1]}}
        
        if(Grant$Rdate[i] %in% SWICP$Rdate == FALSE){
                SWICP[dim(SWICP)[1]+1,'Rdate']=Grant$Rdate[i]
                for(j in 2:8){
                        SWICP[dim(SWICP)[1],j]=ifelse(Grant$Rdate[i] %in% Planetarium$Rdate,
                                                      mean(c(Grant[i,j],Planetarium[which(Grant$Rdate[i]==Planetarium$Rdate,averagesP[which(Grant$Rdate[i]==averagesP$Rdate),j]),j]))+difmeans[2,j-1],
                                                      Grant[i,j]-difmeans[1,j-1]+difmeans[2,j-1])}}
        
        if(Grant$Rdate[i] %in% PlanetP$Rdate == FALSE){
                PlanetP[dim(PlanetP)[1]+1,'Rdate']=Grant$Rdate[i]
                for(j in 2:8){
                        PlanetP[dim(PlanetP)[1],j]=ifelse(Grant$Rdate[i] %in% SWIC$Rdate,
                                                          mean(c(Grant[i,j],Grant[which(Grant$Rdate[i]==SWIC$Rdate),j],averagesP[which(Grant$Rdate[i]==averagesP$Rdate),j]))+difmeans[3,j-1],
                                                          Grant[i,j]-difmeans[1,j-1]+difmeans[3,j-1])}}}

for(i in 1:dim(Planetarium)[1]){
        if(Planetarium$Rdate[i] %in% averagesP$Rdate == FALSE){
                averagesP[dim(averagesP)[1]+1,1]=Planetarium$Rdate[i]
                for(j in 2:8){
                        averagesP[dim(averagesP)[1],j]=Planetarium[i,j]-difmeans[3,j-1]}}
        
        if(Planetarium$Rdate[i] %in% GrantP$Rdate == FALSE){
                GrantP[dim(GrantP)[1]+1,'Rdate']=Planetarium$Rdate[i]
                for(j in 2:8){
                        GrantP[dim(GrantP)[1],j]=ifelse(Planetarium$Rdate[i] %in% SWIC$Rdate,
                                                      mean(c(Planetarium[i,j],SWIC[which(Planetarium$Rdate[i]==SWIC$Rdate),j],averagesP[which(Planetarium$Rdate[i]==averagesP$Rdate),j]))+difmeans[1,j-1],
                                                      Planetarium[i,j]-difmeans[3,j-1]+difmeans[1,j-1])}}
        
        if(Planetarium$Rdate[i] %in% SWICP$Rdate == FALSE){
                SWICP[dim(SWICP)[1]+1,'Rdate']=Planetarium$Rdate[i]
                for(j in 2:8){
                        SWICP[dim(SWICP)[1],j]=ifelse(Planetarium$Rdate[i] %in% Grant$Rdate,
                                                        mean(c(Planetarium[i,j],Grant[which(Planetarium$Rdate[i]==Grant$Rdate),j],averagesP[which(Planetarium$Rdate[i]==averagesP$Rdate),j]))+difmeans[3,j-1],
                                                        Planetarium[i,j]-difmeans[3,j-1]+difmeans[2,j-1])}}}
averagesP$HTdif<-averagesP$RH - averagesP$Temp
GrantP$HTdif<-GrantP$RH - GrantP$Temp
SWICP$HTdif<-SWICP$RH - SWICP$Temp
PlanetP$HTdif<-PlanetP$RH - PlanetP$Temp
##########################################
#Individual plotting w/ Projections
testvariables<-c('Ozone','Temp','Press','RH')
starttime=as.POSIXct('2015-04-01 01:00:00')
endtime=as.POSIXct('2015-10-01 01:00:00')
GrantFrame=GrantP[GrantP$Rdate>starttime & GrantP$Rdate<endtime,'Rdate']
SWICFrame=SWICP[SWICP$Rdate>starttime & SWICP$Rdate<endtime,'Rdate']
PlanetFrame=PlanetP[PlanetP$Rdate>starttime & PlanetP$Rdate<endtime,'Rdate']

par(mfrow=c(2,2))
for (stat in testvariables){
        GrantStat=GrantP[GrantP$Rdate>starttime & GrantP$Rdate<endtime,stat]
        SWICStat=SWICP[SWICP$Rdate>starttime & SWICP$Rdate<endtime,stat]
        PlanetStat=PlanetP[PlanetP$Rdate>starttime & PlanetP$Rdate<endtime,stat]
        col_code=c('green','red','blue')[c(rep(1,length(GrantStat)),rep(2,length(SWICStat)),rep(3,length(PlanetStat)))]
        plot(c(GrantStat,SWICStat,PlanetStat)~c(GrantFrame,SWICFrame,PlanetFrame),
             type="p",pch = 'o', col= col_code,cex=1,
             xlab="Date",ylab=stat,main=c(stat,"2015 w/ projections"))
        #legend("bottomright","(x,y)",legend=c('Grant','SWIC','Planetarium'),col=(c('green','red','blue')),
               #pch=20,cex=.25,pt.cex = 1.5)
}

##########################################
#Individual line plotting w/ Projections
testvariables<-c('Ozone','Temp','Press','RH')
starttime=as.POSIXct('2015-07-01 01:00:00')
endtime=as.POSIXct('2015-08-01 01:00:00')
GrantFrame=GrantP[GrantP$Rdate>starttime & GrantP$Rdate<endtime,'Rdate']
SWICFrame=SWICP[SWICP$Rdate>starttime & SWICP$Rdate<endtime,'Rdate']
PlanetFrame=PlanetP[PlanetP$Rdate>starttime & PlanetP$Rdate<endtime,'Rdate']

par(mfrow=c(2,2))
for (stat in testvariables){
        GrantStat=GrantP[GrantP$Rdate>starttime & GrantP$Rdate<endtime,stat]
        SWICStat=SWICP[SWICP$Rdate>starttime & SWICP$Rdate<endtime,stat]
        PlanetStat=PlanetP[PlanetP$Rdate>starttime & PlanetP$Rdate<endtime,stat]
        plot(GrantStat~GrantFrame,
             type="l", col= 'green',
             xlab="Date",ylab=stat,main=c(stat,"2015 w/ projections"))
        lines(SWICStat~SWICFrame,
             type="l", col= 'red')
        lines(PlanetStat~PlanetFrame,
              type="l", col= 'blue')
        #legend("bottomright","(x,y)",legend=c('Grant','SWIC','Planetarium'),col=(c('green','red','blue')),
        #       pch=20,cex=.25,pt.cex = 1.5)
}

#############################################################
#Testing whether averages are different for damaging ozone levels vs safe levels
testvariables<-c('Ozone','Temp','Press','RH','WindDir','WindSpeed','Rainfall')
for (i in testvariables[2:7]){
        print(i)
        print(t.test(GrantC[GrantC$Ozone>=40,i],GrantC[GrantC$Ozone<40,i]))
        print(t.test(SWICC[SWICC$Ozone>=40,i],SWICC[SWICC$Ozone<40,i]))
        print(t.test(PlanetC[PlanetC$Ozone>=40,i],PlanetC[PlanetC$Ozone<40,i]))}
###############################################################
#as far as research questions
print(t.test(PlanetariumO$Ozone,SWICO$Ozone))
print(t.test(PlanetariumO[PlanetariumO$Ozone>=40,'Ozone'],SWICO[SWICO$Ozone>=40,'Ozone']))
###########################################################################
#Misc. and test code. Build upon or ignore.
legend(1,legend=c('Grant','SWIC','Planetarium'))
plot(Planetarium[Planetarium$Rdate>starttime & Planetarium$Rdate<endtime,'Ozone']~Planetarium[Planetarium$Rdate>starttime & Planetarium$Rdate<endtime,'Rdate'],
     type="p",cex = .2, col='blue',
     xlab="Date",ylab="Ozone(ppb)",main="Planetarium Ozone 2014")

plot(c(Grant$Ozone[1:100],SWIC$Ozone[1:100],Planetarium$Ozone[1:100])~c(Grant$Rdate[1:100],SWIC$Rdate[1:100],Planetarium$Rdate[1:100]),
     type='p',col= c('blue','green','red'))

testingdata$Ozone[testingdata$Rdate>starttime & testingdata$Rdate<endtime]
as.POSIXct('2015-05-01 10:00:00')<as.POSIXct('2015-06-01 10:00:00')

plot(Grant$where~Grant$Rdate,pch='.',ylim=c(.9,1.3))
points(SWIC$where~SWIC$Rdate,pch='.')
points(Planetarium$where~Planetarium$Rdate,pch='.')

plot(GrantO$Ozone~GrantO$HTdif,pch='o',col='green')
points(SWICO$Ozone~SWICO$HTdif,pch='o',col='red')
points(PlanetariumO$Ozone~PlanetariumO$HTdif,pch='o',col='blue')
abline(lm(GrantO$Ozone~GrantO$HTdif),col='green2',lwd=4)
lines(abline(lm(SWICO$Ozone~SWICO$HTdif),col='red2',lwd=4))
lines(abline(lm(PlanetariumO$Ozone~PlanetariumO$HTdif),col='blue2',lwd=4))

plot(GrantP$RH~GrantP$Rdate,pch='.',col='green')
points(Grant$RH~Grant$Rdate,pch='.',col='blue')


#b<-SCorr[1,1]*SWICC$Ozone+SCorr[1,2]*SWICC$Temp+SCorr[1,3]*SWICC$Press+SCorr[1,4]*SWICC$RH+SCorr[1,7]*SWICC$Rainfall+SCorr[1,6]*SWICC$WindSpeed+SCorr[1,9]*SWICC$VPD
reference<-PlanetC#[c(300:950,1060:1523),]
SVars<-SWIC[,c(2:5,7:8,20)]
SCint<-SWICC[SWICC$Rdate %in% reference$Rdate,c(2:5,7:8,20)]
GCint<-GrantC[GrantC$Rdate %in% reference$Rdate,c(2:5,7:8,20)]

PlanetP<-Planetarium[,c(2:5,7,8,20,18)]
variables=c('Ozone','Temp','Press','RH','WindSpeed','Rainfall','VPD')
SR<-data.frame(Ozone=as.matrix(SVars) %*% as.vector(SCorr[c(1:4,6,7,9),1])) +(SVars$Ozone-mean(SVars$Ozone))
SCR<-data.frame(Ozone=(as.matrix(SCint) %*% as.vector(SCorr[c(1:4,6,7,9),1])+(SCint$Ozone-mean(SCint$Ozone))))
trial<-data.frame(Ozone=(as.matrix(SCint) %*% as.vector(SCorr[c(1:4,6,7,9),1])+(SCint$Ozone-mean(SCint$Ozone))+as.matrix(GCint) %*% as.vector(GCorr[c(1:4,6,7,9),1])+(GCint$Ozone-mean(GCint$Ozone))))
for(j in 2:7){
        SR[,variables[j]]<-as.matrix(SVars) %*% as.vector(SCorr[variables[j],c(1:4,6,7,9)])+(SVars[,variables[j]]-mean((SVars[,variables[j]])))
        SCR[,variables[j]]<-as.matrix(SCint) %*% as.vector(SCorr[variables[j],c(1:4,6,7,9)])+(SCint[,variables[j]]-mean((SCint[,variables[j]])))
        trial[,variables[j]]<-as.matrix(SCint) %*% as.vector(SCorr[c(1:4,6,7,9),variables[j]])+(SCint[,variables[j]]-mean((SCint[,variables[j]])))+as.matrix(GCint) %*% as.vector(GCorr[c(1:4,6,7,9),variables[j]])+(GCint[,variables[j]]-mean((GCint[,variables[j]])))
}
for (i in 1:7){
        print(variables[i])
        print(cor(SCR[,variables[i]],reference[,variables[i]]))
        print(cor(trial[,variables[i]],reference[,variables[i]]))
}
for(i in 1:dim(SWIC)[1]){
        if(SWIC$Rdate[i] %in% PlanetP$Rdate == FALSE){
                PlanetP[dim(PlanetP)[1]+1,'Rdate']=SWIC$Rdate[i]
                for(j in 1:7){
                        regression=summary(lm(reference[,variables[j]]~trial[,variables[j]]))
                        PlanetP[dim(PlanetP)[1],j]=SR[i,variables[j]]*regression$coefficients[2,1]+regression$coefficients[1,1]}}}
#######################################################
MSD<-SWICDL[SWICDL$Rdate %in% PlanetDL$Rdate==FALSE,] #Missing SWIC Dates (for the Planetarium)
write.csv(MSD,"Missing SWIC Dates 2015.csv")
##########################################################

missing_data<-read.csv("Missing Planetarium Ozone 2015.csv")
missing_data$Rdate=as.POSIXct(missing_data$Date,format="%m/%d/%Y %H:%M")

plot(Planetarium$Ozone~Planetarium$Rdate,pch=18)
points(missing_data$Ozone~missing_data$Rdate,pch=18,col='red')
