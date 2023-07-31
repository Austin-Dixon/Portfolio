data14<-read.csv("Ozone Data 2014.csv")
testingdata<-data14[(data14$RH<101)&(data14$Ozone<1000 & data14$Ozone>=0)&(data14$WindSpeed<=100),]
testingdata$Rdate=as.POSIXct(testingdata$InstrumentDT,format="%m/%d/%Y %H:%M:%S")
#plot(testingdata$Ozone~testingdata$Rdate)
Grant<-testingdata[testingdata$Latitude==38.325623,]
Planetarium<-testingdata[testingdata$Latitude==38.632801,]
SWIC<-testingdata[testingdata$Latitude==38.310413,]
#summary(Grant)
starttime=as.POSIXct('2014-06-01 10:00:00')
endtime=as.POSIXct('2014-06-07 10:00:00')
plot(testingdata[testingdata$Rdate>starttime & testingdata$Rdate<endtime,'Ozone']~testingdata[testingdata$Rdate>starttime & testingdata$Rdate<endtime,'Rdate'],
     type="p",cex = .35, col= c('blue','red','green'),
     xlab="Date",ylab="Ozone(ppb)",main="Grant Ozone 2014")

testingdata$Ozone[testingdata$Rdate>starttime & testingdata$Rdate<endtime]
as.POSIXct('2015-05-01 10:00:00')<as.POSIXct('2015-06-01 10:00:00')
