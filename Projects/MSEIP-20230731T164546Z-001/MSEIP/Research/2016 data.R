data16<-read.csv("Ozone Data 2016.csv")
summary(data16)
testingdata<-data16[(data16$RH<101)&(data16$Ozone<1000 & data16$Ozone>=0)&(data16$WindSpeed<=100),]
summary(testingdata)
Grant<-testingdata[testingdata$Latitude==38.325623,]
Planetarium<-testingdata[testingdata$Latitude==38.632801,]
SWIC<-testingdata[testingdata$Latitude==38.310413,]
summary(Grant)
