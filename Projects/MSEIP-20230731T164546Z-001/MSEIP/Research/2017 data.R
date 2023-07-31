data17<-read.csv("Ozone Data 2017.csv")
summary(data17)
testingdata<-data17[(data17$RH<101)&(data17$Ozone<1000 & data17$Ozone>=0)&(data17$WindSpeed<=100),]
summary(testingdata)
Grant<-testingdata[testingdata$Latitude==38.325623,]
Planetarium<-testingdata[testingdata$Latitude==38.632801,]
SWIC<-testingdata[testingdata$Latitude==38.310413,]
summary(Grant)
