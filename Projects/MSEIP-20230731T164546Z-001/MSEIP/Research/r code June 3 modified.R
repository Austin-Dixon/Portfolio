###install.packages("datetime")
###library("datetime")

###test <- "2013-12-25T04:32:16.500-08:00"
###z <- as.POSIXct(test,format="%Y-%m-%dT%H:%M:%OS")

###test1 <- "2013-12-25 04:32:16"
###z1 <- as.POSIXct(test1,format="%Y-%m-%d %H:%M:%S")

#####  I don't need to convert any datetime - just do numeric

ozonedata <- read.csv("OS3_all4-11-2019.csv")
head(ozonedata)
###  The datetime entries work directly - R recognizes them as dates
###  The InstrumentD and ServerD (date only) is not recognized as a date
###  (Splitting the columns in excel probably affects the formatting

###  Still might be an issue with time - central time??
###  I think these next lines might not be needed (since might be okay)
ozonedata$Inst_DT = as.POSIXct(ozonedata$InstrumentDT,format="%Y-%m-%d %H:%M:%S")
ozonedata$Server_DT = as.POSIXct(ozonedata$ServerDT,format="%Y-%m-%d %H:%M:%S")
###  Could check to see if Inst_DT is the same as InstrumentDT

###  I think I need these next two lines (see comment above)
ozonedata$Instdate <- as.Date(ozonedata$InstrumentD, "%m/%d/%Y")
ozonedata$numInstD <- as.numeric(ozonedata$Instdate)

ozonedata$numInstDT <- as.numeric(ozonedata$Inst_DT)
ozonedata$numServerDT <- as.numeric(ozonedata$Server_DT)

ozonedata$numInstDTfix <- ozonedata$numInstDT + (ozonedata$GMTOffset)*(3600)


ozonedata$diffInst_Serv <- ozonedata$numInstDT -ozonedata$numServerDT

head(ozonedata)
tail(ozonedata)

summary(ozonedata)


dev.new()
hist(ozonedata$diffInst_Serv)

ozonedata$site <- ifelse(ozonedata$Longitude ==-90.212738,1, 
    ifelse(ozonedata$Longitude ==-89.552177,2,3))

ozonedata$numInstDTfix1 <- ifelse(ozonedata$site ==3,
   ozonedata$numInstDTfix, ozonedata$numInstDT+3600)



names(ozonedata)
###  If site variable is created, I don't need first set of subsetting
Grantdata <- subset(ozonedata, ozonedata$Longitude ==-90.212738)
SWICdata <- subset(ozonedata, ozonedata$Longitude ==-89.552177)
Planetdata <- subset(ozonedata, ozonedata$Longitude ==-90.269699)

Grantdata <- subset(ozonedata, ozonedata$site == 1)
SWICdata <- subset(ozonedata, ozonedata$site == 2)
Planetdata <- subset(ozonedata, ozonedata$site ==3)

### subset to first few days of collection
begindata <- subset(ozonedata, ozonedata$Instdate < 16550)
Grantbegin <- subset(begindata, begindata$site == 1)
SWICbegin <- subset(begindata, begindata$site == 2)
Planetbegin <- subset(begindata, begindata$site ==3)

#####  make average hourly temp variable???

Grant_list <- aggregate(list(temperature = Grantdata$Temp), 
          list(hourofday = cut(Grantdata$Inst_DT, "1 hour")), 
          mean)
class(Grant_list)

#####  Try putting other variables into this new data frame
Grant_list2 <- aggregate(list(temperature = Grantdata$Temp,
     ozone = Grantdata$Ozone,
     pressure = Grantdata$Pressure,
     rh = Grantdata$RH,
     winddir = Grantdata$WindDir,
      windspeed= Grantdata$WindSpeed.mph.,
     rainfall = Grantdata$Rainfall,
     site = Grantdata$site  ), 
          list(hourofday = cut(Grantdata$Inst_DT, "1 hour")), 
          mean)
class(Grant_list2)
Grant_list2$hour_DT <- as.POSIXct(Grant_list2$hourofday,format="%Y-%m-%d %H:%M:%S")

names(Grant_list2)
head(Grant_list2)
SWIC_list2 <- aggregate(list(temperature = SWICdata$Temp,
     ozone = SWICdata$Ozone,
     pressure = SWICdata$Pressure,
     rh = SWICdata$RH,
     winddir = SWICdata$WindDir,
      windspeed= SWICdata$WindSpeed.mph.,
     rainfall = SWICdata$Rainfall,
     site = SWICdata$site  ), 
          list(hourofday = cut(SWICdata$Inst_DT, "1 hour")), 
          mean)
class(SWIC_list2)
names(SWIC_list2)
SWIC_list2$hour_DT <- as.POSIXct(SWIC_list2$hourofday,format="%Y-%m-%d %H:%M:%S")


Planet_list2 <- aggregate(list(temperature = Planetdata$Temp,
     ozone = Planetdata$Ozone,
     pressure = Planetdata$Pressure,
     rh = Planetdata$RH,
     winddir = Planetdata$WindDir,
      windspeed= Planetdata$WindSpeed.mph.,
     rainfall = Planetdata$Rainfall,
     site = Planetdata$site  ), 
          list(hourofday = cut(Planetdata$Inst_DT, "1 hour")), 
          mean)
class(Planet_list2)
Planet_list2$hour_DT <- as.POSIXct(Planet_list2$hourofday,format="%Y-%m-%d %H:%M:%S")

names(Planet_list2)

total_hour <- rbind(Grant_list2,SWIC_list2,Planet_list2)
dim(total_hour)
head(total_hour)

total_hour$hour_DT = as.POSIXct(total_hour$hourofday,format="%Y-%m-%d %H:%M:%S")

total_hour$numhour_DT <- as.numeric(total_hour$hour_DT)

####  date and time seem consistent across sites
total_hour$numhour_DTfix <- 
ifelse(total_hour$site ==3,
   total_hour$numhour_DT, total_hour$numhour_DT-21600)

total_hour$numhour_DTdiff <- total_hour$numhour_DT-total_hour$numhour_DTfix

summary(total_hour$numhour_DTdiff)
names(total_hour)
par(mfrow=c(3,1))
######
color_codes = c("green", "red", "blue")[total_hour$site]
dev.new()
plot(total_hour$temperature ~ total_hour$hour_DT,
      pch=20,main="2015 Temperature 3 sites",ylim=c(35,75),
      col=color_codes,xlab="Time", ylab="Temperature")
dev.new()
plot(total_hour$ozone ~ total_hour$hour_DT,
      pch=20,main="2015 Ozone 3 sites",    ## ylim=c(35,75),
      col=color_codes,xlab="Time", ylab="Ozone Level")
dev.new()
plot(total_hour$pressure ~ total_hour$hour_DT,
      pch=20,main="2015 Pressure 3 sites",    ## ylim=c(35,75),
      col=color_codes,xlab="Time", ylab="Pressure")
dev.new()
plot(total_hour$rh ~ total_hour$hour_DT,
      pch=20,main="2015 Relative Humidity 3 sites",    ## ylim=c(35,75),
      col=color_codes,xlab="Time", ylab="Relative Humidity")

dev.new()
plot(total_hour$winddir ~ total_hour$hour_DT,
      pch=20,main="2015 Wind Direction 3 sites",    ## ylim=c(35,75),
      col=color_codes,xlab="Time", ylab="Wind Direction")
dev.new()
plot(total_hour$windspeed ~ total_hour$hour_DT,
      pch=20,main="2015 Wind Speed 3 sites",    ## ylim=c(35,75),
      col=color_codes,xlab="Time", ylab="Wind Speed")
dev.new()
plot(total_hour$rainfall ~ total_hour$hour_DT,
      pch=20,main="2015 Rainfall 3 sites",    ## ylim=c(35,75),
      col=color_codes,xlab="Time", ylab="Rainfall")





######
color_codes = c("green", "red", "blue")[total_hour$site]



###  put graphs on one page  (not helpful - too squished)
dev.new()
par(mfrow=c(2,2))

plot(total_hour$temperature ~ total_hour$hour_DT,
      pch=20,main="2015 Temperature 3 sites",ylim=c(35,75),
      col=color_codes,xlab="Time", ylab="Temperature")


plot(total_hour$pressure ~ total_hour$hour_DT,
      pch=20,main="2015 Pressure 3 sites",    ## ylim=c(35,75),
      col=color_codes,xlab="Time", ylab="Pressure")

plot(total_hour$ozone ~ total_hour$hour_DT,
      pch=20,main="2015 Ozone 3 sites",    ## ylim=c(35,75),
      col=color_codes,xlab="Time", ylab="Ozone Level")


plot(total_hour$rh ~ total_hour$hour_DT,
      pch=20,main="2015 Relative Humidity 3 sites",    ## ylim=c(35,75),
      col=color_codes,xlab="Time", ylab="Relative Humidity")



### Graph with numeric version of hour
dev.new()
plot(total_hour$temperature ~ total_hour$numhour_DT,
      pch=20,main="2015 Temperature 3 sites Numeric hour",ylim=c(35,75),
      col=color_codes,xlab="Time", ylab="Temperature")
### graph with fixed hour for sites 1 and 2 (numeric)

dev.new()
plot(total_hour$temperature ~ total_hour$numhour_DTfix,
      pch=20,main="2015 Temperature 3 sites Adjusted hour",ylim=c(35,75),
      col=color_codes,xlab="Time", ylab="Temperature")




####  Create different name variable for temp for the three sites
Grantbegin$TempGrant <- Grantbegin$Temp
SWICbegin$TempSWIC <- SWICbegin$Temp
Planetbegin$TempPlanet <- Planetbegin$Temp

names(Grantbegin)
###  Try both with the fixed time and without the fixed time
###  Without fixed time
Grantvars <- c("numInstDT", "TempGrant")
SWICvars <- c("numInstDT", "TempSWIC")
Planetvars <- c("numInstDT", "TempPlanet")
newGrant <- Grantbegin[Grantvars]
newSWIC <- SWICbegin[SWICvars]
newPlanet <- Planetbegin[Planetvars]
newGrant$hour <- newGrant$numInstDT %/%3600
newSWIC$hour <- newSWIC$numInstDT %/%3600
newPlanet$hour <- newPlanet$numInstDT %/%3600

subGrant <- subset(newGrant, !duplicated(hour))
dim(subGrant)
subSWIC <- subset(newSWIC, !duplicated(hour))
dim(subSWIC)
subPlanet <- subset(newPlanet, !duplicated(hour))
dim(subPlanet)
total <- merge(subGrant,subPlanet,by="hour")
dim(total)
total$TempDiff <- total$TempGrant-total$TempPlanet

summary(total$TempDiff)
dev.new()
plot(total$TempDiff~total$hour)


total1 <- merge(total,subSWIC,by="hour")
dim(total1)


total


dim(Grantbegin)
dim(SWICbegin)
dim(Planetbegin)
dim(begindata)



dev.new()
par(mfrow=c(2,1))
plot(total$TempGrant ~ total$hour,main="Time not Fixed")
plot(total$TempPlanet ~ total$hour,main="Time not Fixed" )

dev.new()
plot(total$TempGrant ~ total$hour,pch=20, col="green",main="Time not Fixed" )
par(new=TRUE)
plot(total$TempPlanet, ann=FALSE, axes=FALSE,col='blue')




###  With fixed time
Grantvarsfix <- c("numInstDTfix", "TempGrant")
SWICvarsfix <- c("numInstDTfix", "TempSWIC")
Planetvarsfix <- c("numInstDTfix", "TempPlanet")


newGrantfix <- Grantbegin[Grantvarsfix]
newSWICfix <- SWICbegin[SWICvarsfix]
newPlanetfix <- Planetbegin[Planetvarsfix]

newGrantfix$hour <- newGrantfix$numInstDTfix %/%3600
newSWICfix$hour <- newSWICfix$numInstDTfix %/%3600
newPlanetfix$hour <- newPlanetfix$numInstDTfix %/%3600

subGrantfix <- subset(newGrantfix, !duplicated(hour))
dim(subGrantfix)
subSWICfix <- subset(newSWICfix, !duplicated(hour))
dim(subSWICfix)
subPlanetfix <- subset(newPlanetfix, !duplicated(hour))
dim(subPlanetfix)



dim(newGrantfix)
dim(newSWICfix)
dim(newPlanetfix)


totalfix <- merge(subGrantfix,subPlanetfix,by="hour")
dim(totalfix)
totalfix$TempDiff <- totalfix$TempGrant-totalfix$TempPlanet

summary(totalfix$TempDiff)
dev.new()
plot(totalfix$TempDiff~totalfix$hour)


total1fix <- merge(totalfix,subSWICfix,by="hour")
dim(total1fix)


totalfix


dev.new()
par(mfrow=c(2,1))
plot(totalfix$TempGrant ~ totalfix$hour)
plot(totalfix$TempPlanet ~ totalfix$hour)

dev.new()
plot(totalfix$TempGrant ~ totalfix$hour,pch=20, col="green",main="Time fixed")
par(new=TRUE)
plot(totalfix$TempPlanet, ann=FALSE, axes=FALSE,col='blue')


###############################################################

### try to graph on the same plot small set of data by date
###  Ozone and Temp

beginmean_Grant <- aggregate(Grantbegin$Ozone,list(Grantbegin$Inst_DT),mean,rm.na=TRUE)
beginmean_SWIC <- aggregate(SWICbegin$Ozone,list(SWICbegin$Inst_DT),mean,rm.na=TRUE)
beginmean_Planet <- aggregate(Planetbegin$Ozone,list(Planetbegin$Inst_DT),mean,rm.na=TRUE)


tempmean_Grant <- aggregate(Grantbegin$Temp,list(Grantbegin$Inst_DT),mean,rm.na=TRUE)
tempmean_SWIC <- aggregate(SWICbegin$Temp,list(SWICbegin$Inst_DT),mean,rm.na=TRUE)
tempmean_Planet <- aggregate(Planetbegin$Temp,list(Planetbegin$Inst_DT),mean,rm.na=TRUE)



class(ozonemean_Grant)
beginmean_Grant$site <- 1
beginmean_SWIC$site <- 2
beginmean_Planet$site <- 3

names(ozonemean_Grant)
names(ozonemean_SWIC)
names(ozonemean_Planet)
head(test)


test<- rbind(beginmean_Grant,beginmean_SWIC,beginmean_Planet)
tempmean_Grant$site <- 1
tempmean_SWIC$site <- 2
tempmean_Planet$site <- 3

testTemp <- rbind(tempmean_Grant,tempmean_SWIC,tempmean_Planet)
testTemp$numInstDT <- as.numeric(test$Group.1)
dim(testTemp)
names(testTemp)
head(testTemp)
tail(testTemp)



dim(test)
names(test)
head(test)
tail(test)
test$numInstDT <- as.numeric(test$Group.1)


#####  test$dates <- as.Date(test$Group.1, "%m/%d/%Y")  ## not needed for this run

color_easy = c("red", "blue", "green")[testTemp$site]
dev.new()
plot(testTemp$x ~ testTemp$numInstDT,pch=20,main="2015 Temperature 3 sites",ylim=c(35,75),
      col=color_easy,xlab="Time", ylab="Temperature")

dev.new()
plot(testTemp$x ~ testTemp$Group.1,pch=20,main="2015 Temperature 3 sites",ylim=c(35,70),
      col=color_easy,xlab="Time", ylab="Temperature")

head(testTemp)


color_easy = c("red", "blue", "green")[test$site]

dev.new()
plot(test$x ~ test$numInstDT,pch=17,main="2015 Ozone Level 3 sites",ylim=c(0,60),
      col=color_easy,xlab="Time", ylab="Ozone level ppb")

dev.new()
plot(test$x ~ test$Group.1,pch=17,main="2015 Ozone Level 3 sites",ylim=c(0,60),
      col=color_easy,xlab="Time", ylab="Ozone level ppb")


head(Grantbegin)
head(SWICbegin)
head(Planetbegin)




dim(Grantdata)
dim(SWICdata)
dim(Planetdata)
dim(ozonedata)

dev.new()
par(mfrow=c(3,1))
hist(Grantdata$diffInst_Serv,main="Grant's Farm")
hist(SWICdata$diffInst_Serv,main="SWIC")
hist(Planetdata$diffInst_Serv,main="Planetarium")

head(Grantdata)
head(SWICdata)
head(Planetdata)

tail(Grantdata)
tail(SWICdata)
tail(Planetdata)



dim(ozonedata)
dim(Grantdata)
dim(SWICdata)
dim(Planetdata)

###  Three separate graphs - all data by 15 minute intervals - too much?
dev.new()
plot(Grantdata$Ozone ~ Grantdata$Inst_DT, type="l", col="green", lwd=1, 
        xlab="Day", ylab="Ozone (ppb)", main="Grant's Farm 2015")
dev.new()
plot(SWICdata$Ozone ~ SWICdata$Inst_DT, type="l", col="red", lwd=1, 
        xlab="Day", ylab="Ozone (ppb)", main="SWIC 2015")
dev.new()
plot(Planetdata$Ozone ~ Planetdata$Inst_DT, type="l", col="blue", lwd=1, 
        xlab="Day", ylab="Ozone (ppb)", main="Planetarium 2015")


####  using numeric date time variable (and putting on the same window, but maybe not same scale
dev.new()
par(mfrow=c(3,1))
plot(Grantdata$Ozone ~ Grantdata$numInstDT, type="l", col="green", lwd=1, 
        xlab="Day", ylab="Ozone (ppb)", main="Grant's Farm 2015")

plot(SWICdata$Ozone ~ SWICdata$numInstDT, type="l", col="red", lwd=1, 
        xlab="Day", ylab="Ozone (ppb)", main="SWIC 2015")

plot(Planetdata$Ozone ~ Planetdata$numInstDT, type="l", col="blue", lwd=1, 
        xlab="Day", ylab="Ozone (ppb)", main="Planetarium 2015")

dev.new()
plot(Grantdata$Ozone ~ Grantdata$Instdate)

head(Grantdata)

dev.new()
plot(Grantdata$Ozone ~ Grantdata$Inst_DT)



##  subset to first few months?
subozonedata <- subset(ozonedata, ozonedata$Instdate < 16576)
dim(subozonedata)
tail(subozonedata)

###shortozonedata <- subset(subozonedata,subozonedata$Instdate > 16555)
shortozonedata <- subset(subozonedata,subozonedata$Instdate > 16569)




dim(shortozonedata)
head(shortozonedata)
tail(shortozonedata)

shortGrantdata <- subset(shortozonedata, shortozonedata$Longitude ==-90.212738)
shortSWICdata <- subset(shortozonedata, shortozonedata$Longitude ==-89.552177)
shortPlanetdata <- subset(shortozonedata, shortozonedata$Longitude ==-90.269699)

dim(shortGrantdata)
dim(shortSWICdata)
dim(shortPlanetdata)
head(shortGrantdata)
head(shortSWICdata)
head(shortPlanetdata)

tail(shortGrantdata)
tail(shortSWICdata)
tail(shortPlanetdata)



dev.new()
par(mfrow=c(3,1))
plot(shortGrantdata$Ozone ~ shortGrantdata$Inst_DT, type="l", col="green", lwd=1, 
        xlab="Day", ylab="Ozone (ppb)", main="Grant's Farm 2015")

plot(shortSWICdata$Ozone ~ shortSWICdata$Inst_DT, type="l", col="red", lwd=1, 
        xlab="Day", ylab="Ozone (ppb)", main="SWIC 2015")

plot(shortPlanetdata$Ozone ~ shortPlanetdata$Inst_DT, type="l", col="blue", lwd=1, 
        xlab="Day", ylab="Ozone (ppb)", main="Planetarium 2015")

dev.new()
par(mfrow=c(3,1))
plot(shortGrantdata$Temp ~ shortGrantdata$Inst_DT, type="l", col="green", lwd=1, 
        xlab="Day", ylab="Temperature", main="Grant's Farm 2015")

plot(shortSWICdata$Temp ~ shortSWICdata$Inst_DT, type="l", col="red", lwd=1, 
        xlab="Day", ylab="Temperature", main="SWIC 2015")

plot(shortPlanetdata$Temp ~ shortPlanetdata$Inst_DT, type="l", col="blue", lwd=1, 
        xlab="Day", ylab="Temperature", main="Planetarium 2015")




dev.new()
par(mfrow=c(3,1))
plot(shortGrantdata$Temp ~ shortGrantdata$Inst_DT, type="l", col="green", lwd=1, 
        xlab="Day", ylab="Temperature)", main="Grant's Farm 2015")

plot(shortSWICdata$Temp ~ shortSWICdata$Inst_DT, type="l", col="red", lwd=1, 
        xlab="Day", ylab="Temperature", main="SWIC 2015")

plot(shortPlanetdata$Temp ~ shortPlanetdata$Inst_DT, type="l", col="blue", lwd=1, 
        xlab="Day", ylab="Temperature", main="Planetarium 2015")

dev.new()
# first plot
plot(shortGrantdata$Temp ~ shortGrantdata$Inst_DT, type="l", col="green", lwd=1, 
        xlab="Day", ylab="Temperature", main="2015")

# second plot
par(new = TRUE)
plot(shortSWICdata$Temp ~ shortSWICdata$Inst_DT, type="l", col="red", lwd=1, 
      axes = FALSE,  xlab="", ylab="")
par(new = TRUE)
plot(shortPlanetdata$Temp ~ shortPlanetdata$Inst_DT, type="l", col="blue", lwd=1, 
      axes = FALSE,  xlab="", ylab="")

dev.new()
# first plot
plot(shortGrantdata$Ozone ~ shortGrantdata$Inst_DT, type="l", col="green", lwd=1, 
        xlab="Day", ylab="Ozone (ppb)", main="2015")

# second plot
par(new = TRUE)
plot(shortSWICdata$Ozone ~ shortSWICdata$Inst_DT, type="l", col="red", lwd=1, 
      axes = FALSE,  xlab="", ylab="")
par(new = TRUE)
plot(shortPlanetdata$Ozone ~ shortPlanetdata$Inst_DT, type="l", col="blue", lwd=1, 
      axes = FALSE,  xlab="", ylab="")

dev.new()
# first plot
plot(shortGrantdata$Temp ~ shortGrantdata$Inst_DT, type="l", col="green", lwd=1, 
        xlab="Day", ylab="Temp", main="2015")

# second plot
par(new = TRUE)
plot(shortSWICdata$Temp ~ shortSWICdata$Inst_DT, type="l", col="red", lwd=1, 
      axes = FALSE,  xlab="", ylab="")
par(new = TRUE)
plot(shortPlanetdata$Temp ~ shortPlanetdata$Inst_DT, type="l", col="blue", lwd=1, 
      axes = FALSE,  xlab="", ylab="")


dev.new()
# first plot
plot(shortGrantdata$Pressure ~ shortGrantdata$Inst_DT, type="l", col="green", lwd=1, 
        xlab="Day", ylab="Pressure", main="2015")

# second plot
par(new = TRUE)
plot(shortSWICdata$Pressure ~ shortSWICdata$Inst_DT, type="l", col="red", lwd=1, 
      axes = FALSE,  xlab="", ylab="")
par(new = TRUE)
plot(shortPlanetdata$Pressure ~ shortPlanetdata$Inst_DT, type="l", col="blue", lwd=1, 
      axes = FALSE,  xlab="", ylab="")

dev.new()
# first plot
plot(shortGrantdata$Rainfall ~ shortGrantdata$Inst_DT, type="l", col="green", lwd=1, 
        xlab="Day", ylab="Rainfall", main="2015")

# second plot
par(new = TRUE)
plot(shortSWICdata$Rainfall ~ shortSWICdata$Inst_DT, type="l", col="red", lwd=1, 
      axes = FALSE,  xlab="", ylab="")
par(new = TRUE)
plot(shortPlanetdata$Rainfall ~ shortPlanetdata$Inst_DT, type="l", col="blue", lwd=1, 
      axes = FALSE,  xlab="", ylab="")



dev.new()
par(mfrow=c(3,1))
plot(shortGrantdata$Rainfall ~ shortGrantdata$Inst_DT, type="l", col="green", lwd=1, 
        xlab="Day", ylab="Rainfall)", main="Grant's Farm 2015")

plot(shortSWICdata$Rainfall ~ shortSWICdata$Inst_DT, type="l", col="red", lwd=1, 
        xlab="Day", ylab="Rainfall", main="SWIC 2015")

plot(shortPlanetdata$Rainfall ~ shortPlanetdata$Inst_DT, type="l", col="blue", lwd=1, 
        xlab="Day", ylab="Rainfall", main="Planetarium 2015")




head(shortGrantdata)
head(shortSWICdata)
head(shortPlanetdata)


tail(shortGrantdata)
tail(shortSWICdata)
tail(shortPlanetdata)


####  using numeric date time variable
dev.new()
par(mfrow=c(3,1))
plot(shortGrantdata$Ozone ~ shortGrantdata$numInstDT, type="l", col="aquamarine4", lwd=1, 
        xlab="Day", ylab="Ozone (ppb)", main="Grant's Farm 2015 NOT FIX")

plot(shortSWICdata$Ozone ~ shortSWICdata$numInstDT, type="l", col="darkorange4", lwd=1, 
        xlab="Day", ylab="Ozone (ppb)", main="SWIC 2015 NOT FIX")

plot(shortPlanetdata$Ozone ~ shortPlanetdata$numInstDT, type="l", col="darkslateblue", lwd=1, 
        xlab="Day", ylab="Ozone (ppb)", main="Planetarium 2015 NOT FIX")


dev.new()
par(mfrow=c(3,1))
plot(shortGrantdata$Temp ~ shortGrantdata$numInstDT, type="l", col="green", lwd=1, 
        xlab="Day", ylab="Temperature)", main="Grant's Farm 2015")

plot(shortSWICdata$Temp ~ shortSWICdata$numInstDT, type="l", col="red", lwd=1, 
        xlab="Day", ylab="Temperature", main="SWIC 2015")

plot(shortPlanetdata$Temp ~ shortPlanetdata$numInstDT, type="l", col="blue", lwd=1, 
        xlab="Day", ylab="Temperature", main="Planetarium 2015")



dev.new()
par(mfrow=c(3,1))
plot(shortGrantdata$Ozone ~ shortGrantdata$numInstDTfix, type="l", col="aquamarine4", lwd=1, 
        xlab="Day", ylab="Ozone (ppb)", main="Grant's Farm 2015 FIX")

plot(shortSWICdata$Ozone ~ shortSWICdata$numInstDTfix, type="l", col="darkorange4", lwd=1, 
        xlab="Day", ylab="Ozone (ppb)", main="SWIC 2015 FIX")

plot(shortPlanetdata$Ozone ~ shortPlanetdata$numInstDTfix, type="l", col="darkslateblue", lwd=1, 
        xlab="Day", ylab="Ozone (ppb)", main="Planetarium 2015 FIX")

#Testing notes
> dim(ozonedata[ozonedata$Ozone>30,])
[1] 16005    26
> dim(Grantdata[Grantdata$Ozone>30,])
[1] 4966   26
> dim(Grantdata[Grantdata$Ozone>30,])[1]
[1] 4966
> 100*dim(Grantdata[Grantdata$Ozone>30,])[1])/dim(Grantdata)[1]
Error: unexpected ')' in "100*dim(Grantdata[Grantdata$Ozone>30,])[1])"
> 100*dim(Grantdata[Grantdata$Ozone>30,])[1]/dim(Grantdata)[1]
[1] 46.31599
> 100*dim(Planetdata[Planetdata$Ozone>30,])[1]/dim(Planetdata)[1]
[1] 48.48686
> 100*dim(SWICdata[SWICdata$Ozone>30,])[1]/dim(SWICdata)[1]
[1] 50.79431
> View(ozonedata)
> Grantdata[Grantdata$Ozone>30,Grantdata$Ozone]
Error in x[j] : only 0's may be mixed with negative subscripts
> Grantdata[Grantdata$Ozone>30,1]
