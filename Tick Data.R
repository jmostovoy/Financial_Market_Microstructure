install.packages("highfrequency")
install.packages("devtools")
install.packages("kimisc")
library(zoo)
library(xts)
library(highfrequency)
library(plyr)
library(devtools)
library(plyr)
#yooooooooooo

setwd("~/Documents")
nsd <- read.csv("companylist.csv", sep=",", header = T)
View(nsd)

round(runif(100, 1, 3178.5))

nsd2<-nsd[c(round(runif(250, .5, 3178.5))),]
View(nsd2)
nsd2[,1]

trades <- read.csv("trades.csv", header = T)
trades<-trades[-c(6, 7)]
names(trades)<-c("symbol", "date", "time", "price", "size")
datetime<-paste(trades$date, trades$time)
trades<-data.frame(trades, datetime)
trades$datetime<-strptime(as.character(trades$datetime) ,
                          format= '%Y%m%d %H:%M:%S')
trades<-trades[-c(2,3)]
trades<-data.frame(trades[1], trades[4], trades[2], trades[3])

mydate = strptime('16/Oct/2005:07:51:00',format='%d/%b/%Y:%H:%M:%S')
trades$DATE=paste(trades$DATE, trades$TIME, sep=" ") #adds a new col to data frame
#by merging two existing cols
View(trades)

trades$DATE=as.POSIXct(strptime(trades$DATE,"%Y%m%d %H:%M:%S" )) #convert 
#to date/time objects

trades$DATE=as.POSIXct(strptime(trades$DATE,"%d.%m.%Y %H:%M" ))

trades$Duration=difftime(trades$DATE,trades$DATE, units="hours") #adds new col 
#calculating the time difference in hours