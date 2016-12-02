install.packages("highfrequency")
install.packages("devtools")
library(zoo)
library(xts)
library(highfrequency)
library(plyr)
library(devtools)
#yoooo


setwd("~/Documents")
nsd <- read.csv("companylist.csv", sep=",", header = T)
View(nsd)

round(runif(100, 1, 3178.5))

nsd2<-nsd[c(round(runif(250, .5, 3178.5))),]
View(nsd2)
nsd2[,1]

library(help="highfrequency")

trades <- read.csv("trades.csv", sep=",", header = T)
View(trades)
trades<-trades[-c(2, 6, 7)]

trades$DATE=paste(trades$DATE, trades$TIME, sep=" ") #adds a new col to data frame
#by merging two existing cols
View(trades)

trades$DATE=as.POSIXct(strptime(trades$DATE,"%Y%m%d %H:%M:%S" )) #convert 
#to date/time objects

trades$DATE=as.POSIXct(strptime(trades$DATE,"%d.%m.%Y %H:%M" ))

trades$Duration=difftime(trades$DATE,trades$DATE, units="hours") #adds new col 
#calculating the time difference in hours