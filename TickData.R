#### Set Up ####

install.packages("highfrequency")
install.packages("devtools")
install.packages("kimisc")
install.packages("quantmod")
library(zoo)
library(xts)
library(highfrequency)
library(plyr)
library(devtools)
library(plyr)
library(TTR)
library(quantmod)
#yooooooooooo

#### Load Data ####

setwd("~/Documents")
nsd <- read.csv("companylist.csv", sep=",", header = T)
View(nsd)

round(runif(100, 1, 3178.5))

nsd2<-nsd[c(round(runif(250, .5, 3178.5))),]
View(nsd2)
nsd2[,1]

trades <- read.csv("trades.csv", header = T)


#### Clean Data ####
trades<-trades[,-c(6, 7)]
names(trades)<-c("symbol", "date", "time", "price", "size")
datetime<-paste(trades$date, trades$time)
trades<-data.frame(trades, datetime)
trades$datetime<-strptime(as.character(trades$datetime) ,
                          format= '%Y%m%d %H:%M:%S')
trades<-trades[-c(2,3)]
trades<-data.frame(trades[,1], trades[,4], trades[,2], trades[,3], 
                   as.numeric(trades$datetime))
names(trades)<-c("symbol", "datetime", "price", "size", "numericdate")

Z<-"2013-08-22 09:30:00"<=trades$datetime &
          "2013-08-22 16:00:00">=trades$datetime

trades<-trades[Z,]
row.names(trades) <- 1:nrow(trades)

pxv<-trades$price*trades$size
trades<-data.frame(trades, pxv)


counting<-c(1:length(unique(trades$symbol)) )
for (i in c(1:(length(unique(trades$symbol))))) {
  counting[i]<-sum( unique(trades$symbol)[i]==trades$symbol)
}
counting1<-counting
counting1
for(i in c(2:(length(counting1)))) {
  counting1[i]<-counting1[i]+counting1[i-1]
}
counting1<-c(0, counting1)


#Set up vectors for the (time consuming) loop coming u
max(counting)
mega<-matrix(NA, nrow=max(counting), ncol=(6*length(unique(trades$symbol))))
View(mega)
counting2<-counting1[-c(1)]
counting2<-c(1,counting2)
counting2

#Important numbers to assess how hard data will be to process in the next Loop
length(unique(trades$symbol))
head(counting)
head(counting1)


#Loop to seperate data into one large matrix defined by z columns, with (1,...,n), n:= # of
#interesting parameters and z=n*J, J=# of stocks, and m:= # of observations for stock j.
for (i in c(1:length(trades$symbol))) {
  for (j in c(1: (length(counting1)-1) ) ) {
    if(i>counting1[j] & j<=counting[j+1]) {
      mega[,c( (6*j-5) : (6*(j))) ] <- rbind(as.matrix(trades[c(counting1[j]+1:counting1[j+1]),] , ncol=6),
                                      matrix(NA, nrow=(max(counting)-(counting1[j+1])), ncol=6))
    }
  }
}
mega<-data.frame(t(unique(trades$symbol)), mega)


#### Code Playground #### hii

counting1[1]
1:counting1[2]

hzy<-matrix(NA, nrow=(max(counting)), ncol=6)
dim(hzy)

hzy[, c( (6*1-5) : (6*(1)+1)) ] <- rbind(as.matrix(trades[c(1:counting1[2]),], ncol=6),
                                         matrix(NA, nrow=(max(counting)-(counting1[2])), ncol=6))
      


View(hzy[, c( (6*1-5) : (6*(1))) ])

ll<-rbind(as.matrix(trades[c(1:counting1[2]),], ncol=6),
          matrix(NA, nrow=(max(counting)-(counting1[2])), ncol=6))
View(ll)
dim(ll)
dim(hzy)
View(hzy)
max(counting)


c(1:counting1[2])
length(trades$symbol=="ABCD")

counting1
c(1:length(trades$symbol))


mega<-list()


max_length <- max(sapply(mega,length))
bb<-sapply(mega, function(x){
  c(x, rep(NA, max_length - length(x)))
})
View(mega)

matrix()^

mega<-data.frame(0, ncol=)

for (j in c(1:199542)) {
  for(i in c(1:length(unique(trades$symbol)))) {
      mega[(j-counting[i]),c((6*i-5):6*i)]<-trades[i,]
  }
  }


unique(trades$symbol)

