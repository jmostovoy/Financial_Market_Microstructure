#### Set Up ####

install.packages("highfrequency")
install.packages("devtools")
install.packages("kimisc")
install.packages("quantmod")
install.packages("progress")
library(zoo)
library(xts)
library(highfrequency)
library(plyr)
library(devtools)
library(plyr)
library(TTR)
library(quantmod)
library(progress)
library(timeDate)
library(xts)
#yooooooooooo

#### Load Data ####

setwd("~/Documents")
nsd <- read.csv("companylist.csv", sep=",", header = T)
View(nsd)

nsd2<-nsd[c(round(runif(250, .5, 3178.5))),]

#Open data
trades <- read.csv("trades.csv", header = T)


#### Clean Data in data.frame format so as to analyize multiple stock statistics ####
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


#Set up vectors for the (time consuming) loop coming up
mega<-matrix(NA, nrow=max(counting), ncol=(6*length(unique(trades$symbol))))
counting2<-counting1[-c(1)]
counting2<-c(1,counting2)

#Important numbers to assess how hard data will be to process in the next Loop
length(unique(trades$symbol))
head(counting)
head(counting1)


#Loop to seperate data into one large matrix defined by z columns, with (1,...,n), n:= # of
#interesting parameters and z=n*J, J=# of stocks, and m:= # of observations for stock j.


lmn<-c(1:length(trades$symbol))
for (i in c(1:length(lmn))) {
  lmn[i]=1
}

for (j in c(2:length(trades$symbol))) {
 if (trades$symbol[j]==trades$symbol[j-1]) {
   lmn[j]<-lmn[j-1]
   } else {
     lmn[j]<-lmn[j-1]+1
   }
}



trades<-data.frame(trades, lmn)
View(trades)

for (j in c(1: (length(counting1)-1) ) ) {
    mega[,c( (6*j-5) : (6*(j))) ] <- rbind(as.matrix(trades[c(j==trades$lmn), c(1:6)] , ncol=6),
                                             matrix(NA, nrow=max(counting)-sum(c(j==trades$lmn)), 
                                                    ncol=6))

}

#Add company tickers to first column
companies<- rbind(as.matrix(unique(trades$symbol) , ncol=1),
            matrix(NA, nrow=(max(counting)-length(unique(trades$symbol))), ncol=1))
mega<-data.frame(cbind(companies, mega))

#Rename matrix columns
nms<-matrix(c("symbol", "datetime", "price", "size", 
              "numericdate", "price x size"), ncol=1, nrow=6*length(counting))
nms<-as.vector(c("all stock tickers", as.vector(nms)))
names(mega)<- nms

as.vector(nms)

#Not that important Stats
max(counting)
min(counting)
mean(counting)
dim(mega)

#Change NULL -> _
mega[is.na(mega)]<- " "

#Write csv file
write.csv(mega, file = "mega_null.csv", row.names = F, col.names = T)

####Taking just (for example) non-cleaned fox data...####
fox_trades<-read.csv("trades.csv", header = T)
View(fox_trades)
fox_trades<-fox_trades[fox_trades$SYMBOL=="FOXA",]
fox_quotes<-read.csv("quotes.csv", header = T)
View(fox_quotes)
fox_quotes<-fox_quotes[fox_quotes$SYMBOL=="FOXA",]
write.csv(fox_trades, file = "fox_trades.csv", row.names = F, col.names = T)
write.csv(fox_quotes, file = "fox_quotes.csv", row.names = F, col.names = T)


##### Cleaning Data through package xts format so as to analyize individual stock statistics #####

from <- "2013-08-22"
to <- "2013-08-22"
ticker<-"FOXA"
datasource <- "~/Documents/raw_data"
datadestination <- "~/Documents/xts_data"
convert(from=from, to=to1, datasource=datasource, 
        datadestination=datadestination, trades = T,  quotes = F, 
        ticker=ticker, dir = TRUE, extension = "csv", 
        header = TRUE, tradecolnames = NULL, quotecolnames = NULL, 
        format="%Y%m%d %H:%M:%S", onefile = TRUE )
convert(from=from, to=to, datasource=datasource, 
        datadestination=datadestination, trades = F,  quotes = T, 
        ticker=ticker, dir = F, extension = "csv", 
        header = TRUE, tradecolnames = NULL, quotecolnames = NULL, 
        format="%Y%m%d %H:%M:%S", onefile = TRUE )


foxt = TAQLoad(tickers=ticker, from="2013-08-22", to="2013-08-22", trades=TRUE,
                     quotes=FALSE, datasource= datadestination)
foxq = TAQLoad(tickers=ticker, from="2013-08-22", to="2013-08-22",trades=F,
               quotes=TRUE, datasource=datadestination)
View(foxt)
View(foxq)











#### O.G. Slow Code ####
for (i in c(1:length(trades$symbol))) {
  for (j in c(1: (length(counting1)-1) ) ) {
    if(i>counting1[j] & j<=counting[j+1]) {
      mega[,c( (6*j-5) : (6*(j))) ] <- rbind(as.matrix(trades[c(counting1[j]+1:counting1[j+1]),] , ncol=6),
                                      matrix(NA, nrow=(max(counting)-(counting1[j+1])), ncol=6))
    }
  }
}
dzy<- rbind(as.matrix(unique(trades$symbol) , ncol=1),
            matrix(NA, nrow=(max(counting)-length(unique(trades$symbol))), ncol=1))
mega<-cbind(dzy, mega)

#### Code Playground #### hii

bvx<-rbind(as.matrix(trades[c(2==trades$lmn), c(1:6)] , ncol=6),
      matrix(NA, nrow= max(counting)-sum(c(2==trades$lmn)) , ncol=6))
View(bvx)
dim(bvx)
dim(mega)


as.matrix(trades[c(1==trades$lmn), c(1:6)] , ncol=6)

sum(c(1!=trades$lmn))+sum(c(1==trades$lmn))

counting1[1]
1:counting1[2]

hzy<-matrix(NA, nrow=(max(counting)), ncol=6)
dim(hzy)

hzy[, c( (6*1-5) : (6*(1)+1)) ] <- rbind(as.matrix(trades[c(1:counting1[2]),], ncol=6),
                                         matrix(NA, nrow=(max(counting)-(counting1[3])), ncol=6))
      
max(counting)

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
#Say what???

