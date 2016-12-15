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
library(fasttime)
library(data.table)
library(FinAsym)


####Seperating data####
setwd("~/Documents")
alltrades<-read.csv("cdntrades.csv", header = T)
allquotes<-read.csv("cdnquotes.csv", header = T)

BMOt<-alltrades[alltrades$SYM_ROOT=="BMO",]
BNSt<-alltrades[alltrades$SYM_ROOT=="BNS",]
CMt<-alltrades[alltrades$SYM_ROOT=="CM",]
RYt<-alltrades[alltrades$SYM_ROOT=="RY",]
TDt<-alltrades[alltrades$SYM_ROOT=="TD",]
names(BMOt)<-c("DATE", "TIME", "EX", "SYMBOL", "SYM_SUFFIX", "COND", "SIZE", "PRICE")
names(BNSt)<-c("DATE", "TIME", "EX", "SYMBOL", "SYM_SUFFIX", "COND", "SIZE", "PRICE")
names(CMt)<-c("DATE", "TIME", "EX", "SYMBOL", "SYM_SUFFIX", "COND", "SIZE", "PRICE")
names(RYt)<-c("DATE", "TIME", "EX", "SYMBOL", "SYM_SUFFIX", "COND", "SIZE", "PRICE")
names(TDt)<-c("DATE", "TIME", "EX", "SYMBOL", "SYM_SUFFIX", "COND", "SIZE", "PRICE")


BMOq<-allquotes[allquotes$SYM_ROOT=="BMO",]
BNSq<-allquotes[allquotes$SYM_ROOT=="BNS",]
CMq<-allquotes[allquotes$SYM_ROOT=="CM",]
RYq<-allquotes[allquotes$SYM_ROOT=="RY",]
TDq<-allquotes[allquotes$SYM_ROOT=="TD",]

names(BMOq)<-c("DATE", "TIME", "EX", "SYMBOL", "SYM_SUFFIX", "BID", "BIDSIZ", "OFR", "OFRSIZ", "QU_COND")
names(BNSq)<-c("DATE", "TIME", "EX", "SYMBOL", "SYM_SUFFIX", "BID", "BIDSIZ", "OFR", "OFRSIZ", "QU_COND")
names(CMq)<-c("DATE", "TIME", "EX", "SYMBOL", "SYM_SUFFIX", "BID", "BIDSIZ", "OFR", "OFRSIZ", "QU_COND")
names(RYq)<-c("DATE", "TIME", "EX", "SYMBOL", "SYM_SUFFIX", "BID", "BIDSIZ", "OFR", "OFRSIZ", "QU_COND")
names(TDq)<-c("DATE", "TIME", "EX", "SYMBOL", "SYM_SUFFIX", "BID", "BIDSIZ", "OFR", "OFRSIZ", "QU_COND")


head(alltrades)


write.csv(BMOt, file = "BMO_trades.csv", row.names = F, col.names = T)
write.csv(BNSt, file = "BNS_trades.csv", row.names = F, col.names = T)
write.csv(CMt, file = "CM_trades.csv", row.names = F, col.names = T)
write.csv(RYt, file = "RY_trades.csv", row.names = F, col.names = T)
write.csv(TDt, file = "TD_trades.csv", row.names = F, col.names = T)
write.csv(BMOq, file = "BMO_quotes.csv", row.names = F, col.names = T)
write.csv(BNSq, file = "BNS_quotes.csv", row.names = F, col.names = T)
write.csv(CMq, file = "CM_quotes.csv", row.names = F, col.names = T)
write.csv(RYq, file = "RY_quotes.csv", row.names = F, col.names = T)
write.csv(TDq, file = "TD_quotes.csv", row.names = F, col.names = T)


head(allquotes)
tail(alltrades)
head(fox_quotes)
View(fox_quotes)

####BMO Analysis####

from <- "2016-08-19"
to <- "2016-08-31"
ticker1<-"BMO"
datasource1 <- "~/Documents/BMO"
datadestination1 <- "~/Documents/BMOx"
convert(from=from, to=to, datasource=datasource1, 
        datadestination=datadestination1, trades = T,  quotes = F, 
        ticker=ticker1, dir = T, extension = "csv", 
        header = TRUE, tradecolnames = NULL, quotecolnames = NULL, 
        format="%Y%m%d %H:%M:%OS", onefile = TRUE )
convert(from=from, to=to, datasource=datasource1, 
        datadestination=datadestination1, trades = F,  quotes = T, 
        ticker=ticker1, dir = F, extension = "csv", 
        header = TRUE, tradecolnames = NULL, quotecolnames = NULL, 
        format="%Y%m%d %H:%M:%OS", onefile = TRUE )


BMOt = TAQLoad(tickers=ticker1, from="2016-08-19", to="2016-08-31", trades=TRUE,
               quotes=FALSE, datasource= datadestination1)
BMOq = TAQLoad(tickers=ticker1, from="2016-08-19", to="2016-08-31",trades=F,
               quotes=TRUE, datasource=datadestination1)
View(BMOt)
View(BMOq)


BMOt<-exchangeHoursOnly(BMOt)
BMOq<-exchangeHoursOnly(BMOq)
dim(BMOt)
dim(BMOq)

ext1<-as.character(autoSelectExchangeTrades(BMOt)[1,2])
exq1<-as.character(autoSelectExchangeQuotes(BMOq)[1,2])
ext<-"N"
exq<-"Q"

ext==ext1
exq==exq1

View(BMOt)
BMOtc<-BMOt[ext==BMOt$EX ,]
BMOqc<-BMOq[ext==BMOq$EX]


BMOtc<-noZeroPrices(BMOtc)
BMOtc<-salesCondition(BMOtc)
BMOtc<-mergeTradesSameTimestamp(BMOtc)
View(BMOtc)


BMOqc<-noZeroQuotes(BMOqc)
BMOqc<-rmLargeSpread(BMOqc)
BMOqc<-mergeQuotesSameTimestamp(BMOqc)
BMOtc<-tradesCleanupFinal(tdata = BMOtc, qdata = BMOqc)


#### Liquidity Measures ####
BMO_m<-matchTradesQuotes(BMOtc, BMOqc)
BMOtdir<-getTradeDirection(BMO_m)

liqtypes<- c("es", "rs", "value_trade", "signed_value_trade", "signed_trade_size",
             "di_diff", "pes", "prs", "price_impact", "prop_price_impact", "tspread", 
             "pts", "p_return_sqr", "p_return_abs", "qs", "pqs", "logqs", 
             "logsize", "qslope", "logqslope", "mq_return_sqr", "mq_return_abs")

BMO_liq<-matrix(NA, nrow=length(BMOtdir), ncol=length(liqtypes))
View(BMO_liq)

for (i in c(1:length(liqtypes))) {
  BMO_liq[,i]<-tqLiquidity(BMO_m, BMOtc, BMOqc, type = liqtypes[i])
}
View(BMO_liq)

BMO_liq<-data.frame(BMO_liq)
liqtypes2<-c("effective spread", "realized spread", "trade value (price × size)", 
             "signed trade value", "signed trade size", "depth imbalance", 
             "proportional effective spread", "proportional realized spread", 
             "price impact", "proportional price impact", "half traded spread", 
             "proportional half-traded spread", "squared log return on Trade prices", 
             "absolute log return on Trade prices", "quoted spread", 
             "proportional quoted spread", "log quoted spread", "log quoted size", 
             "quoted slope", "log quoted slope", "midquote squared return", 
             "midquote absolute return")
names(BMO_liq)<- liqtypes2
row.names(BMO_liq)<-row.names(data.frame(BMO_m))
View(BMO_liq)
setwd("~/Documents")
write.csv(BMO_liq, file = "BMO_liq.csv", row.names = T, col.names = T)



# Example, liquidity before & after market out of function
before<-as.POSIXlt("2013-08-22 12:19:19")
after<-as.POSIXlt("2013-08-22 15:25:06")
beef<-as.POSIXlt(row.names(liquidity))<before
aftrr<-as.POSIXlt(row.names(liquidity))>after
View(beef)
mean(liquidity[beef, 1])
mean(liquidity[aftrr, 1])
mean(liquidity[beef, 2])
mean(liquidity[aftrr, 2])
#^so, effective spreads are nearly uneffected, but realized are more than doubled

####TD Analysis####

from <- "2016-08-19"
to <- "2016-08-31"
ticker2<-"TD"
datasource2 <- "~/Documents/TD"
datadestination2 <- "~/Documents/TDx"
convert(from=from, to=to, datasource=datasource2, 
        datadestination=datadestination2, trades = T,  quotes = F, 
        ticker=ticker2, dir = T, extension = "csv", 
        header = TRUE, tradecolnames = NULL, quotecolnames = NULL, 
        format="%Y%m%d %H:%M:%OS", onefile = TRUE )
convert(from=from, to=to, datasource=datasource2, 
        datadestination=datadestination2, trades = F,  quotes = T, 
        ticker=ticker2, dir = F, extension = "csv", 
        header = TRUE, tradecolnames = NULL, quotecolnames = NULL, 
        format="%Y%m%d %H:%M:%OS", onefile = TRUE )


TDt = TAQLoad(tickers=ticker2, from="2016-08-19", to="2016-08-31", trades=TRUE,
              quotes=FALSE, datasource= datadestination2)
TDq = TAQLoad(tickers=ticker2, from="2016-08-19", to="2016-08-31",trades=F,
              quotes=TRUE, datasource=datadestination2)

head(TDq)
head(TDt)
head(BMOt)

TDt<-exchangeHoursOnly(TDt)
TDq<-exchangeHoursOnly(TDq)

ext<-"N"
exq<-"Q"


TDtc<-TDt[ext==TDt$EX ,]
TDqc<-TDq[ext==TDq$EX, ]
head(TDtc)
head(TDqc)


TDtc<-noZeroPrices(TDtc)
TDtc<-salesCondition(TDtc)
TDtc<-mergeTradesSameTimestamp(TDtc)



TDqc<-noZeroQuotes(TDqc)
TDqc<-rmLargeSpread(TDqc)
TDqc<-mergeQuotesSameTimestamp(TDqc)
TDtc<-tradesCleanupFinal(tdata = TDtc, qdata = TDqc)


#### Liquidity Measures ####
TD_m<-matchTradesQuotes(TDtc, TDqc)
TDtdir<-getTradeDirection(TD_m)

TD_liq<-matrix(NA, nrow=length(TDtdir), ncol=length(liqtypes))

for (i in c(1:length(liqtypes))) {
  TD_liq[,i]<-tqLiquidity(TD_m, TDtc, TDqc, type = liqtypes[i])
}


TD_liq<-data.frame(TD_liq)
names(TD_liq)<- liqtypes2
row.names(TD_liq)<-row.names(data.frame(TD_m))
View(TD_liq)
setwd("~/Documents")
write.csv(TD_liq, file = "TD_liq.csv", row.names = T, col.names = T)


#### RY Analysis ####
setwd("~/Documents")
from <- "2016-08-19"
to <- "2016-08-31"
ticker3<-"RY"
datasource3 <- "~/Documents/RY"
datadestination3 <- "~/Documents/RYx"
convert(from=from, to=to, datasource=datasource3, 
        datadestination=datadestination3, trades = T,  quotes = F, 
        ticker=ticker3, dir = T, extension = "csv", 
        header = TRUE, tradecolnames = NULL, quotecolnames = NULL, 
        format="%Y%m%d %H:%M:%OS", onefile = TRUE )
convert(from=from, to=to, datasource=datasource3, 
        datadestination=datadestination3, trades = F,  quotes = T, 
        ticker=ticker3, dir = F, extension = "csv", 
        header = TRUE, tradecolnames = NULL, quotecolnames = NULL, 
        format="%Y%m%d %H:%M:%OS", onefile = TRUE )


RYt = TAQLoad(tickers=ticker3, from="2016-08-19", to="2016-08-31", trades=TRUE,
              quotes=FALSE, datasource= datadestination3)
RYq = TAQLoad(tickers=ticker3, from="2016-08-19", to="2016-08-31",trades=F,
              quotes=TRUE, datasource=datadestination3)

RYt<-exchangeHoursOnly(RYt)
RYq<-exchangeHoursOnly(RYq)

ext<-"N"
exq<-"Q"



RYtc<-RYt[ext==RYt$EX,]
RYqc<-if(is.null(TDq[ext==RYq$EX, ])) {
  RYq[exq==RYq$EX, ]
} else {
  RYq[ext==RYq$EX, ]
}

RYtc<-noZeroPrices(RYtc)
RYtc<-salesCondition(RYtc)
RYtc<-mergeTradesSameTimestamp(RYtc)

RYqc<-noZeroQuotes(RYqc)
RYqc<-rmLargeSpread(RYqc)
RYqc<-mergeQuotesSameTimestamp(RYqc)
RYtc<-tradesCleanupFinal(tdata = RYtc, qdata = RYqc)


#### Liquidity Measures ####
RY_m<-matchTradesQuotes(RYtc, RYqc)
RYtdir<-getTradeDirection(RY_m)

RY_liq<-matrix(NA, nrow=length(RYtdir), ncol=length(liqtypes))

for (i in c(1:length(liqtypes))) {
  RY_liq[,i]<-tqLiquidity(RY_m, RYtc, RYqc, type = liqtypes[i])
}


RY_liq<-data.frame(RY_liq)
names(RY_liq)<- liqtypes2
row.names(RY_liq)<-row.names(data.frame(RY_m))
View(RY_liq)
setwd("~/Documents")
write.csv(RY_liq, file = "RY_liq.csv", row.names = T, col.names = T)

#### CM Analysis ####
setwd("~/Documents")
from <- "2016-08-19"
to <- "2016-08-31"
ticker4<-"CM"
datasource4 <- "~/Documents/CM"
datadestination4 <- "~/Documents/CMx"
convert(from=from, to=to, datasource=datasource4, 
        datadestination=datadestination4, trades = T,  quotes = F, 
        ticker=ticker4, dir = T, extension = "csv", 
        header = TRUE, tradecolnames = NULL, quotecolnames = NULL, 
        format="%Y%m%d %H:%M:%OS", onefile = TRUE )
convert(from=from, to=to, datasource=datasource4, 
        datadestination=datadestination4, trades = F,  quotes = T, 
        ticker=ticker4, dir = F, extension = "csv", 
        header = TRUE, tradecolnames = NULL, quotecolnames = NULL, 
        format="%Y%m%d %H:%M:%OS", onefile = TRUE )


CMt = TAQLoad(tickers=ticker4, from="2016-08-19", to="2016-08-31", trades=TRUE,
              quotes=FALSE, datasource= datadestination4)
CMq = TAQLoad(tickers=ticker4, from="2016-08-19", to="2016-08-31",trades=F,
              quotes=TRUE, datasource=datadestination4)

CMt<-exchangeHoursOnly(CMt)
CMq<-exchangeHoursOnly(CMq)

ext<-"N"
exq<-"Q"



CMtc<-CMt[ext==CMt$EX,]
CMqc<-if(is.null(CMq[ext==CMq$EX, ])) {
  CMq[exq==CMq$EX, ]
} else {
  CMq[ext==CMq$EX, ]
}

CMtc<-noZeroPrices(CMtc)
CMtc<-salesCondition(CMtc)
CMtc<-mergeTradesSameTimestamp(CMtc)

CMqc<-noZeroQuotes(CMqc)
CMqc<-rmLargeSpread(CMqc)
CMqc<-mergeQuotesSameTimestamp(CMqc)
CMtc<-tradesCleanupFinal(tdata = CMtc, qdata = CMqc)

#### Liquidity Measures ####
CM_m<-matchTradesQuotes(CMtc, CMqc)
CMtdir<-getTradeDirection(CM_m)

CM_liq<-matrix(NA, nrow=length(CMtdir), ncol=length(liqtypes))

for (i in c(1:length(liqtypes))) {
  CM_liq[,i]<-tqLiquidity(CM_m, CMtc, CMqc, type = liqtypes[i])
}


CM_liq<-data.frame(CM_liq)
names(CM_liq)<- liqtypes2
row.names(CM_liq)<-row.names(data.frame(CM_m))
View(CM_liq)
setwd("~/Documents")
write.csv(CM_liq, file = "CM_liq.csv", row.names = T, col.names = T)

#### BNS Analysis ####
setwd("~/Documents")
from <- "2016-08-19"
to <- "2016-08-31"
ticker5<-"BNS"
datasource5 <- "~/Documents/BNS"
datadestination5 <- "~/Documents/BNSx"
convert(from=from, to=to, datasource=datasource5, 
        datadestination=datadestination5, trades = T,  quotes = F, 
        ticker=ticker5, dir = T, extension = "csv", 
        header = TRUE, tradecolnames = NULL, quotecolnames = NULL, 
        format="%Y%m%d %H:%M:%OS", onefile = TRUE )
convert(from=from, to=to, datasource=datasource5, 
        datadestination=datadestination5, trades = F,  quotes = T, 
        ticker=ticker5, dir = F, extension = "csv", 
        header = TRUE, tradecolnames = NULL, quotecolnames = NULL, 
        format="%Y%m%d %H:%M:%OS", onefile = TRUE )


BNSt = TAQLoad(tickers=ticker5, from="2016-08-19", to="2016-08-31", trades=TRUE,
              quotes=FALSE, datasource= datadestination5)
BNSq = TAQLoad(tickers=ticker5, from="2016-08-19", to="2016-08-31",trades=F,
              quotes=TRUE, datasource=datadestination5)

BNSt<-exchangeHoursOnly(BNSt)
BNSq<-exchangeHoursOnly(BNSq)

ext<-"N"
exq<-"Q"



BNStc<-BNSt[ext==BNSt$EX,]
BNSqc<-if(is.null(BNSq[ext==BNSq$EX, ])) {
  BNSq[exq==BNSq$EX, ]
} else {
  BNSq[ext==BNSq$EX, ]
}

BNStc<-noZeroPrices(BNStc)
BNStc<-salesCondition(BNStc)
BNStc<-mergeTradesSameTimestamp(BNStc)

BNSqc<-noZeroQuotes(BNSqc)
BNSqc<-rmLargeSpread(BNSqc)
BNSqc<-mergeQuotesSameTimestamp(BNSqc)
BNStc<-tradesCleanupFinal(tdata = BNStc, qdata = BNSqc)

#### Liquidity Measures ####
BNS_m<-matchTradesQuotes(BNStc, BNSqc)
BNStdir<-getTradeDirection(BNS_m)

BNS_liq<-matrix(NA, nrow=length(BNStdir), ncol=length(liqtypes))

for (i in c(1:length(liqtypes))) {
  BNS_liq[,i]<-tqLiquidity(BNS_m, BNStc, BNSqc, type = liqtypes[i])
}


BNS_liq<-data.frame(BNS_liq)
names(BNS_liq)<- liqtypes2
row.names(BNS_liq)<-row.names(data.frame(BNS_m))
View(BNS_liq)
setwd("~/Documents")
write.csv(BNS_liq, file = "BNS_liq.csv", row.names = T, col.names = T)


#### VPIN Calculations ####

#Define Vector that computes days data TD

TDqc.df <- data.frame(date=index(TDqc), coredata(TDqc))

TD_udays<-(unique(as.Date(TDqc.df$date)))
TD_days<-c(1:length(TD_udays))
for (i in c(1:length(TD_udays))) {
  TD_days[i]<-sum(as.Date(TDqc.df$date)==TD_udays[i])
}
TD_daysc<-TD_days
for (i in c(2:length(TD_udays))) {
  TD_daysc[i]<-TD_daysc[i]+TD_daysc[i-1]
}
TD_daysc<-c(1,TD_daysc)
TD_daysc

#Define Vector that computes days data RY

RYqc.df <- data.frame(date=index(RYqc), coredata(RYqc))

RY_udays<-(unique(as.Date(RYqc.df$date)))
RY_days<-c(1:length(RY_udays))
for (i in c(1:length(TD_udays))) {
  RY_days[i]<-sum(as.Date(RYqc.df$date)==RY_udays[i])
}
RY_daysc<-RY_days
for (i in c(2:length(RY_udays))) {
  RY_daysc[i]<-RY_daysc[i]+RY_daysc[i-1]
}
RY_daysc<-c(1,RY_daysc)
RY_daysc

#Define Vector that computes days data BMO

BMOqc.df <- data.frame(date=index(BMOqc), coredata(BMOqc))

BMO_udays<-(unique(as.Date(BMOqc.df$date)))
BMO_days<-c(1:length(BMO_udays))
for (i in c(1:length(BMO_udays))) {
  BMO_days[i]<-sum(as.Date(BMOqc.df$date)==BMO_udays[i])
}
BMO_daysc<-BMO_days
for (i in c(2:length(BMO_udays))) {
  BMO_daysc[i]<-BMO_daysc[i]+BMO_daysc[i-1]
}
BMO_daysc<-c(1,BMO_daysc)
BMO_daysc

#Define Vector that computes days data CM

CMqc.df <- data.frame(date=index(CMqc), coredata(CMqc))

CM_udays<-(unique(as.Date(CMqc.df$date)))
CM_days<-c(1:length(CM_udays))
for (i in c(1:length(CM_udays))) {
  CM_days[i]<-sum(as.Date(CMqc.df$date)==CM_udays[i])
}
CM_daysc<-CM_days
for (i in c(2:length(CM_udays))) {
  CM_daysc[i]<-CM_daysc[i]+CM_daysc[i-1]
}
CM_daysc<-c(1,CM_daysc)
CM_daysc

#Define Vector that computes days data BNS

BNSqc.df <- data.frame(date=index(BNSqc), coredata(BNSqc))

BNS_udays<-(unique(as.Date(BNSqc.df$date)))
BNS_days<-c(1:length(BNS_udays))
for (i in c(1:length(BNS_udays))) {
  BNS_days[i]<-sum(as.Date(BNSqc.df$date)==BNS_udays[i])
}
BNS_daysc<-BNS_days
for (i in c(2:length(BNS_udays))) {
  BNS_daysc[i]<-BNS_daysc[i]+BNS_daysc[i-1]
}
BNS_daysc<-c(1,BNS_daysc)
BNS_daysc






#Manipulate Data Frame
TD_date<-as.Date(TDqc.df$date)
TD_time<-strftime(TDqc.df$date, format="%H:%M:%S", tz= "GMT")
TDqc.df<-data.frame(TDqc.df, TD_date, TD_time)
TDqc.df$BID<-as.numeric(levels(TDqc.df$BID))[TDqc.df$BID]
TDqc.df$OFR<-as.numeric(levels(TDqc.df$OFR))[TDqc.df$OFR]

#### GRAPH VECTORS ####


TD_liqdays<-c(1:length(TD_udays))
for (i in c(1:length(TD_udays))) {
  TD_days[i]<-sum(as.Date(TDqc.df$date)==TD_udays[i])
}
TD_daysc<-TD_days
for (i in c(2:length(TD_udays))) {
  TD_daysc[i]<-TD_daysc[i]+TD_daysc[i-1]
}
TD_daysc<-c(1,TD_daysc)
TD_daysc

#Define Vector that computes days data RY

RYqc.df <- data.frame(date=index(RYqc), coredata(RYqc))

RY_udays<-(unique(as.Date(RYqc.df$date)))
RY_days<-c(1:length(RY_udays))
for (i in c(1:length(TD_udays))) {
  RY_days[i]<-sum(as.Date(RYqc.df$date)==RY_udays[i])
}
RY_daysc<-RY_days
for (i in c(2:length(RY_udays))) {
  RY_daysc[i]<-RY_daysc[i]+RY_daysc[i-1]
}
RY_daysc<-c(1,RY_daysc)
RY_daysc


#Define Vector that computes days data BMO

BMOqc.df <- data.frame(date=index(BMOqc), coredata(BMOqc))

BMO_udays<-(unique(as.Date(BMOqc.df$date)))
BMO_days<-c(1:length(BMO_udays))
for (i in c(1:length(BMO_udays))) {
  BMO_days[i]<-sum(as.Date(BMOqc.df$date)==BMO_udays[i])
}
BMO_daysc<-BMO_days
for (i in c(2:length(BMO_udays))) {
  BMO_daysc[i]<-BMO_daysc[i]+BMO_daysc[i-1]
}
BMO_daysc<-c(1,BMO_daysc)
BMO_daysc

#Define Vector that computes days data CM

CMqc.df <- data.frame(date=index(CMqc), coredata(CMqc))

CM_udays<-(unique(as.Date(CMqc.df$date)))
CM_days<-c(1:length(CM_udays))
for (i in c(1:length(CM_udays))) {
  CM_days[i]<-sum(as.Date(CMqc.df$date)==CM_udays[i])
}
CM_daysc<-CM_days
for (i in c(2:length(CM_udays))) {
  CM_daysc[i]<-CM_daysc[i]+CM_daysc[i-1]
}
CM_daysc<-c(1,CM_daysc)
CM_daysc

#Define Vector that computes days data BNS

BNSqc.df <- data.frame(date=index(BNSqc), coredata(BNSqc))

BNS_udays<-(unique(as.Date(BNSqc.df$date)))
BNS_days<-c(1:length(BNS_udays))
for (i in c(1:length(BNS_udays))) {
  BNS_days[i]<-sum(as.Date(BNSqc.df$date)==BNS_udays[i])
}
BNS_daysc<-BNS_days
for (i in c(2:length(BNS_udays))) {
  BNS_daysc[i]<-BNS_daysc[i]+BNS_daysc[i-1]
}
BNS_daysc<-c(1,BNS_daysc)
BNS_daysc

#PIN for TD


a1<-classify_quotes(TDqc.df[c(TD_daysc[1]:TD_daysc[2]), c(4,6)], 1, 2, TD_udays[1])
a2<-classify_quotes(TDqc.df[c(TD_daysc[2]:TD_daysc[3]), c(4,6)], 1, 2, TD_udays[2])
a3<-classify_quotes(TDqc.df[c(TD_daysc[3]:TD_daysc[4]), c(4,6)], 1, 2, TD_udays[3])
a4<-classify_quotes(TDqc.df[c(TD_daysc[4]:TD_daysc[5]), c(4,6)], 1, 2, TD_udays[4])
a5<-classify_quotes(TDqc.df[c(TD_daysc[5]:TD_daysc[6]), c(4,6)], 1, 2, TD_udays[5])
a6<-classify_quotes(TDqc.df[c(TD_daysc[6]:TD_daysc[7]), c(4,6)], 1, 2, TD_udays[6])
a7<-classify_quotes(TDqc.df[c(TD_daysc[7]:TD_daysc[8]), c(4,6)], 1, 2, TD_udays[7])
a8<-classify_quotes(TDqc.df[c(TD_daysc[8]:TD_daysc[9]), c(4,6)], 1, 2, TD_udays[8])
a9<-classify_quotes(TDqc.df[c(TD_daysc[9]:TD_daysc[10]), c(4,6)], 1, 2, TD_udays[9])

AA<-data.frame(c(a1$no_trades, a1$sell_trades, a1$buy_trades), 
               c(a2$no_trades, a2$sell_trades, a2$buy_trades), 
               c(a3$no_trades, a3$sell_trades, a3$buy_trades),
               c(a4$no_trades, a4$sell_trades, a4$buy_trades), 
               c(a5$no_trades, a5$sell_trades, a5$buy_trades),
               c(a6$no_trades, a6$sell_trades, a6$buy_trades), 
               c(a7$no_trades, a7$sell_trades, a7$buy_trades), 
               c(a8$no_trades, a8$sell_trades, a8$buy_trades), 
               c(a9$no_trades, a9$sell_trades, a9$buy_trades))
View(AA)
names(AA)<-TD_udays
row.names(AA)<-c("no_trades", "sell_trades", "buy_trades")
write.csv(AA, file = "PINInfo.csv", row.names = F, col.names = T)

#Make a starting guess at the solution
par0 <- c(0.5, 0.5, 0.5, 0.5)

#Calc optimum parameters
b1<-optim(par0, pin_likelihood, gr=NULL, TDqc.df[c(TD_daysc[1]:TD_daysc[2]), c(4,6)])
b2<-optim(par0, pin_likelihood, gr=NULL, TDqc.df[c(TD_daysc[2]:TD_daysc[3]), c(4,6)])
b3<-optim(par0, pin_likelihood, gr=NULL, TDqc.df[c(TD_daysc[3]:TD_daysc[4]), c(4,6)])
b4<-optim(par0, pin_likelihood, gr=NULL, TDqc.df[c(TD_daysc[4]:TD_daysc[5]), c(4,6)])
b5<-optim(par0, pin_likelihood, gr=NULL, TDqc.df[c(TD_daysc[5]:TD_daysc[6]), c(4,6)])
b6<-optim(par0, pin_likelihood, gr=NULL, TDqc.df[c(TD_daysc[6]:TD_daysc[7]), c(4,6)])
b7<-optim(par0, pin_likelihood, gr=NULL, TDqc.df[c(TD_daysc[7]:TD_daysc[8]), c(4,6)])
b8<-optim(par0, pin_likelihood, gr=NULL, TDqc.df[c(TD_daysc[8]:TD_daysc[9]), c(4,6)])
b9<-optim(par0, pin_likelihood, gr=NULL, TDqc.df[c(TD_daysc[9]:TD_daysc[10]), c(4,6)])

BB<-data.frame(b1$par, b2$par, b3$par, b4$par, b5$par, b6$par, b7$par, b8$par, b9$par)
View(BB)
names(BB)<-TD_udays
write.csv(BB, file = "TD_par.csv", row.names = F, col.names = T)

#PIN for RY

#Manipulate Data Frame
RY_date<-as.Date(RYqc.df$date)
RY_time<-strftime(RYqc.df$date, format="%H:%M:%S", tz= "GMT")
RYqc.df<-data.frame(RYqc.df, RY_date, RY_time)
RYqc.df$BID<-as.numeric(levels(RYqc.df$BID))[RYqc.df$BID]
RYqc.df$OFR<-as.numeric(levels(RYqc.df$OFR))[RYqc.df$OFR]

a11<-classify_quotes(TDqc.df[c(RY_daysc[1]:RY_daysc[2]), c(4,6)], 1, 2, RY_udays[1])
a21<-classify_quotes(TDqc.df[c(RY_daysc[2]:RY_daysc[3]), c(4,6)], 1, 2, RY_udays[2])
a31<-classify_quotes(TDqc.df[c(RY_daysc[3]:RY_daysc[4]), c(4,6)], 1, 2, RY_udays[3])
a41<-classify_quotes(TDqc.df[c(RY_daysc[4]:RY_daysc[5]), c(4,6)], 1, 2, RY_udays[4])
a51<-classify_quotes(TDqc.df[c(RY_daysc[5]:RY_daysc[6]), c(4,6)], 1, 2, RY_udays[5])
a61<-classify_quotes(TDqc.df[c(RY_daysc[6]:RY_daysc[7]), c(4,6)], 1, 2, RY_udays[6])
a71<-classify_quotes(TDqc.df[c(RY_daysc[7]:RY_daysc[8]), c(4,6)], 1, 2, RY_udays[7])
a81<-classify_quotes(TDqc.df[c(RY_daysc[8]:RY_daysc[9]), c(4,6)], 1, 2, RY_udays[8])
a91<-classify_quotes(TDqc.df[c(RY_daysc[9]:RY_daysc[10]), c(4,6)], 1, 2, RY_udays[9])

AA1<-data.frame(c(a11$no_trades, a11$sell_trades, a11$buy_trades), 
               c(a21$no_trades, a21$sell_trades, a21$buy_trades), 
               c(a31$no_trades, a31$sell_trades, a31$buy_trades),
               c(a41$no_trades, a41$sell_trades, a41$buy_trades), 
               c(a51$no_trades, a51$sell_trades, a51$buy_trades),
               c(a61$no_trades, a61$sell_trades, a61$buy_trades), 
               c(a71$no_trades, a71$sell_trades, a71$buy_trades), 
               c(a81$no_trades, a81$sell_trades, a81$buy_trades), 
               c(a91$no_trades, a91$sell_trades, a91$buy_trades))

names(AA1)<-RY_udays
row.names(AA1)<-c("no_trades", "sell_trades", "buy_trades")
write.csv(AA1, file = "RY_PINInfo.csv", row.names = F, col.names = T)

#Make a starting guess at the solution
par0 <- c(0.5, 0.5, 0.5, 0.5)

#Calc optimum parameters
b11<-optim(par0, pin_likelihood, gr=NULL, RYqc.df[c(RY_daysc[1]:RY_daysc[2]), c(4,6)])
b21<-optim(par0, pin_likelihood, gr=NULL, RYqc.df[c(RY_daysc[2]:RY_daysc[3]), c(4,6)])
b31<-optim(par0, pin_likelihood, gr=NULL, RYqc.df[c(RY_daysc[3]:RY_daysc[4]), c(4,6)])
b41<-optim(par0, pin_likelihood, gr=NULL, RYqc.df[c(RY_daysc[4]:RY_daysc[5]), c(4,6)])
b51<-optim(par0, pin_likelihood, gr=NULL, RYqc.df[c(RY_daysc[5]:RY_daysc[6]), c(4,6)])
b61<-optim(par0, pin_likelihood, gr=NULL, RYqc.df[c(RY_daysc[6]:RY_daysc[7]), c(4,6)])
b71<-optim(par0, pin_likelihood, gr=NULL, RYqc.df[c(RY_daysc[7]:RY_daysc[8]), c(4,6)])
b81<-optim(par0, pin_likelihood, gr=NULL, RYqc.df[c(RY_daysc[8]:RY_daysc[9]), c(4,6)])
b91<-optim(par0, pin_likelihood, gr=NULL, RYqc.df[c(RY_daysc[9]:RY_daysc[10]), c(4,6)])

BB1<-data.frame(b11$par, b21$par, b31$par, b41$par, b51$par, b61$par, b71$par, b81$par, b91$par)
names(BB1)<-RY_udays
write.csv(BB1, file = "RY_par.csv", row.names = F, col.names = T)

par0 <- c(0.2, 0.2, 0.2, 0.2)

#Calc optimum parameters

BMO_date<-as.Date(BMOqc.df$date)
BMO_time<-strftime(BMOqc.df$date, format="%H:%M:%S", tz= "GMT")
BMOqc.df<-data.frame(BMOqc.df, BMO_date, BMO_time)
BMOqc.df$BID<-as.numeric(levels(BMOqc.df$BID))[BMOqc.df$BID]
BMOqc.df$OFR<-as.numeric(levels(BMOqc.df$OFR))[BMOqc.df$OFR]

b12<-optim(par0, pin_likelihood, gr=NULL, BMOqc.df[c(BMO_daysc[1]:BMO_daysc[2]), c(4,6)])
b22<-optim(par0, pin_likelihood, gr=NULL, BMOqc.df[c(BMO_daysc[2]:BMO_daysc[3]), c(4,6)])
b32<-optim(par0, pin_likelihood, gr=NULL, BMOqc.df[c(BMO_daysc[3]:BMO_daysc[4]), c(4,6)])
b42<-optim(par0, pin_likelihood, gr=NULL, BMOqc.df[c(BMO_daysc[4]:BMO_daysc[5]), c(4,6)])
b52<-optim(par0, pin_likelihood, gr=NULL, BMOqc.df[c(BMO_daysc[5]:BMO_daysc[6]), c(4,6)])
b62<-optim(par0, pin_likelihood, gr=NULL, BMOqc.df[c(BMO_daysc[6]:BMO_daysc[7]), c(4,6)])
b72<-optim(par0, pin_likelihood, gr=NULL, BMOqc.df[c(BMO_daysc[7]:BMO_daysc[8]), c(4,6)])
b82<-optim(par0, pin_likelihood, gr=NULL, BMOqc.df[c(BMO_daysc[8]:BMO_daysc[9]), c(4,6)])
b92<-optim(par0, pin_likelihood, gr=NULL, BMOqc.df[c(BMO_daysc[9]:BMO_daysc[10]), c(4,6)])

BB2<-data.frame(b12$par, b22$par, b32$par, b42$par, b52$par, b62$par, b72$par, b82$par, b92$par)
names(BB2)<-BMO_udays
write.csv(BB2, file = "BMO_par.csv", row.names = F, col.names = T)

par0 <- c(0.2, 0.2, 0.2, 0.2)

#Calc optimum parameters

CM_date<-as.Date(CMqc.df$date)
CM_time<-strftime(CMqc.df$date, format="%H:%M:%S", tz= "GMT")
CMqc.df<-data.frame(CMqc.df, CM_date, CM_time)
CMqc.df$BID<-as.numeric(levels(CMqc.df$BID))[CMqc.df$BID]
CMqc.df$OFR<-as.numeric(levels(CMqc.df$OFR))[CMqc.df$OFR]

b12<-optim(par0, pin_likelihood, gr=NULL, CMqc.df[c(CM_daysc[1]:CM_daysc[2]), c(4,6)])
b22<-optim(par0, pin_likelihood, gr=NULL, CMqc.df[c(CM_daysc[2]:CM_daysc[3]), c(4,6)])
b32<-optim(par0, pin_likelihood, gr=NULL, CMqc.df[c(CM_daysc[3]:CM_daysc[4]), c(4,6)])
b42<-optim(par0, pin_likelihood, gr=NULL, CMqc.df[c(CM_daysc[4]:CM_daysc[5]), c(4,6)])
b52<-optim(par0, pin_likelihood, gr=NULL, CMqc.df[c(CM_daysc[5]:CM_daysc[6]), c(4,6)])
b62<-optim(par0, pin_likelihood, gr=NULL, CMqc.df[c(CM_daysc[6]:CM_daysc[7]), c(4,6)])
b72<-optim(par0, pin_likelihood, gr=NULL, CMqc.df[c(CM_daysc[7]:CM_daysc[8]), c(4,6)])
b82<-optim(par0, pin_likelihood, gr=NULL, CMqc.df[c(CM_daysc[8]:CM_daysc[9]), c(4,6)])
b92<-optim(par0, pin_likelihood, gr=NULL, CMqc.df[c(CM_daysc[9]:CM_daysc[10]), c(4,6)])

BB2<-data.frame(b12$par, b22$par, b32$par, b42$par, b52$par, b62$par, b72$par, b82$par, b92$par)
names(BB2)<-CM_udays
write.csv(BB2, file = "CM_par.csv", row.names = F, col.names = T)

BNS_date<-as.Date(BNSqc.df$date)
BNS_time<-strftime(BNSqc.df$date, format="%H:%M:%S", tz= "GMT")
BNSqc.df<-data.frame(BNSqc.df, BNS_date, BNS_time)
BNSqc.df$BID<-as.numeric(levels(BNSqc.df$BID))[BNSqc.df$BID]
BNSqc.df$OFR<-as.numeric(levels(BNSqc.df$OFR))[BNSqc.df$OFR]

b12<-optim(par0, pin_likelihood, gr=NULL, BNSqc.df[c(CM_daysc[1]:BNS_daysc[2]), c(4,6)])
b22<-optim(par0, pin_likelihood, gr=NULL, BNSqc.df[c(CM_daysc[2]:BNS_daysc[3]), c(4,6)])
b32<-optim(par0, pin_likelihood, gr=NULL, BNSqc.df[c(CM_daysc[3]:BNS_daysc[4]), c(4,6)])
b42<-optim(par0, pin_likelihood, gr=NULL, BNSqc.df[c(CM_daysc[4]:BNS_daysc[5]), c(4,6)])
b52<-optim(par0, pin_likelihood, gr=NULL, BNSqc.df[c(CM_daysc[5]:BNS_daysc[6]), c(4,6)])
b62<-optim(par0, pin_likelihood, gr=NULL, BNSqc.df[c(CM_daysc[6]:BNS_daysc[7]), c(4,6)])
b72<-optim(par0, pin_likelihood, gr=NULL, BNSqc.df[c(CM_daysc[7]:BNS_daysc[8]), c(4,6)])
b82<-optim(par0, pin_likelihood, gr=NULL, BNSqc.df[c(CM_daysc[8]:BNS_daysc[9]), c(4,6)])
b92<-optim(par0, pin_likelihood, gr=NULL, BNSqc.df[c(CM_daysc[9]:BNS_daysc[10]), c(4,6)])

BB2<-data.frame(b12$par, b22$par, b32$par, b42$par, b52$par, b62$par, b72$par, b82$par, b92$par)
names(BB2)<-BNS_udays
write.csv(BB2, file = "BNS_par.csv", row.names = F, col.names = T)


#VPIN

stock<-as.xts(TDtc[,c(3,4)])
View(stock)
names(stock)<-c("Price", "Volume")

VPIN=function(stock,Vbucket) {
  stock$dP1=diff(stock[,'Price'],lag=1,diff=1,na.pad=TRUE)
  ends=endpoints(stock,'minutes')
  timeDF=period.apply(stock[,'dP1'],INDEX=ends,FUN=sum)
  timeDF$Volume=period.apply(stock[,'Volume'],INDEX=ends,FUN=sum)
  Vbar=mean(period.apply(timeDF[,'Volume'],INDEX=endpoints(timeDF,'days'),
                         FUN=sum))/Vbucket
  timeDF$Vfrac=timeDF[,'Volume']/Vbar
  timeDF$CumVfrac=cumsum(timeDF[,'Vfrac'])
  timeDF$Next=(timeDF[,'CumVfrac']-floor(timeDF[,'CumVfrac']))/timeDF[,'Vfrac']
  timeDF[timeDF[,'Next']<1,'Next']=0
  timeDF$Previous=lag(timeDF[,'dP1'])*lag(timeDF[,'Next'])
  timeDF$dP2=(1-timeDF[,'Next'])*timeDF[,'dP1'] + timeDF[,'Previous']
  timeDF$Vtick=floor(timeDF[,'CumVfrac'])
  timeDF[,'Vtick']=timeDF[,'Vtick']-diff(timeDF[,'Vtick']); timeDF[1,'Vtick']=0
  timeDF=as.data.frame(timeDF); timeDF[,'DateTime']=row.names(timeDF)
  timeDF=ddply(as.data.frame(timeDF),.(Vtick),last)
  timeDF=as.xts(timeDF[,c('Volume','dP2','Vtick')],
                order.by=fastPOSIXct(timeDF$DateTime,tz='GMT'))
  timeDF[1,'dP2']=0
  timeDF$sigma=rollapply(timeDF[,'dP2'],Vbucket,sd,fill=NA)
  timeDF$sigma=na.fill(timeDF$sigma,"extend")
  timeDF$Vbuy=Vbar*pnorm(timeDF[,'dP2']/timeDF[,'sigma'])
  timeDF$Vsell=Vbar-timeDF[,'Vbuy']
  timeDF$OI=abs(timeDF[,'Vsell']-timeDF[,'Vbuy'])
  timeDF$VPIN=rollapply(timeDF[,'OI'],Vbucket,sum)/(Vbar*Vbucket)
  timeDF=timeDF[,c('VPIN')]; return(timeDF)
}

stock$dP1=diff(stock[,'Price'],lag=1,diff=1,na.pad=TRUE)
ends=endpoints(stock,'minutes')
timeDF=period.apply(stock[,'dP1'],INDEX=ends,FUN=sum)
timeDF$Volume=period.apply(stock[,'Volume'],INDEX=ends,FUN=sum)
Vbar=mean(period.apply(timeDF[,'Volume'],INDEX=endpoints(timeDF,'days'),
                       FUN=sum))/Vbucket
timeDF$Vfrac=timeDF[,'Volume']/Vbar
timeDF$CumVfrac=cumsum(timeDF[,'Vfrac'])
timeDF$Next=(timeDF[,'CumVfrac']-floor(timeDF[,'CumVfrac']))/timeDF[,'Vfrac']
timeDF[timeDF[,'Next']<1,'Next']=0
timeDF$Previous=lag(timeDF[,'dP1'])*lag(timeDF[,'Next'])
timeDF$dP2=(1-timeDF[,'Next'])*timeDF[,'dP1'] + timeDF[,'Previous']
timeDF$Vtick=floor(timeDF[,'CumVfrac'])
timeDF[,'Vtick']=timeDF[,'Vtick']-diff(timeDF[,'Vtick']); timeDF[1,'Vtick']=0
timeDF=as.data.frame(timeDF); timeDF[,'DateTime']=row.names(timeDF)
timeDF=ddply(as.data.frame(timeDF),.(Vtick),last)
timeDF=as.xts(timeDF[,c('Volume','dP2','Vtick')],
              order.by=fastPOSIXct(timeDF$DateTime,tz='GMT'))
timeDF[1,'dP2']=0
timeDF$sigma=rollapply(timeDF[,'dP2'],Vbucket,sd,fill=NA)
timeDF$sigma=na.fill(timeDF$sigma,"extend")
timeDF$Vbuy=Vbar*pnorm(timeDF[,'dP2']/timeDF[,'sigma'])
timeDF$Vsell=Vbar-timeDF[,'Vbuy']
timeDF$OI=abs(timeDF[,'Vsell']-timeDF[,'Vbuy'])
timeDF$VPIN=rollapply(timeDF[,'OI'],Vbucket,sum)/(Vbar*Vbucket)
timeDF=timeDF[,c('VPIN')]; return(timeDF)

out=VPIN(stock,50)
View(TDtc)
out=VPIN(TDtc,50)



####Stats####

mean(TD_liq$`realized spread`)

#### Days for Graphs ####
View(TD_liq.df)
TD_liq.df<-data.frame(date=as.Date(row.names(TD_liq)), coredata(TD_liq))
TD_udays<-(unique(as.Date(TDqc.df$date)))
TD_liqdays<-c(1:length(TD_udays))
for (i in c(1:length(TD_udays))) {
  TD_liqdays[i]<-sum(as.Date(TD_liq.df$date)==TD_udays[i])
}
TD_liqdaysc<-TD_liqdays
for (i in c(2:length(TD_liqdays))) {
  TD_liqdaysc[i]<-TD_liqdaysc[i]+TD_liqdaysc[i-1]
}
TD_liqdaysc<-c(1,TD_liqdaysc)
TD_liqdaysc

#Define Vector that computes days data RY

View(RY_liq.df)
RY_liq.df<-data.frame(date=as.Date(row.names(RY_liq)), coredata(RY_liq))
RY_udays<-(unique(as.Date(RYqc.df$date)))
RY_liqdays<-c(1:length(RY_udays))
for (i in c(1:length(RY_udays))) {
  RY_liqdays[i]<-sum(as.Date(RY_liq.df$date)==RY_udays[i])
}
RY_liqdaysc<-RY_liqdays
for (i in c(2:length(RY_liqdays))) {
  RY_liqdaysc[i]<-RY_liqdaysc[i]+RY_liqdaysc[i-1]
}
RY_liqdaysc<-c(1,RY_liqdaysc)
RY_liqdaysc


#Define Vector that computes days data BMO

BMOqc.df <- data.frame(date=index(BMOqc), coredata(BMOqc))

BMO_liq.df<-data.frame(date=as.Date(row.names(BMO_liq)), coredata(BMO_liq))
BMO_udays<-(unique(as.Date(BMOqc.df$date)))
BMO_liqdays<-c(1:length(BMO_udays))
for (i in c(1:length(BMO_udays))) {
  BMO_liqdays[i]<-sum(as.Date(BMO_liq.df$date)==BMO_udays[i])
}
BMO_liqdaysc<-BMO_liqdays
for (i in c(2:length(BMO_liqdays))) {
  BMO_liqdaysc[i]<-BMO_liqdaysc[i]+BMO_liqdaysc[i-1]
}
BMO_liqdaysc<-c(1,BMO_liqdaysc)
BMO_liqdaysc


#Define Vector that computes days data CM

CMqc.df <- data.frame(date=index(CMqc), coredata(CMqc))

CM_liq.df<-data.frame(date=as.Date(row.names(CM_liq)), coredata(CM_liq))
CM_udays<-(unique(as.Date(CMqc.df$date)))
CM_liqdays<-c(1:length(CM_udays))
for (i in c(1:length(CM_udays))) {
  CM_liqdays[i]<-sum(as.Date(CM_liq.df$date)==CM_udays[i])
}
CM_liqdaysc<-CM_liqdays
for (i in c(2:length(CM_liqdays))) {
  CM_liqdaysc[i]<-CM_liqdaysc[i]+CM_liqdaysc[i-1]
}
CM_liqdaysc<-c(1,CM_liqdaysc)
CM_liqdaysc

#Define Vector that computes days data BNS

BNSqc.df <- data.frame(date=index(BNSqc), coredata(BNSqc))

BNS_liq.df<-data.frame(date=as.Date(row.names(BNS_liq)), coredata(BNS_liq))
BNS_udays<-(unique(as.Date(BNSqc.df$date)))
BNS_liqdays<-c(1:length(BNS_udays))
for (i in c(1:length(BNS_udays))) {
  BNS_liqdays[i]<-sum(as.Date(BNS_liq.df$date)==BNS_udays[i])
}
BNS_liqdaysc<-BNS_liqdays
for (i in c(2:length(BNS_liqdays))) {
  BNS_liqdaysc[i]<-BNS_liqdaysc[i]+BNS_liqdaysc[i-1]
}
BNS_liqdaysc<-c(1,BNS_liqdaysc)
BNS_liqdaysc

TD_liqdays<-c(1:length(TD_udays))
for (i in c(1:length(TD_udays))) {
  TD_days[i]<-sum(as.Date(TDqc.df$date)==TD_udays[i])
}
TD_daysc<-TD_days
for (i in c(2:length(TD_udays))) {
  TD_daysc[i]<-TD_daysc[i]+TD_daysc[i-1]
}
TD_daysc<-c(1,TD_daysc)
TD_daysc

#Define Vector that computes days data RY

RYqc.df <- data.frame(date=index(RYqc), coredata(RYqc))

RY_udays<-(unique(as.Date(RYqc.df$date)))
RY_days<-c(1:length(RY_udays))
for (i in c(1:length(TD_udays))) {
  RY_days[i]<-sum(as.Date(RYqc.df$date)==RY_udays[i])
}
RY_daysc<-RY_days
for (i in c(2:length(RY_udays))) {
  RY_daysc[i]<-RY_daysc[i]+RY_daysc[i-1]
}
RY_daysc<-c(1,RY_daysc)
RY_daysc


#Define Vector that computes days data BMO

BMOqc.df <- data.frame(date=index(BMOqc), coredata(BMOqc))

BMO_udays<-(unique(as.Date(BMOqc.df$date)))
BMO_days<-c(1:length(BMO_udays))
for (i in c(1:length(BMO_udays))) {
  BMO_days[i]<-sum(as.Date(BMOqc.df$date)==BMO_udays[i])
}
BMO_daysc<-BMO_days
for (i in c(2:length(BMO_udays))) {
  BMO_daysc[i]<-BMO_daysc[i]+BMO_daysc[i-1]
}
BMO_daysc<-c(1,BMO_daysc)
BMO_daysc

#Define Vector that computes days data CM

CMqc.df <- data.frame(date=index(CMqc), coredata(CMqc))

CM_udays<-(unique(as.Date(CMqc.df$date)))
CM_days<-c(1:length(CM_udays))
for (i in c(1:length(CM_udays))) {
  CM_days[i]<-sum(as.Date(CMqc.df$date)==CM_udays[i])
}
CM_daysc<-CM_days
for (i in c(2:length(CM_udays))) {
  CM_daysc[i]<-CM_daysc[i]+CM_daysc[i-1]
}
CM_daysc<-c(1,CM_daysc)
CM_daysc

#Define Vector that computes days data BNS

BNSqc.df <- data.frame(date=index(BNSqc), coredata(BNSqc))

BNS_udays<-(unique(as.Date(BNSqc.df$date)))
BNS_days<-c(1:length(BNS_udays))
for (i in c(1:length(BNS_udays))) {
  BNS_days[i]<-sum(as.Date(BNSqc.df$date)==BNS_udays[i])
}
BNS_daysc<-BNS_days
for (i in c(2:length(BNS_udays))) {
  BNS_daysc[i]<-BNS_daysc[i]+BNS_daysc[i-1]
}
BNS_daysc<-c(1,BNS_daysc)
BNS_daysc

plot_colours <- c("blue", "red", "forestgreen", "yellow")
plot_colours1 <- plot_colours[c(1,2)]

#### TD ES ####
for (i in c(1:9)) {
  png(filename=paste("~/Documents/ESTD/", paste(substr(as.character(row.names(TD_liq.df)[TD_liqdaysc[i]+1]), 1, 10), ".png", sep = ""), sep=""), width = 1000, height = 600)
  plot(strptime(substr(row.names(TD_liq.df)[c((TD_liqdaysc[i]+1):TD_liqdaysc[i+1])], 12, 19), 
                format = "%H:%M:%S"), 
       TD_liq$`effective spread`[c((TD_liqdaysc[i]+1):TD_liqdaysc[i+1])], 
       type="l", col=plot_colours1[1], ann=FALSE)
  title(main=paste("TD", names(TD_liq)[1], sep = " "), col.main="forestgreen", font.main=3)
  title(xlab=paste("Time on", as.character(as.Date(TD_liq.df$date[TD_liqdaysc[i]+1])), 
                   sep = " "), col.lab=rgb(0,0.6,.7))
  title(ylab=names(TD_liq)[1] , col.lab=rgb(0,0.6,.7))
  dev.off()
}


#### TD RS ####

for (i in c(1:9)) {
  png(filename=paste("~/Documents/RSTD/", paste(substr(as.character(row.names(TD_liq.df)[TD_liqdaysc[i]+1]), 1, 10), ".png", sep = ""), sep=""), width = 1000, height = 600)
  plot(strptime(substr(row.names(TD_liq.df)[c((TD_liqdaysc[i]+1):TD_liqdaysc[i+1])], 12, 19), 
                format = "%H:%M:%S"), 
       TD_liq$`realized spread`[c((TD_liqdaysc[i]+1):TD_liqdaysc[i+1])], 
       type="l", col=plot_colours1[1], ann=FALSE)
  title(main=paste("TD", names(TD_liq)[2], sep = " "), col.main="forestgreen", font.main=3)
  title(xlab=paste("Time on", as.character(as.Date(TD_liq.df$date[TD_liqdaysc[i]+1])), 
                   sep = " "), col.lab=rgb(0,0.6,.7))
  title(ylab=names(TD_liq)[1] , col.lab=rgb(0,0.6,.7))
  dev.off()
}

#### BMO ES ####
for (i in c(1:9)) {
  png(filename=paste("~/Documents/ESBMO/", paste(substr(as.character(row.names(BMO_liq.df)[BMO_liqdaysc[i]+1]), 1, 10), ".png", sep = ""), sep=""), width = 1000, height = 600)
  plot(strptime(substr(row.names(BMO_liq.df)[c((BMO_liqdaysc[i]+1):BMO_liqdaysc[i+1])], 12, 19), 
                format = "%H:%M:%S"), 
       BMO_liq$`effective spread`[c((BMO_liqdaysc[i]+1):BMO_liqdaysc[i+1])], 
       type="l", col=plot_colours1[1], ann=FALSE)
  title(main=paste("BMO", names(BMO_liq)[1], sep = " "), col.main="forestgreen", font.main=3)
  title(xlab=paste("Time on", as.character(as.Date(BMO_liq.df$date[BMO_liqdaysc[i]+1])), 
                   sep = " "), col.lab=rgb(0,0.6,.7))
  title(ylab=names(TD_liq)[1] , col.lab=rgb(0,0.6,.7))
  dev.off()
}


#### BMO RS ####

for (i in c(1:9)) {
  png(filename=paste("~/Documents/RSBMO/", paste(substr(as.character(row.names(BMO_liq.df)[BMO_liqdaysc[i]+1]), 1, 10), ".png", sep = ""), sep=""), width = 1000, height = 600)
  plot(strptime(substr(row.names(BMO_liq.df)[c((BMO_liqdaysc[i]+1):BMO_liqdaysc[i+1])], 12, 19), 
                format = "%H:%M:%S"), 
       BMO_liq$`realized spread`[c((BMO_liqdaysc[i]+1):BMO_liqdaysc[i+1])], 
       type="l", col=plot_colours1[1], ann=FALSE)
  title(main=paste("BMO", names(BMO_liq)[2], sep = " "), col.main="forestgreen", font.main=3)
  title(xlab=paste("Time on", as.character(as.Date(BMO_liq.df$date[BMO_liqdaysc[i]+1])), 
                   sep = " "), col.lab=rgb(0,0.6,.7))
  title(ylab=names(BMO_liq)[1] , col.lab=rgb(0,0.6,.7))
  dev.off()
}

#### RY ES ####
for (i in c(1:9)) {
  png(filename=paste("~/Documents/ESRY/", paste(substr(as.character(row.names(RY_liq.df)[RY_liqdaysc[i]+1]), 1, 10), ".png", sep = ""), sep=""), width = 1000, height = 600)
  plot(strptime(substr(row.names(RY_liq.df)[c((RY_liqdaysc[i]+1):RY_liqdaysc[i+1])], 12, 19), 
                format = "%H:%M:%S"), 
       RY_liq$`effective spread`[c((RY_liqdaysc[i]+1):RY_liqdaysc[i+1])], 
       type="l", col=plot_colours1[1], ann=FALSE)
  title(main=paste("TD", names(RY_liq)[1], sep = " "), col.main="forestgreen", font.main=3)
  title(xlab=paste("Time on", as.character(as.Date(RY_liq.df$date[RY_liqdaysc[i]+1])), 
                   sep = " "), col.lab=rgb(0,0.6,.7))
  title(ylab=names(TD_liq)[1] , col.lab=rgb(0,0.6,.7))
  dev.off()
}


#### RY RS ####

for (i in c(1:9)) {
  png(filename=paste("~/Documents/RSRY/", paste(substr(as.character(row.names(RY_liq.df)[RY_liqdaysc[i]+1]), 1, 10), ".png", sep = ""), sep=""), width = 1000, height = 600)
  plot(strptime(substr(row.names(RY_liq.df)[c((RY_liqdaysc[i]+1):RY_liqdaysc[i+1])], 12, 19), 
                format = "%H:%M:%S"), 
       RY_liq$`realized spread`[c((RY_liqdaysc[i]+1):RY_liqdaysc[i+1])], 
       type="l", col=plot_colours1[1], ann=FALSE)
  title(main=paste("RY", names(TD_liq)[2], sep = " "), col.main="forestgreen", font.main=3)
  title(xlab=paste("Time on", as.character(as.Date(RY_liq.df$date[RY_liqdaysc[i]+1])), 
                   sep = " "), col.lab=rgb(0,0.6,.7))
  title(ylab=names(RY_liq)[1] , col.lab=rgb(0,0.6,.7))
  dev.off()
}

#### BNS ES ####
for (i in c(1:9)) {
  png(filename=paste("~/Documents/ESBNS/", paste(substr(as.character(row.names(BNS_liq.df)[BNS_liqdaysc[i]+1]), 1, 10), ".png", sep = ""), sep=""), width = 1000, height = 600)
  plot(strptime(substr(row.names(BNS_liq.df)[c((BNS_liqdaysc[i]+1):BNS_liqdaysc[i+1])], 12, 19), 
                format = "%H:%M:%S"), 
       TD_liq$`effective spread`[c((BNS_liqdaysc[i]+1):BNS_liqdaysc[i+1])], 
       type="l", col=plot_colours1[1], ann=FALSE)
  title(main=paste("TD", names(BNS_liq)[1], sep = " "), col.main="forestgreen", font.main=3)
  title(xlab=paste("Time on", as.character(as.Date(BNS_liq.df$date[BNS_liqdaysc[i]+1])), 
                   sep = " "), col.lab=rgb(0,0.6,.7))
  title(ylab=names(BNS_liq)[1] , col.lab=rgb(0,0.6,.7))
  dev.off()
}


#### BNS RS ####

for (i in c(1:9)) {
  png(filename=paste("~/Documents/RSBNS/", paste(substr(as.character(row.names(BNS_liq.df)[BNS_liqdaysc[i]+1]), 1, 10), ".png", sep = ""), sep=""), width = 1000, height = 600)
  plot(strptime(substr(row.names(BNS_liq.df)[c((BNS_liqdaysc[i]+1):BNS_liqdaysc[i+1])], 12, 19), 
                format = "%H:%M:%S"), 
       BNS_liq$`realized spread`[c((BNS_liqdaysc[i]+1):BNS_liqdaysc[i+1])], 
       type="l", col=plot_colours1[1], ann=FALSE)
  title(main=paste("TD", names(BNS_liq)[2], sep = " "), col.main="forestgreen", font.main=3)
  title(xlab=paste("Time on", as.character(as.Date(BNS_liq.df$date[BNS_liqdaysc[i]+1])), 
                   sep = " "), col.lab=rgb(0,0.6,.7))
  title(ylab=names(BNS_liq)[1] , col.lab=rgb(0,0.6,.7))
  dev.off()
}

#### CM ES ####
for (i in c(1:9)) {
  png(filename=paste("~/Documents/ESCM/", paste(substr(as.character(row.names(CM_liq.df)[CM_liqdaysc[i]+1]), 1, 10), ".png", sep = ""), sep=""), width = 1000, height = 600)
  plot(strptime(substr(row.names(CM_liq.df)[c((CM_liqdaysc[i]+1):CM_liqdaysc[i+1])], 12, 19), 
                format = "%H:%M:%S"), 
       CM_liq$`effective spread`[c((CM_liqdaysc[i]+1):CM_liqdaysc[i+1])], 
       type="l", col=plot_colours1[1], ann=FALSE)
  title(main=paste("TD", names(CM_liq)[1], sep = " "), col.main="forestgreen", font.main=3)
  title(xlab=paste("Time on", as.character(as.Date(CM_liq.df$date[CM_liqdaysc[i]+1])), 
                   sep = " "), col.lab=rgb(0,0.6,.7))
  title(ylab=names(CM_liq)[1] , col.lab=rgb(0,0.6,.7))
  dev.off()
}


#### CM RS ####

for (i in c(1:9)) {
  png(filename=paste("~/Documents/RSCM/", paste(substr(as.character(row.names(CM_liq.df)[CM_liqdaysc[i]+1]), 1, 10), ".png", sep = ""), sep=""), width = 1000, height = 600)
  plot(strptime(substr(row.names(CM_liq.df)[c((CM_liqdaysc[i]+1):CM_liqdaysc[i+1])], 12, 19), 
                format = "%H:%M:%S"), 
       CM_liq$`realized spread`[c((CM_liqdaysc[i]+1):CM_liqdaysc[i+1])], 
       type="l", col=plot_colours1[1], ann=FALSE)
  title(main=paste("TD", names(TD_liq)[2], sep = " "), col.main="forestgreen", font.main=3)
  title(xlab=paste("Time on", as.character(as.Date(CM_liq.df$date[CM_liqdaysc[i]+1])), 
                   sep = " "), col.lab=rgb(0,0.6,.7))
  title(ylab=names(CM_liq)[1] , col.lab=rgb(0,0.6,.7))
  dev.off()
}

paste(substr(row.names(TD_liq.df)[c(TD_liqdaysc[1]+1]), 12, 19), 
      ".png", sep = "")

plot(as.POSIXct(row.names(TD_liq.df)[c(TD_liqdaysc[2]:TD_liqdaysc[3])]), TD_liq$`effective spread`[c(TD_liqdaysc[1]:TD_liqdaysc[2])], type="l", col=plot_colours1[1], ann=FALSE)
title(main=paste("TD", names(TD_liq)[2], sep = " "), col.main="forestgreen", font.main=3)
title(xlab=paste("Time on", as.character(as.Date(CMqc.df$date[TD_liqdaysc[2]])), sep = " "), col.lab=rgb(0,0.6,.7))
title(ylab=names(TD_liq)[2] , col.lab=rgb(0,0.6,.7))

plot(as.POSIXct(row.names(TD_liq.df)[c(TD_liqdaysc[3]:TD_liqdaysc[4])]), TD_liq$`effective spread`[c(TD_liqdaysc[1]:TD_liqdaysc[2])], type="l", col=plot_colours1[1], ann=FALSE)
title(main=paste("TD", names(TD_liq)[3], sep = " "), col.main="forestgreen", font.main=3)
title(xlab=paste("Time on", as.character(as.Date(CMqc.df$date[TD_liqdaysc[3]])), sep = " "), col.lab=rgb(0,0.6,.7))
title(ylab=names(TD_liq)[3] , col.lab=rgb(0,0.6,.7))

plot(as.POSIXct(row.names(TD_liq.df)[c(TD_liqdaysc[1]:TD_liqdaysc[2])]), TD_liq$`effective spread`[c(TD_liqdaysc[1]:TD_liqdaysc[2])], type="l", col=plot_colours1[1], ann=FALSE)
title(main=paste("TD", names(TD_liq)[1], sep = " "), col.main="forestgreen", font.main=3)
title(xlab=paste("Time on", as.character(as.Date(CMqc.df$date[TD_liqdaysc[1]])), sep = " "), col.lab=rgb(0,0.6,.7))
title(ylab=names(TD_liq)[1] , col.lab=rgb(0,0.6,.7))

plot(as.POSIXct(row.names(TD_liq.df)[c(TD_liqdaysc[1]:TD_liqdaysc[2])]), TD_liq$`effective spread`[c(TD_liqdaysc[1]:TD_liqdaysc[2])], type="l", col=plot_colours1[1], ann=FALSE)
title(main=paste("TD", names(TD_liq)[1], sep = " "), col.main="forestgreen", font.main=3)
title(xlab=paste("Time on", as.character(as.Date(CMqc.df$date[TD_liqdaysc[1]])), sep = " "), col.lab=rgb(0,0.6,.7))
title(ylab=names(TD_liq)[1] , col.lab=rgb(0,0.6,.7))

plot(as.POSIXct(row.names(TD_liq.df)[c(TD_liqdaysc[1]:TD_liqdaysc[2])]), TD_liq$`effective spread`[c(TD_liqdaysc[1]:TD_liqdaysc[2])], type="l", col=plot_colours1[1], ann=FALSE)
title(main=paste("TD", names(TD_liq)[1], sep = " "), col.main="forestgreen", font.main=3)
title(xlab=paste("Time on", as.character(as.Date(CMqc.df$date[TD_liqdaysc[1]])), sep = " "), col.lab=rgb(0,0.6,.7))
title(ylab=names(TD_liq)[1] , col.lab=rgb(0,0.6,.7))

plot(as.POSIXct(row.names(TD_liq.df)[c(TD_liqdaysc[1]:TD_liqdaysc[2])]), TD_liq$`effective spread`[c(TD_liqdaysc[1]:TD_liqdaysc[2])], type="l", col=plot_colours1[1], ann=FALSE)
title(main=paste("TD", names(TD_liq)[1], sep = " "), col.main="forestgreen", font.main=3)
title(xlab=paste("Time on", as.character(as.Date(CMqc.df$date[TD_liqdaysc[1]])), sep = " "), col.lab=rgb(0,0.6,.7))
title(ylab=names(TD_liq)[1] , col.lab=rgb(0,0.6,.7))

plot(as.POSIXct(row.names(TD_liq.df)[c(TD_liqdaysc[1]:TD_liqdaysc[2])]), TD_liq$`effective spread`[c(TD_liqdaysc[1]:TD_liqdaysc[2])], type="l", col=plot_colours1[1], ann=FALSE)
title(main=paste("TD", names(TD_liq)[1], sep = " "), col.main="forestgreen", font.main=3)
title(xlab=paste("Time on", as.character(as.Date(CMqc.df$date[TD_liqdaysc[1]])), sep = " "), col.lab=rgb(0,0.6,.7))
title(ylab=names(TD_liq)[1] , col.lab=rgb(0,0.6,.7))

plot(as.POSIXct(row.names(TD_liq.df)[c(TD_liqdaysc[1]:TD_liqdaysc[2])]), TD_liq$`effective spread`[c(TD_liqdaysc[1]:TD_liqdaysc[2])], type="l", col=plot_colours1[1], ann=FALSE)
title(main=paste("TD", names(TD_liq)[1], sep = " "), col.main="forestgreen", font.main=3)
title(xlab=paste("Time on", as.character(as.Date(CMqc.df$date[TD_liqdaysc[1]])), sep = " "), col.lab=rgb(0,0.6,.7))
title(ylab=names(TD_liq)[1] , col.lab=rgb(0,0.6,.7))




plot(c(1:length(TD_liq$`realized spread`)), TD_liq$`realized spread`, type="l", col=plot_colours1[1], ann=FALSE)
title(main=names(TD_liq)[2], col.main="forestgreen", font.main=3)
title(xlab="Date", col.lab=rgb(0,0.6,.7))
title(ylab=names(TD_liq)[2], col.lab=rgb(0,0.6,.7))

plot(c(1:599), TD_liq$`realized spread`[c(1:599)], type="l", col=plot_colours1[1], ann=FALSE)
title(main=names(TD_liq)[2], col.main="forestgreen", font.main=3)
title(xlab="Date", col.lab=rgb(0,0.6,.7))
title(ylab=names(TD_liq)[2], col.lab=rgb(0,0.6,.7))

plot(c(1:length(TD_liq$`realized spread`)), TD_liq$`proportional effective spread`, type="l", col=plot_colours1[1], ann=FALSE)
title(main=names(TD_liq)[7], col.main="forestgreen", font.main=3)
title(xlab="Date", col.lab=rgb(0,0.6,.7))
title(ylab=names(TD_liq)[7], col.lab=rgb(0,0.6,.7))

x<-rnorm(2000, 0, 1)
for (i in c(1:2000)) {
  x[i]<- 6*pi*i/2000
}
y<-x
for (i in c(1:2000)) {
  y[i]<-(-1)^i*sin(x[i]-2.5)/(x[i]-2.5)*.4
}

par(mfrow=c(1,2))
plot(c(1:599), TD_liq$`realized spread`[c(1:599)], type="l", col=plot_colours1[1], ann=FALSE)
title(main=names(TD_liq)[2], col.main="forestgreen", font.main=3)
title(xlab="Date", col.lab=rgb(0,0.6,.7))
title(ylab=names(TD_liq)[2], col.lab=rgb(0,0.6,.7))
plot(x, y, type="l")
