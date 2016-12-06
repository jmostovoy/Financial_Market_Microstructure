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


