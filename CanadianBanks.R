install.packages("FinAsym")
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


#### VPIN Calculations ####

#Define Vector that computes unique days & # of days in each interval

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

#Manipulate Data Frame
TD_date<-as.Date(TDqc.df$date)
TD_time<-strftime(TDqc.df$date, format="%H:%M:%S", tz= "GMT")
TDqc.df<-data.frame(TDqc.df, TD_date, TD_time)
TDqc.df$BID<-as.numeric(levels(TDqc.df$BID))[TDqc.df$BID]
TDqc.df$OFR<-as.numeric(levels(TDqc.df$OFR))[TDqc.df$OFR]

#PIN

a1<-classify_quotes(TDqc.df[c(TD_daysc[1]:TD_daysc[2]), c(4,6)], 1, 2, TD_udays[1])
a2<-classify_quotes(TDqc.df[c(TD_daysc[2]:TD_daysc[3]), c(4,6)], 1, 2, TD_udays[2])
a3<-classify_quotes(TDqc.df[c(TD_daysc[3]:TD_daysc[4]), c(4,6)], 1, 2, TD_udays[3])
a4<-classify_quotes(TDqc.df[c(TD_daysc[4]:TD_daysc[5]), c(4,6)], 1, 2, TD_udays[4])
a5<-classify_quotes(TDqc.df[c(TD_daysc[5]:TD_daysc[6]), c(4,6)], 1, 2, TD_udays[5])
a6<-classify_quotes(TDqc.df[c(TD_daysc[6]:TD_daysc[7]), c(4,6)], 1, 2, TD_udays[6])
a7<-classify_quotes(TDqc.df[c(TD_daysc[7]:TD_daysc[8]), c(4,6)], 1, 2, TD_udays[7])
a8<-classify_quotes(TDqc.df[c(TD_daysc[8]:TD_daysc[9]), c(4,6)], 1, 2, TD_udays[8])
a9<-classify_quotes(TDqc.df[c(TD_daysc[9]:TD_daysc[10]), c(4,6)], 1, 2, TD_udays[9])
a1

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


#VPIN


VPIN=function(stock,Vbucket) {
  stock$dP1=diff(stock[,'PRICE'],lag=1,diff=1,na.pad=TRUE)
  ends=endpoints(stock,'minutes')
  timeDF=period.apply(stock[,'dP1'],INDEX=ends,FUN=sum)
  timeDF$Volume=period.apply(stock[,'SIZE'],INDEX=ends,FUN=sum)
  Vbar=mean(period.apply(timeDF[,'SIZE'],INDEX=endpoints(timeDF,'days'),
                         FUN=sum))/Vbucket
  timeDF$Vfrac=timeDF[,'SIZE']/Vbar
  timeDF$CumVfrac=cumsum(timeDF[,'Vfrac'])
  timeDF$Next=(timeDF[,'CumVfrac']-floor(timeDF[,'CumVfrac']))/timeDF[,'Vfrac']
  timeDF[timeDF[,'Next']<1,'Next']=0
  timeDF$Previous=lag(timeDF[,'dP1'])*lag(timeDF[,'Next'])
  timeDF$dP2=(1-timeDF[,'Next'])*timeDF[,'dP1'] + timeDF[,'Previous']
  timeDF$Vtick=floor(timeDF[,'CumVfrac'])
  timeDF[,'Vtick']=timeDF[,'Vtick']-diff(timeDF[,'Vtick']); timeDF[1,'Vtick']=0
  timeDF=as.data.frame(timeDF); timeDF[,'DateTime']=row.names(timeDF)
  timeDF=ddply(as.data.frame(timeDF),.(Vtick),last)
  timeDF=as.xts(timeDF[,c('SIZE','dP2','Vtick')],
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
View(TDtc)
out=VPIN(TDtc,50)



####Stats####

mean(TD_liq$`realized spread`)

#### Plots ####
plot_colours <- c("blue", "red", "forestgreen", "yellow")
plot_colours1 <- plot_colours[c(1,2)]

plot(c(1:599), TD_liq$`effective spread`[c(1:599)], type="l", col=plot_colours1[1], ann=FALSE)
title(main=names(TD_liq)[1], col.main="forestgreen", font.main=3)
title(xlab="Date", col.lab=rgb(0,0.6,.7))
title(ylab=names(TD_liq)[1], col.lab=rgb(0,0.6,.7))

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
