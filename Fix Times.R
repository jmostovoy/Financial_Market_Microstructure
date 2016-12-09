setwd("~/Downloads")
BMO_Aug19<-read.csv("BMO_Aug19.csv", header = T)
View(BMO_Aug19)
example2<-read.csv("example2.csv", header = T)
View(example2)


BMO_timesub<-t(data.frame(lapply(BMO_Aug19$ExchTime, as.character), stringsAsFactors=FALSE))
BMO_timesub<-substr(BMO_timesub, 7, 8)
View(BMO_timesub)
row.names(BMO_timesub)<-c(1:length(BMO_timesub))

BMO_otime<-t(data.frame(lapply(BMO_Aug19$Time, as.character), stringsAsFactors=FALSE))
View(BMO_otime)
row.names(BMO_otime)<-c(1:length(BMO_otime))


ExchTime<-as.character(paste(BMO_otime, BMO_timesub, sep="."))
ExchTime<-data.frame(ExchTime)
names(ExchTime)<-c("ExchTime")
View(ExchTime)
BMO_Aug19$ExchTime<-ExchTime

write.csv(BMO_Aug19, file = "BMO_Aug19.csv", row.names = F, col.names = T)
