
#### I see three packages on CRAN with a fromJSON function (rjson, RJSONIO, and jsonlite)
library("anytime")
library("jsonlite")
library("data.table")
library("tidyverse")
library("zoo")

url1 <- "https://api.covid19india.org/data.json"

mydata <- fromJSON(url1)

names(mydata)
names(mydata$cases_time_series)
names(mydata$statewise)
names(mydata$tested)

DF1 <- mydata[["cases_time_series"]]     #### india cases_time_series
DF1$year <- 2020
DF1$dateyear = paste(DF1$date,DF1$year)
DF1$dateyear <- anydate(DF1$dateyear)


### DF1$date <- as.Date(DF1$date,"%d/%m/%y")
DF1 <- DF1[rev(order(as.Date(DF1$dateyear, format = "%Y-%m-%d"))),]
DF1 <- DF1[,c("dateyear","dailyconfirmed","totalconfirmed","dailydeceased","totaldeceased","dailyrecovered","totalrecovered")]

### tt1 <- paste0("D:\\covid19india\\pkgdevlopment\\miryati\\R\\input\\cases_time_series_", format(Sys.time(), "%Y%m%d"), ".csv")
### write.table(DF1,tt1 ,na = 'NA', sep = ',',row.names = F, col.names = T,quote = TRUE)


DF2 <- mydata[["statewise"]]                               #### india statewise
DF2$date <- as.Date(DF2$lastupdatedtime,"%d/%m/%y")
DF2 <- DF2[rev(order(as.Date(DF2$date, format = "%Y-%m-%d"))),]
DF2 <- DF2[,c("state","statecode","date","confirmed","active","recovered",
              "deaths","deltaconfirmed","deltadeaths","deltarecovered")]
DF2$statecode

### tt2 <- paste0("D:\\covid19india\\pkgdevlopment\\miryati\\R\\input\\statewise_", format(Sys.time(), "%Y%m%d"), ".csv")
### write.table(DF2,tt2 ,na = 'NA', sep = ',',row.names = F, col.names = T,quote = TRUE)


DF3 <- mydata[["tested"]]                               #### india tested
DF3$date <- as.Date(DF3$updatetimestamp,"%d/%m/%y")
DF3 <- DF3[rev(order(as.Date(DF3$date, format = "%Y-%m-%d"))),]
names(DF3)
DF3 <- DF3[,c("date","totalsamplestested")]
setnames(DF3,c("dateyear","totalsamplestested"))
DF3 <- data.table(DF3)
DF3 <- aggregate(totalsamplestested ~ dateyear, data = DF3, max)
####### DF3 <- DF3[rev(order(as.Date(DF3$dateyear, format = "%Y-%m-%d"))),]
DF3$totalsamplestested[DF3$totalsamplestested==""] <- NA
str(DF3)
DF3$totalsamplestested <- na.locf(DF3$totalsamplestested, na.rm=FALSE)
DF3$totalsamplestested <- as.integer(DF3$totalsamplestested)
DF3$Dailysampletested <- ave(DF3$totalsamplestested, FUN=function(x) c(0, diff(x)))
DF3 <- DF3[rev(order(as.Date(DF3$dateyear, format = "%Y-%m-%d"))),]

master <- merge(x=DF1,y=DF3,by.x='dateyear',by.y='dateyear',all=F)
master <- master[rev(order(as.Date(master$dateyear, format = "%Y-%m-%d"))),]

#### tt3 <- paste0("D:\\covid19india\\pkgdevlopment\\miryati\\R\\input\\statewise_8_", format(Sys.time(), "%Y%m%d"), ".csv")
#### write.table(master,tt3 ,na = 'NA', sep = ',',row.names = F, col.names = T,quote = TRUE)

####### str(master)
gc()

##   .rs.restartR()
