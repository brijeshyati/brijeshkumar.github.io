#' @title Predicts Stock Price Movement for Given Stock Symbol
#'
#' @description This package predicts whether the stock price at tommorow's market close would be higher or lower compared to today's closing place.
#'
#' @param symbol
#'
#' @return NULL
#'
#' @examples  statwiseprogress('AAPL')
#'
#' @export statwiseprogress
statwiseprogress<-function(symbol)
{
library("anytime")
library("jsonlite")
library("data.table")
library("tidyverse")
library("zoo")
library("ggplot2")
library("gridExtra")
library("grid")

	url1 <- "https://api.covid19india.org/data.json"

	mydata <- fromJSON(url1)

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

  DF2$date <-  as.Date(as.POSIXct(DF2$date ,"%y-%m-%d"))  ### character to date format

  tt <- symbol

  pd1 <- DF2 %>% filter(deltaconfirmed > tt) %>%
    ggplot(aes(x = reorder(state, deltaconfirmed), y = deltaconfirmed)) +
    geom_bar(stat = "identity",aes(fill=state),color="red")+
    coord_flip()+
    geom_text(aes(label=deltaconfirmed),hjust=1) +
    theme(legend.position = "none")+
    labs(### title = "COVID-19 dailywise confirmed count in indian State",
      #### subtitle = paste("confirmed as of", format(max(DF2$date), "%A, %B %e, %Y")),
      x = "statewise", y = "today confirmed count") +
    #### ,caption = "With reference to COVID Tracking Project(covid19india.org)") +
    theme_minimal()


  pd2 <- DF2 %>% filter(deltarecovered > tt) %>%
    ggplot(aes(x = reorder(state, deltarecovered), y = deltarecovered)) +
    geom_bar(stat = "identity",aes(fill=state),color="red")+
    coord_flip()+
    geom_text(aes(label=deltarecovered),hjust=1) +
    theme(legend.position = "none")+
    labs(### title = "COVID-19 dailywise recovered count in indian State",
      #### subtitle = paste("recovered as of", format(max(DF2$date), "%A, %B %e, %Y")),
      x = "statewise", y = "today recovered count") +
    #### ,caption = "With reference to COVID Tracking Project(covid19india.org)") +
    theme_minimal()

  grid.arrange(pd1,pd2,nrow=1,
               top = paste("Covid19 india   ",format(max(DF2$date), "%A, %B %e, %Y"),"(",
                           format(Sys.time()," %H:%M",tz="Asia/Kolkata",usetz=TRUE),")"),
               bottom = textGrob("W.r.t COVID Tracking Project(covid19india.org)",
                                 gp = gpar(fontface = 3, fontsize = 9),hjust = 1,x = 1))
  #Printing results
  print("Probability of Stock price going up tommorow:")
  ### print(pred)
}
