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

  source('D:\\covid19india\\pkgdevlopment\\miryati\\R\\covid19indiajson.R')


  library("tidyverse")
  library("ggplot2")
  library("gridExtra")
  library("grid")


  tt2 <- paste0("D:\\covid19india\\pkgdevlopment\\miryati\\R\\input\\statewise_", format(Sys.time(), "%Y%m%d"), ".csv")
  ### write.table(DF2,tt2 ,na = 'NA', sep = ',',row.names = F, col.names = T,quote = TRUE)
  covnat <- read.csv(file=tt2, header=TRUE, sep=",",stringsAsFactors = FALSE)
  covnat$date <-  as.Date(as.POSIXct(covnat$date ,"%y-%m-%d"))  ### character to date format

  names(covnat)
  str(covnat)

  tt <- symbol

  pd1 <- covnat %>% filter(deltaconfirmed > tt) %>%
    ggplot(aes(x = reorder(state, deltaconfirmed), y = deltaconfirmed)) +
    geom_bar(stat = "identity",aes(fill=state),color="red")+
    coord_flip()+
    geom_text(aes(label=deltaconfirmed),hjust=1) +
    theme(legend.position = "none")+
    labs(### title = "COVID-19 dailywise confirmed count in indian State",
      #### subtitle = paste("confirmed as of", format(max(covnat$date), "%A, %B %e, %Y")),
      x = "statewise", y = "today confirmed count") +
    #### ,caption = "With reference to COVID Tracking Project(covid19india.org)") +
    theme_minimal()


  pd2 <- covnat %>% filter(deltarecovered > tt) %>%
    ggplot(aes(x = reorder(state, deltarecovered), y = deltarecovered)) +
    geom_bar(stat = "identity",aes(fill=state),color="red")+
    coord_flip()+
    geom_text(aes(label=deltarecovered),hjust=1) +
    theme(legend.position = "none")+
    labs(### title = "COVID-19 dailywise recovered count in indian State",
      #### subtitle = paste("recovered as of", format(max(covnat$date), "%A, %B %e, %Y")),
      x = "statewise", y = "today recovered count") +
    #### ,caption = "With reference to COVID Tracking Project(covid19india.org)") +
    theme_minimal()

  grid.arrange(pd1,pd2,nrow=1,
               top = paste("Covid19 india   ",format(max(covnat$date), "%A, %B %e, %Y"),"(",
                           format(Sys.time()," %H:%M",tz="Asia/Kolkata",usetz=TRUE),")"),
               bottom = textGrob("W.r.t COVID Tracking Project(covid19india.org)",
                                 gp = gpar(fontface = 3, fontsize = 9),hjust = 1,x = 1))
  #Printing results
  print("Probability of Stock price going up tommorow:")
  ### print(pred)
}
