#' @title Predicts Stock Price Movement for Given Stock Symbol
#'
#' @description This package predicts whether the stock price at tommorow's market close would be higher or lower compared to today's closing place.
#'
#' @param symbol
#'
#' @return NULL
#'
#' @examples  statwiseprogress(10)
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

DF1 <- DF1[rev(order(as.Date(DF1$dateyear, format = "%Y-%m-%d"))),]
DF1 <- DF1[,c("dateyear","dailyconfirmed","totalconfirmed","dailydeceased","totaldeceased","dailyrecovered","totalrecovered")]


}
