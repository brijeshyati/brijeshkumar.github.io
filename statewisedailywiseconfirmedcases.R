## https://github.com/kjhealy/covdata/
## before that must run "covid19indiajson.R" script for updataed daily data

source('/cloud/project/covid19indiajson/script/covid19indiajson.R')


library("tidyverse")
library("ggplot2")
library("gridExtra")
library("grid")


tt2 <- paste0("/cloud/project/covid19indiajson/input/statewise_", format(Sys.time(), "%Y%m%d"), ".csv")
### write.table(DF2,tt2 ,na = 'NA', sep = ',',row.names = F, col.names = T,quote = TRUE)
covnat <- read.csv(file=tt2, header=TRUE, sep=",",stringsAsFactors = FALSE)
covnat$date <-  as.Date(as.POSIXct(covnat$date ,"%y-%m-%d"))  ### character to date format

names(covnat)
str(covnat)

pd1 <- covnat %>% filter(deltaconfirmed > 0) %>% 
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


pd2 <- covnat %>% filter(deltarecovered > 0) %>% 
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


pd3 <- covnat %>% filter(active > 2000) %>% 
  ggplot(aes(x = reorder(state, active), y = active)) +
  geom_bar(stat = "identity",aes(fill=state),color="red")+
  coord_flip()+
  geom_text(aes(label=active),hjust=1) + 
  theme(legend.position = "none")+
  labs(### title = "COVID-19 dailywise active count gt 2k in indian State",
    #### subtitle = paste("active as of", format(max(covnat$date), "%A, %B %e, %Y")),
       x = "statewise", y = "today active count gt 2000") +
  #### ,caption = "With reference to COVID Tracking Project(covid19india.org)") + 
  theme_minimal()

pd4 <- covnat %>% filter(between(active, 1000, 2000)) %>% 
  ggplot(aes(x = reorder(state, active), y = active)) +
  geom_bar(stat = "identity",aes(fill=state),color="red")+
  coord_flip()+
  geom_text(aes(label=active),hjust=1) + 
  theme(legend.position = "none")+
  labs(### title = "COVID-19 dailywise active count  bt 2k to 1k in indian State",
    #### subtitle = paste("active as of", format(max(covnat$date), "%A, %B %e, %Y")),
       x = "statewise", y = "today active count bt 2k to 1k ") +
  #### ,caption = "With reference to COVID Tracking Project(covid19india.org)") + 
  theme_minimal()

pd5 <- covnat %>% filter(between(active, 500, 1000)) %>% 
  ggplot(aes(x = reorder(state, active), y = active)) +
  geom_bar(stat = "identity",aes(fill=state),color="red")+
  coord_flip()+
  geom_text(aes(label=active),hjust=1) + 
  theme(legend.position = "none")+
  labs(### title = "COVID-19 dailywise active count bt 1k to 500 in indian State",
    #### subtitle = paste("active as of", format(max(covnat$date), "%A, %B %e, %Y")),
       x = "statewise", y = "today active count bt 1k to 500") +
  #### ,caption = "With reference to COVID Tracking Project(covid19india.org)") + 
  theme_minimal()

pd6 <- covnat %>% filter(between(active, 250, 500)) %>% 
  ggplot(aes(x = reorder(state, active), y = active)) +
  geom_bar(stat = "identity",aes(fill=state),color="red")+
  coord_flip()+
  geom_text(aes(label=active),hjust=1) + 
  theme(legend.position = "none")+
  labs(### title = "COVID-19 dailywise active count bt 250 to 500 in indian State",
    #### subtitle = paste("active as of", format(max(covnat$date), "%A, %B %e, %Y")),
       x = "statewise", y = "today active count bt 250 to 500") +
       #### ,caption = "With reference to COVID Tracking Project(covid19india.org)") + 
  theme_minimal()

pd7 <- covnat %>% filter(between(active, 150, 250)) %>% 
  ggplot(aes(x = reorder(state, active), y = active)) +
  geom_bar(stat = "identity",aes(fill=state),color="red")+
  coord_flip()+
  geom_text(aes(label=active),hjust=1) + 
  theme(legend.position = "none")+
  labs(### title = "COVID-19 dailywise active count bt 150 to 250 in indian State",
    #### subtitle = paste("active as of", format(max(covnat$date), "%A, %B %e, %Y")),
       x = "statewise", y = "today active count bt 150 to 250") +
       #### ,caption = "With reference to COVID Tracking Project(covid19india.org)") + 
  theme_minimal()

pd8 <- covnat %>% filter(between(active, 50, 150)) %>% 
  ggplot(aes(x = reorder(state, active), y = active)) +
  geom_bar(stat = "identity",aes(fill=state),color="red")+
  coord_flip()+
  geom_text(aes(label=active),hjust=1) + 
  theme(legend.position = "none")+
  labs(### title = "COVID-19 dailywise active count bt 50 to 150 in indian State",
    #### subtitle = paste("active as of", format(max(covnat$date), "%A, %B %e, %Y")),
    x = "statewise", y = "today active count bt 50 to 150") +
  #### ,caption = "With reference to COVID Tracking Project(covid19india.org)") + 
  theme_minimal()

pd9 <- covnat %>% filter(between(active, 10, 50)) %>% 
  ggplot(aes(x = reorder(state, active), y = active)) +
  geom_bar(stat = "identity",aes(fill=state),color="red")+
  coord_flip()+
  geom_text(aes(label=active),hjust=1) + 
  theme(legend.position = "none")+
  labs(### title = "COVID-19 dailywise active count bt 10 to 50 in indian State",
    #### subtitle = paste("active as of", format(max(covnat$date), "%A, %B %e, %Y")),
    x = "statewise", y = "today active count bt 10 to 50") +
  #### ,caption = "With reference to COVID Tracking Project(covid19india.org)") + 
  theme_minimal()

#### grid.arrange(pd1,pd2,pd3,pd4,pd5,pd6,pd7,pd8,pd9)

date()

grid.arrange(pd3,pd4,pd5,pd6,pd7,pd8,pd9,nrow=4,
             top = paste("Covid19 india", format(max(covnat$date), "%A, %B %e, %Y"),"(",
                         format(Sys.time()," %H:%M",tz="Asia/Kolkata",usetz=TRUE),")"),
             bottom = textGrob("W.r.t COVID Tracking Project(covid19india.org)",
                               gp = gpar(fontface = 3, fontsize = 9),hjust = 1,x = 1))

grid.arrange(pd1,pd2,pd3,pd4,pd5,pd6,pd7,pd8,pd9,
  top = paste("Covid19 india", format(max(covnat$date), "%A, %B %e, %Y"),"(",
              format(Sys.time()," %H:%M",tz="Asia/Kolkata",usetz=TRUE),")"),
  bottom = textGrob("W.r.t COVID Tracking Project(covid19india.org)",
                    gp = gpar(fontface = 3, fontsize = 9),hjust = 1,x = 1))
