library(dslabs)
library(tidyverse)
polls_dates <- polls_us_election_2016$startdate %>% head
class(polls_dates)
polls_us_election_2016 %>% filter(pollster == "Ipsos" & state == "U.S.") %>%
  ggplot(aes(startdate,rawpoll_trump)) + 
  geom_line()

library(lubridate)
set.seed(2)
dates <- sample(polls_us_election_2016$startdate,10) %>% sort
dates
data.frame(date = days(dates),
           month = month(dates, label = TRUE),
           day = day(dates),
           year = year(dates)
           )  

x <- "09/02/10 "
ymd(x)
ydm(x)
mdy(x)
myd(x)
dmy(x)
dym(x)

Sys.time()
now()
now("GMT")
now("EST")
OlsonNames()

now() %>% hour()
now() %>% minute()
now() %>% second()

y <- c("15:25:59")
hms(y)

z <- c("Sep/16/2017 10:15:33")
mdy_hms(z)
