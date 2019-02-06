library(rvest)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
murders_raw <- read_html(url) %>% 
  html_nodes("table")  %>% 
  html_table()
murders_raw

murders_raw <- murders_raw[[2]] %>% 
  setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))
head(murders_raw)

test1 <- str_replace_all(murders_raw$population, ",", "")
test1 <- as.numeric(test1)

test2 <- parse_number(murders_raw$population)
identical(test1,test2)

murders_new <- murders_raw %>% mutate_at(2:5, parse_number)
head(murders_new)
