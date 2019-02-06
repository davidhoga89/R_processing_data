library(dslabs)
library(tidyverse)
data(reported_heights)
x <- as.numeric(reported_heights$height)
sum(is.na(x))

reported_heights %>% mutate(new_height = as.numeric(height)) %>%
  filter(is.na(new_height)) %>%
  head(n=10)

not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- as.numeric(x) #supressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}
  
problems <- reported_heights %>% 
  filter(not_inches(height)) %>%
  .$height
problems

#strings processing
str_subset(reported_heights$height, "cm")

yes <- c( "180 cm", "70 inches")
no <- c("180","70''")
s <- c(yes, no)
str_detect(s,"cm|inches")

yes2 <- c("2","3","7")
no2 <- c("two", "three", "_")
s2 <- c(yes2, no2)
pattern <- "\\d"
str_detect(s2, pattern)
str_view(s2, pattern)
str_view_all(s2,pattern)

str_view(s,"[56]")
yes3 <- as.character(4:7)
no3 <- as.character(1:3)
s3 <- c(yes3, no3)
str_detect(s3,"[4-7]")

pattern <- "^[4-7]'\\d{1,2}$"
problems %>%
  str_replace("feet|ft|foot","'") %>%
  str_replace("inches|in|''|\"","") %>%
  str_detect(pattern) %>%
  sum

pattern2 <- "^[4-7]'\\s\\d{1,2}\"$"
str_subset(problems,pattern2)

pattern_with_groups <- "^([4-7])'\\s*[,\\.\\s+]\\s*(\\d*)$"
str_subset(problems,pattern_with_groups) %>%
  str_replace(pattern_with_groups,"\\1'\\2") %>% head

#final definitive code:
not_inches_or_cm <- function (x, smallest = 50, tallest = 84){
  inches <- as.numeric(x)#supressWarnings(as.numeric(x))
  ind <- !is.na(inches) &
    ((inches >= smallest & inches <= tallest) | 
       (inches/2.54 >= smallest & inches/2.54 <= tallest))
  !ind
}

problems <- reported_heights %>% 
  filter(not_inches_or_cm(height)) %>%
  .$height
length(problems)

converted <- problems %>%
  str_replace("feet|ft|foot","'") %>% #convert ft symbols into '
  str_replace("inches|in|''|\"","") %>% #remove in symbols
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") ##change format

pattern_final <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
mean(index)

converted[!index]

yes5 <- c("5", "6", "5")
no5 <- c("5'", "5''", "5'4")
s5 <- c(yes5, no5)
str_replace(s5, "^([4-7])$", "\\1'0")

s <- "Hi "
cat(s)
identical(s, "Hi")
str_trim("5 ' 9 ")

s <- c("Five feet eight inches")
str_to_lower(s)

convert_format <- function(s){
  s %>%
    str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
    str_replace_all("inches|in|''|\"|cm|and", "") %>%  #remove inches and other symbols
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>% #change x.y, x,y x y
    str_replace("^([56])'?$", "\\1'0") %>% #add 0 when to 5 or 6
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>% #change european decimal
    str_trim() #remove extra space
}

words_to_numbers <- function(s){
  str_to_lower(s) %>%  
    str_replace_all("zero", "0") %>%
    str_replace_all("one", "1") %>%
    str_replace_all("two", "2") %>%
    str_replace_all("three", "3") %>%
    str_replace_all("four", "4") %>%
    str_replace_all("five", "5") %>%
    str_replace_all("six", "6") %>%
    str_replace_all("seven", "7") %>%
    str_replace_all("eight", "8") %>%
    str_replace_all("nine", "9") %>%
    str_replace_all("ten", "10") %>%
    str_replace_all("eleven", "11")
}

converted <- problems %>% words_to_numbers %>% convert_format
remaining_problems <- converted[not_inches_or_cm(converted)]
pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"
index <- str_detect(remaining_problems, pattern)
remaining_problems[!index]

pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"

smallest <- 50
tallest <- 84
new_heights <- reported_heights %>% 
  mutate(original = height, 
         height = words_to_numbers(height) %>% convert_format()) %>%
  extract(height, c("feet", "inches"), regex = pattern, remove = FALSE) %>% 
  mutate_at(c("height", "feet", "inches"), as.numeric) %>%
  mutate(guess = 12*feet + inches) %>%
  mutate(height = case_when(
    !is.na(height) & between(height, smallest, tallest) ~ height, #inches 
    !is.na(height) & between(height/2.54, smallest, tallest) ~ height/2.54, #centimeters
    !is.na(height) & between(height*100/2.54, smallest, tallest) ~ height*100/2.54, #meters
    !is.na(guess) & inches < 12 & between(guess, smallest, tallest) ~ guess, #feet'inches
    TRUE ~ as.numeric(NA))) %>%
  select(-guess)

new_heights %>%
  filter(not_inches(original)) %>%
  select(original, height) %>% 
  arrange(height) %>%
  View()

new_heights %>% arrange(height) %>% head(n=7)
