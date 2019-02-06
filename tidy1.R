library(tidyverse)
library(dslabs)
data("gapminder")
tidy_data <- gapminder %>%
  filter (country %in% c("South Korea", "Germany")) %>% 
  select (country, year, fertility)
head(tidy_data)

tidy_data %>%
  ggplot(aes(year,fertility, color = country)) + 
  geom_point()

#wide data
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
wide_data

#gather
new_tidy <- wide_data %>% gather(year, fertility, '1960':'2015')
new_tidy <- wide_data %>% gather(year, fertility, -country)
head(new_tidy)

class(new_tidy$year)
class(tidy_data$year)

new_tidy <- wide_data %>% gather(year, fertility, -country, convert = TRUE)
class(new_tidy$year)

new_tidy %>%
  ggplot(aes(year,fertility, color = country)) + 
  geom_point()

#spread
new_tidy <- new_tidy %>% spread(year, fertility)
head(new_tidy)

#separate & unite
filename2 <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")
raw_dat <- read_csv(filename2)
head(raw_dat)

dat <- raw_dat %>% gather(key, value, -country)
head(dat)
dat %>% separate (key, c("year","variable_type"), "_")
dat %>% separate (key, c("year","variable_type")) #same result as _ is by default

dat %>% separate (key, c("year","variable_type1","variable_type2")) #same result as _ is by default
dat_final <- dat %>% separate (key, c("year","variable_type1"), sep = "_",  extra= "merge") %>%
  spread(variable_type1, value)
head(dat_final)
