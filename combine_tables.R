data(murders)
head(murders)

data(polls_us_election_2016)
head(results_us_election_2016)

tab <- left_join(murders, results_us_election_2016, by = "state")
tab %>% ggplot(aes(population/10^6,electoral_votes,label = abb)) + 
  geom_point() + 
  geom_text(check_overlap = TRUE) +
  scale_x_continuous(trans = "log2") + 
  scale_y_continuous(trans = "log2") +
  geom_smooth(method = "lm", se = FALSE)

tab1 <- slice(murders, 1:6) %>% select(state, population)
results_us_election_2016 <- results_us_election_2016[order(results_us_election_2016$state),]
head(results_us_election_2016)
tab2 <- slice(results_us_election_2016, c(1:3, 5, 7:8)) %>% 
  select(state, electoral_votes)

left_join(tab1, tab2) #data from tab1
tab1 %>% left_join(tab2) #same result different methods

tab1 %>% right_join(tab2) #data from tab2

inner_join(tab1,tab2) #intersection from both tables (no NAs)

full_join(tab1,tab2) #union (all data)

semi_join(tab1,tab2) #keeps part of the first table which we have info in the second

anti_join(tab1,tab2) #keeps elements of the first table which there is no info in the second

#BINDING
tab1 <- tab[, 1:3]
tab2 <- tab[, 4:6]
tab3 <- tab[, 7:9]
new_tab <- bind_cols(tab1, tab2, tab3)
head(new_tab)

tab4 <- tab[1:2,]
tab5 <- tab[3:4,]
bind_rows(tab4,tab5)

#SET OPERATORS
tab6 <- tab[1:5,]
tab7 <- tab[3:7,]
intersect(tab6,tab7)
union(tab6,tab7)
setdiff(tab6,tab7)
setdiff(tab7,tab6)
setequal(tab6,tab7)
