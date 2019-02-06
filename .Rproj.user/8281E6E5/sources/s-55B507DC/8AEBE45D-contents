library(rvest)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)
class(h)
h

tab <- h %>% html_nodes("table")
tab <- tab[[2]]
tab <- tab %>% html_table
class(tab)


#For the guacamole recipe page we already have done this and determined that we need the following selectors:
  
r <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
recipe <- r %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
prep_time <- r %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
ingredients <- r %>% html_nodes(".o-Ingredients") %>% html_text()

#You can see how complex the selectors are. In any case we are now ready to extract what we want and create a list:
  
guacamole <- list(recipe, prep_time, ingredients)
guacamole

#Since recipe pages from this website follow this general layout, we can use this code to create a function that extracts this information:
  
  get_recipe <- function(url){
    m <- read_html(url)
    recipe <- m %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
    prep_time <- m %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
    ingredients <- m %>% html_nodes(".o-Ingredients") %>% html_text()
    return(list(recipe = recipe, prep_time = prep_time, ingredients = ingredients))
  }

#and then use it on any of their webpages:
  
get_recipe("http://www.foodnetwork.com/recipes/food-network-kitchen/pancakes-recipe-1913844")
get_recipe("https://www.foodnetwork.com/recipes/food-network-kitchen/spaghetti-squash-carbonara-4590186")  
