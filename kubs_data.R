library(stringr)
library(maps)
library(dplyr)

# I scraped data by myself from a Facebook page
data("world.cities")

american_cities <- world.cities %>% filter(country.etc == "USA")
american_cities$name <- tolower(american_cities$name)
dat <- readRDS("ucla_ride_share.rds")
dat$message <- str_replace_all(pattern = "[^[:alnum:]]", string = dat$message, " ")
dat$message <- tolower(dat$message)

vector <- american_cities$name


vec <- vector(length=0)


vec <- str_match_all(string = dat$message, paste(vector,collapse="|"))


vec <- unlist(vec)

d <- as.data.frame(vec)

desc_order_data <- d %>% group_by(vec) %>% 
	summarise(freq = n()) %>% 
	arrange(desc(freq))
