library("tm")
library("SnowballC")
library(stringr)
library(maps)
library(dplyr)
data("world.cities")

american_cities <- world.cities %>% filter(country.etc == "USA")
american_cities$name <- tolower(american_cities$name)
dat$message <- tolower(dat$message)
dat$message <- str_replace_all(pattern = "[^[:alnum:]]", string = dat$message, " ")
docs <- Corpus(VectorSource(dat$message))
vector <- american_cities$name

cities <- dat$message %>% filter(dat$message %in% vector)
str_match_all(string = dat$message[1:40], vector)
inspect(docs)

docs1 <- Courpus(Vector(Source(matches)))
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
dtm <- TermDocumentMatrix(docs1)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)


matches <- unique(grep(paste(vector,collapse="|"), 
												dat$message, value=TRUE))

for(i in 1:length(matches)){
	matches[i] <- str_conv(matches[i], "UTF-8")
	matches[i] <- str_replace_all(matches[i],"\\s"," ")
}

vec <- vector(length=0)


vec <- str_match_all(string = matches, paste(vector,collapse="|"))

vec <- append(vec, 0)

vec <- na.omit(vec)

vec <- unlist(vec)

d <- as.data.frame(vec)

desc_order_data <- d %>% group_by(vec) %>% 
	summarise(freq = n()) %>% 
	arrange(desc(freq))
saveRDS(desc_order_data, "desc_order_data.rds")
