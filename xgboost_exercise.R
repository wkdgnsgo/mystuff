library(xgboost)
library(readr)
library(tidyverse)
library(stringr)

dat <- read_csv("Outbreak_240817.csv")

set.seed(1234)
diseaseInfo <- dat[sample(1:nrow(dat)), ]

diseaseInfo_humansRemoved <- diseaseInfo %>%
	select(-starts_with("human"))

diseaseLabels1 <- diseaseInfo %>%
	select(humansAffected) %>% # get the column with the # of humans affected
	is.na() %>% # is it NA?
	magrittr::not() 

head(diseaseInfo$humansAffected)
head(diseaseLabels)


diseaseInfo_numeric <- diseaseInfo_humansRemoved %>%
	select(-Id) %>% # the case id shouldn't contain useful information
	select(-c(longitude, latitude)) %>% # location data is also in country data
	select_if(is.numeric)

str(diseaseInfo_numeric)

region <- model.matrix(~country-1, diseaseInfo)


diseaseInfo_numeric$is_domestic <- str_detect(diseaseInfo$speciesDescription, "domestic")
head(diseaseInfo$speciesDescription, 20)
head(str_match_all(diseaseInfo$speciesDescription, "[a-z]*$"),20)
head(str_replace(string = diseaseInfo$speciesDescription, pattern = "[[:punct:]]", replacement = "") ,20)
######

b <- unique(diseaseInfo$speciesDescription)

b1 <- str_replace_all(b, "(\\([a-z]*\\s?[a-z]*\\))", replacement = "")
b1 <- str_replace_all(string = b1, pattern ="[[:punct:]]", replacement = "")

c <- str_replace_all(b1, "wild(?! boar)|domestic|captive", "")

speciesList1 <- str_extract(string = diseaseInfo$speciesDescription, pattern = paste(c, collapse = "|"))
speciesList1 <- tibble(species =speciesList1)


#####
options(na.action='na.pass') 
species <- model.matrix(~species-1,speciesList)


# add our one-hot encoded variable and convert the dataframe into a matrix
diseaseInfo_numeric <- cbind(diseaseInfo_numeric, region, species)
diseaseInfo_matrix <- data.matrix(diseaseInfo_numeric)

# diseaseInfo_numeric$is_domestic <- str_detect(diseaseInfo$speciesDescription, "domestic")
# 
# speciesList <- diseaseInfo$speciesDescription %>%
# 	str_replace("[[:punct:]]", "") %>% # remove punctuation (some rows have parentheses)
# 	str_extract("[a-z]*$") # extract the least word in each row
# 
# # convert our list into a dataframe...
# speciesList <- tibble(species = speciesList)
# 
# # and convert to a matrix using 1 hot encoding
# options(na.action='na.pass') # don't drop NA values!
# species <- model.matrix(~species-1,speciesList)




odds_to_probs <- function(odds){
	return(exp(odds)/ (1 + exp(odds)))
}
