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

# -1 getting rid of intercept
#model.matrix(~region-1, dat)

diseaseInfo_numeric$is_domestic <- str_detect(diseaseInfo$speciesDescription, "domestic")
head(diseaseInfo$speciesDescription, 20)
head(str_match_all(diseaseInfo$speciesDescription, "[a-z]*$"),20)
head(str_replace(string = diseaseInfo$speciesDescription, pattern = "[[:punct:]]", replacement = "") ,20)
######

b <- unique(diseaseInfo$speciesDescription)

b1 <- str_replace_all(b, "(\\([a-z]*\\s?[a-z]*\\))", replacement = "")
b1 <- str_replace_all(string = b1, pattern ="[[:punct:]]", replacement = "")

c <- str_replace_all(b1, "wild(?! boar)|domestic|captive", "")

speciesList <- str_extract(string = diseaseInfo$speciesDescription, pattern = paste(c, collapse = "|"))
speciesList <- tibble(species = speciesList)


#####
options(na.action='na.pass') 
species <- model.matrix(~species-1,speciesList)


# add our one-hot encoded variable and convert the dataframe into a matrix
diseaseInfo_numeric <- cbind(diseaseInfo_numeric, region, species)
diseaseInfo_matrix <- data.matrix(diseaseInfo_numeric)

training_num <- sample(dim(dat)[1], round(dim(dat)[1] * 0.7), F)

train_data <- diseaseInfo_matrix[training_num, ]
train_label <- diseaseLabels1[training_num, ]

test_data <- diseaseInfo_matrix[-training_num, ]
test_label <- diseaseLabels1[-training_num, ]

dtrain <- xgb.DMatrix(data = train_data, label = train_label)
dtest <- xgb.DMatrix(data = test_data, label = test_label)

model <- xgboost(data = dtrain,
								 nrounds = 2,
								 objective = "binary:logistic")

pred <- predict(model, dtest)

label_pred <- ifelse(pred > 0.5, T, F)

table(label_pred, test_label)

err <- mean(as.numeric(pred > 0.5) != test_label)
print(paste("test-error=", err))


model_tuned <- xgboost(data = dtrain, # the data           
											 max.depth = 3, # the maximum depth of each decision tree
											 nround = 2, # max number of boosting iterations
											 objective = "binary:logistic") # the objective function 

# generate predictions for our held-out testing data
pred <- predict(model_tuned, dtest)

label_pred <- ifelse(pred > 0.5, T, F)

table(label_pred, test_label)

# weight for the rare cases

positive_case <- sum(train_label == T)
negative_case <- sum(train_label == F)

model_tuned2 <- xgboost(data = dtrain,
												max.depth = 3,
												nround = 10,
												early_stopping_rounds = 3,
												objective = "binary:logistic",
												scale_pos_weight = negative_case / positive_case,
												gamma = 1)

pred <- predict(model_tuned2, dtest)

label_pred <- ifelse(pred > 0.5, T, F)

err <- mean(as.numeric(pred > 0.5) != test_label)
print(paste("test-error=", err))

table(label_pred, test_label)


odds_to_probs <- function(odds){
	return(exp(odds)/ (1 + exp(odds)))
}
