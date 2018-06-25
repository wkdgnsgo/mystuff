Data Mining Project
================
Chang Hun
6/25/2018

Introduction
============

Often, we faced sad stories that some people died or got injured by police shooting. If we can predict the fatality in police shooting situation, we are able to reduce rate of dying people. In this project, I am given part of datasets of police shootings and I will predict fatality for the rest of data sets.

``` r
police <- read.csv("Police train.csv")

colnames(police)
```

    ##  [1] "id"               "Date"             "NumberOfSubjects"
    ##  [4] "Fatal"            "SubjectArmed"     "SubjectRace"     
    ##  [7] "SubjectGender"    "SubjectAge"       "NatureOfStop"    
    ## [10] "NumberOfShots"    "NumberOfOfficers" "OfficerRace"     
    ## [13] "OfficerGender"    "Department"       "FullNarrative"   
    ## [16] "City"             "Notes"

``` r
summary(police)
```

    ##        id                Date      NumberOfSubjects Fatal    SubjectArmed
    ##  Min.   :   2982   7/2/1905:  80   Min.   :1        F: 928   N   : 839   
    ##  1st Qu.:1291768   7/4/1905:  63   1st Qu.:1        N:1883   U   : 643   
    ##  Median :2560276   7/5/1905:  63   Median :1        U: 189   Y   :1119   
    ##  Mean   :2524990   7/3/1905:  56   Mean   :1                 NA's: 399   
    ##  3rd Qu.:3764138   7/7/1905:  56   3rd Qu.:1                             
    ##  Max.   :4998309   (Other) :1150   Max.   :1                             
    ##                    NA's    :1532                                         
    ##  SubjectRace SubjectGender   SubjectAge              NatureOfStop 
    ##  A   :  25   F  :  77      U      :1596   Call For Service :  83  
    ##  B   :1159   M  :1968      20-29  : 104   Crime            :  46  
    ##  L   : 459   N/A:   2      23     :  56   Robbery          :  43  
    ##  O   :  11   U  : 953      26     :  56   Call for Service :  36  
    ##  U   : 886                 21     :  51   Suspicious person:  33  
    ##  W   : 459                 31     :  51   (Other)          : 830  
    ##  NA's:   1                 (Other):1086   NA's             :1929  
    ##    NumberOfShots  NumberOfOfficers  OfficerRace  OfficerGender 
    ##  1        : 185   Min.   : 0.00    U      :830   M      :1201  
    ##  2        : 102   1st Qu.: 1.00    W      :618   U      : 996  
    ##  3        :  69   Median : 1.00    B      :221   M;M    : 361  
    ##  4        :  51   Mean   : 1.72    H      :152   M;M;M  : 115  
    ##  not clear:  47   3rd Qu.: 2.00    W;W    :143   F      :  69  
    ##  (Other)  : 397   Max.   :15.00    (Other):720   M;M;M;M:  40  
    ##  NA's     :2149   NA's   :371      NA's   :316   (Other): 218  
    ##                           Department  
    ##  Chicago Police Department     : 353  
    ##  Los Angeles Police Department : 227  
    ##  Houston Police Department     : 213  
    ##  Philadelphia Police Department: 196  
    ##  New York Police Department    : 187  
    ##  Phoenix Police Department     : 108  
    ##  (Other)                       :1716  
    ##                                                                                                                                                                                                              FullNarrative 
    ##  NO HITS                                                                                                                                                                                                            :   8  
    ##  No hits                                                                                                                                                                                                            :   5  
    ##  The armed suspects were leaving a business that they had just robbed when they were engaged by the officers. The suspects refused to drop their weapons forcing the officers to shoot.                             :   5  
    ##  An HPD officer was chasing a stolen vehicle, suspect evaded, and one of the suspects pointed a rifle at the officer.  The officer, in fear of his life, shot and missed the suspect.  Suspect arrested and charged.:   4  
    ##  An off duty robbery officer shot at a car burglar at 0400 in Ft. Bend County.  Multiple suspects were apprehended by Fort Bend County Sheriff's Deputies.  There were no injuries.                                 :   4  
    ##  (Other)                                                                                                                                                                                                            :1340  
    ##  NA's                                                                                                                                                                                                               :1634  
    ##            City                              Notes     
    ##  Chicago     : 354   Firearm Discharge - No Hits: 115  
    ##  LosAngeles  : 227   NO HITS                    :  97  
    ##  Houston     : 213   Hit                        :  88  
    ##  Philadelphia: 196   No hits                    :  50  
    ##  New York    : 187   No hits.                   :  40  
    ##  Phoenix     : 108   (Other)                    :1266  
    ##  (Other)     :1715   NA's                       :1344

Sentiment Analysis
==================

First of all, to predict fatality, I have to define the situation whether it is critical or non-critical.

Sentiment Analysis will be helpful to define the situation.

``` r
library(glmnet)
library(dplyr)
library(text2vec)
library(magrittr)
library(stringr)
library(tm)

FN_sentiments <- read.csv("Police train.csv")
FN_sentiments <- select(FN_sentiments, c(id, Fatal, FullNarrative))

FN_sentiments <- FN_sentiments[!is.na(FN_sentiments$FullNarrative), ]
FN_sentiments <- FN_sentiments[FN_sentiments$Fatal != "U", ]
FN_sentiments$Fatal <- as.character(FN_sentiments$Fatal)

FN_sentiments$Fatal[FN_sentiments$Fatal == "N"] <- "0"
FN_sentiments$Fatal[FN_sentiments$Fatal == "F"] <- "1"
FN_sentiments$Fatal <- as.factor(as.numeric(FN_sentiments$Fatal))
FN_sentiments$id <- as.character(FN_sentiments$id)

FN_sentiments$FullNarrative <- as.character(FN_sentiments$FullNarrative)
FN_sentiments$FullNarrative <- tolower(FN_sentiments$FullNarrative)

# I dont need digits to analyze
FN_sentiments$FullNarrative <- str_replace_all(FN_sentiments$FullNarrative, pattern = "[[:digit:]]", replacement = "")

# it should be stemmized to count words properly
FN_sentiments$FullNarrative <- stemDocument(FN_sentiments$FullNarrative)


# tokenize for sentiment analysis.
prep_fun = tolower
tok_fun = word_tokenizer


it_train = itoken(FN_sentiments$FullNarrative, 
                                    preprocessor = prep_fun, 
                                    tokenizer = tok_fun, 
                                    ids = FN_sentiments$id,
                                    progressbar = FALSE)

# I will remove the common words in English.

stop_words = c(stopwords(), letters, "th")

# I will go bigram because non fatal and fatal are different.

vocab = create_vocabulary(it_train, ngram = c(1L, 2L), stopwords = stop_words)

vocab = prune_vocabulary(vocab, term_count_min = 10, 
                                                 doc_proportion_max = 0.5)
bigram_vectorizer = vocab_vectorizer(vocab)


dtm_train = create_dtm(it_train, bigram_vectorizer)


NFOLDS = 10

# by using logistic, I can predict the situation.
glmnet_classifier = cv.glmnet(x =dtm_train, y = FN_sentiments$Fatal, 
                                                            family = 'binomial', 
                                                            alpha = 1,
                                                            type.measure = "class",
                                                            nfolds = NFOLDS,
                                                            thresh = 1e-3,
                                                            maxit = 1e3)


preds = predict(glmnet_classifier, dtm_train, type = "class")

# Compare between prediction and actual 
table(preds, FN_sentiments$Fatal)
```

    ##      
    ## preds   0   1
    ##     0 832 174
    ##     1  28 311

I did same procedure for Nature of stops and notes. And then make new variable situation by looking these classification.

``` r
data["situation"] <- NA

for ( i in 1:dim(data)[1]) {
    
    if(sum(data[i, c("FN_situation", "NT_situation", "NS_situation")] == "C") >= 1)
        data$situation[i] = "C"
    else if(data$FN_situation[i] == "N" && data$NT_situation[i] == "U")
        data$situation[i] = "N"
    else if(sum(data[i, c("FN_situation", "NT_situation", "NS_situation")] == "U") == 1 && 
                    sum(data[i, c("FN_situation", "NT_situation", "NS_situation")] == "N") == 2)
        data$situation[i] = "N"
    else if(data$FN_situation[i] == "U" && data$NT_situation[i] == "U")
        data$situation[i] = "U"
    else if(sum(data[i, c("FN_situation", "NT_situation", "NS_situation")] == "N") >= 2)
        data$situation[i] = "N"
}

# I noticed that "No hits" in Notes leads to Non-fatal. I will adjust manually to increase the accuracy.

data$Notes <- tolower(data$Notes)

ind <- str_match_all(data$Notes, "no hit|no hits")

ind2 <- numeric()
j = 1
for( i in 1:length(ind)) {
    if(length(ind[[i]]) >= 1 && !is.na(ind[[i]])) {
        ind2[j] <- i
        j = j +1
    }
}
data$situation[ind2] <- "N"

ind <- str_match_all(data$Notes, "(?<!no) hit|(?<!no) hits|^hit$|^hits$")

ind2 <- numeric()
j = 1
for( i in 1:length(ind)) {
    if(length(ind[[i]]) >= 1 && !is.na(ind[[i]])) {
        ind2[j] <- i
        j = j +1
    }
}

data$situation[ind2] <- "C"
data$situation[is.na(data$situation)] <- "U"
data %>% select(c(Fatal, situation)) %>% group_by(situation, Fatal) %>% summarise(cont = n()) %>% 
    mutate(prop = cont / sum(cont))
```

    ## # A tibble: 9 x 4
    ## # Groups:   situation [3]
    ##   situation  Fatal  cont      prop
    ##       <chr> <fctr> <int>     <dbl>
    ## 1         C      F   397 0.5468320
    ## 2         C      N   102 0.1404959
    ## 3         C     NA   227 0.3126722
    ## 4         N      F   230 0.1101533
    ## 5         N      N  1183 0.5665709
    ## 6         N     NA   675 0.3232759
    ## 7         U      F   301 0.1897856
    ## 8         U      N   787 0.4962169
    ## 9         U     NA   498 0.3139975

Modeling
========

To predict the model, I will use random Forest.

Cross Validation.
=================

``` r
train <- sample(1:3000, 3000 * 0.7, replace = F) 
training <- data[train,]
testing <- data[-train, ]
for( i in 1:8){
    if( i == 1 ) 
        print(paste(i,"st run"))
    else if( i == 2 )
        print(paste(i,"nd run"))
    else print(paste(i,"th run"))
    police_tree=randomForest(Fatal~., data=training, mtry=i,
                                                     importance =TRUE)
    Fatal2 <- predict(police_tree ,newdata=testing)
    print((table(Fatal2, testing$Fatal)[1,1] + table(Fatal2, testing$Fatal)[2,2])/dim(testing)[1])
}
```

    ## [1] "1 st run"
    ## [1] 0.7733333
    ## [1] "2 nd run"
    ## [1] 0.7711111
    ## [1] "3 th run"
    ## [1] 0.7588889
    ## [1] "4 th run"
    ## [1] 0.7555556
    ## [1] "5 th run"
    ## [1] 0.7511111
    ## [1] "6 th run"
    ## [1] 0.75
    ## [1] "7 th run"
    ## [1] 0.7388889
    ## [1] "8 th run"
    ## [1] 0.74

Generally, when mtry = 2, it gives the maximum accuracy.

``` r
train <- sample(1:3000, 3000 * 0.7, replace = F) 

training <- data[train, ]
testing <- data[-train, ]

police_tree=randomForest(Fatal~., data=training, mtry=2,
                                                         importance =TRUE)
Fatal2 <- predict(police_tree ,newdata=testing)
table(Fatal2, testing$Fatal)
```

    ##       
    ## Fatal2   F   N
    ##      F 142  42
    ##      N 136 580

``` r
print((table(Fatal2, testing$Fatal)[1,1] + table(Fatal2, testing$Fatal)[2,2])/dim(testing)[1])
```

    ## [1] 0.8022222
