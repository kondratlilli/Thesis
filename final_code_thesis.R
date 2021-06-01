#Code for the Master Thesis of Lili Kondrat

#Loading packages
library(quanteda)
library(e1071)
library(ggplot2)
library(dplyr)
library(SparseM)
library(writexl)
library(tidyverse)
library(spacyr)
set.seed(42)
library(readxl)
library(quanteda.textmodels)
library(caret)
library(openxlsx)
library(lubridate)
library(scales)
library(forecast)
library(smooth)
library(Mcomp)
library(wordcloud)
library(wordcloud2)
library(TTR)
library(AER)
library(dynlm)
library(stargazer)
library(quantmod)
library(urca)
library(MASS)

#Predefined functions
accuracy <- function(ypred, y){
  tab <- table(ypred, y)
  return(sum(diag(tab))/sum(tab))
}

accuracy_reduced <- function(ypred, y){
  tab <- table(ypred, y)
  return((tab[1,1]+tab[3,3])/(tab[1,1]+tab[1,3]+tab[3,1]+tab[3,3]))
}

precision <- function(ypred, y){
  tab <- table(ypred, y)
  if (length(tab)==9) {
    prec_1 = (tab[1,1])/(tab[1,1]+tab[2,1]+tab[3,1])
    prec_2 = (tab[2,2])/(tab[2,2]+tab[1,2]+tab[3,2])
    prec_3 = (tab[3,3])/(tab[3,3]+tab[1,3]+tab[2,3])
    return((prec_1+prec_2+prec_3)/3)
  }
  else {
    return((tab[2,2])/(tab[2,1]+tab[2,2]))}
}

precision_reduced <- function(ypred, y){
  tab <- table(ypred, y)
  prec_red_1 <-(tab[3,3])/(tab[3,3]+tab[3,1])
  prec_red_2 <-(tab[1,1])/(tab[1,1]+tab[1,3])
  return((prec_red_1+ prec_red_2)/2)
}

recall <- function(ypred, y){
  tab <- table(ypred, y)
  if (length(tab)==9) {
    rec_1 = (tab[1,1])/(tab[1,1]+tab[1,2]+tab[1,3])
    rec_2 = (tab[2,2])/(tab[2,2]+tab[2,1]+tab[2,3])
    rec_3 = (tab[3,3])/(tab[3,3]+tab[3,1]+tab[3,2])
    return((rec_1+rec_2+rec_3)/3)
    }
  else {
    return(tab[2,2]/(tab[1,2]+tab[2,2]))}  
}

recall_reduced <- function(ypred, y){
  tab <- table(ypred, y)
  rec_red_1 <- (tab[3,3]/(tab[3,3]+tab[1,3]))
  rec_red_2 <- (tab[1,1]/(tab[1,1]+tab[3,1]))
  return((rec_red_1+rec_red_2)/2)
}

F1 <- function(ypred, y){
  return(2*precision(ypred, y)*recall(ypred, y)/(precision(ypred, y)+recall(ypred, y)))
}

F1_reduced <- function(ypred, y){
  return(2*precision_reduced(ypred, y)*recall_reduced(ypred, y)/(precision_reduced(ypred, y)+recall_reduced(ypred, y)))
}


############################## LOAD DATA ####################################

#Data is available upon request.
#total_set <- read.csv('')
#training_set <- read.csv(')
#test_set <- read.csv('',row.names = 1)
test_set$training <- 0
total_set <- rbind(training_set,test_set)

#The training set is complemented by a COVID specific training set here:
#total_set_extended <- read.csv('')
#training_set_extended <- read.csv('')

############################ Creating subsets to validate our model on ##############################

set.seed(42) 

#In this set the whole training set is used to fit the model, and the 2020 nov data is used as test set
#november_test <- read_excel(")
november_test$training <- 0
colnames(november_test)[9] <- "text_body"
colnames(november_test)[13] <- "full_text"

modell_ertekeles2 <-rbind(training_set,november_test)

############################Descriptive statistics of the data##############################

total_corpus <- corpus(total_set,text_field="full_text")

table(total_corpus$source)

summary(total_corpus) %>%
  summarise(
    mean_wordcount = mean(Tokens), 
    std_dev = sd(Tokens), 
    min_wordc = min(Tokens), 
    max_wordc = max(Tokens)
  )

############################Preprocessing steps##############################

additional_stopwords <- list("mti","mintegy","afp","hu","iratkozz","elküldjük","guardian","getty","images","via",
                             "kiemelt","kép","százal*","forint*","magyarország*","cég","milliárd","millió","vállalat*",
                             "cég*","magyar","ország","nap*","hónap*","adat*","rész","címlapkép","száma","elmúlt",
                             "közölte","forintos","forintot","fontos","továbbra","idei","esetében","program",
                             "jelent","héten","jövő","áll","összesen","es","január*","február*","március*","április*",
                             "május*","június*","július*","augusztus*","szeptember*","október*","november*","december*",
                             "-os","ra","re","érdemes","követően","következő*","-kal","azaz","írja","fő")
#Creating a stopword list

swords <- append(scan("stopwords-hu.txt", what="", sep="\n") ,
                 additional_stopwords)

#Tokenization
### #for the whole dataset
total_tokens <- tokens(total_corpus, remove_punct = T, remove_symbols = T, remove_numbers = T, remove_separators = T) 
total_tokens <- tokens_tolower(total_tokens)
total_tokens <- tokens_select(total_tokens,pattern = swords, selection = "remove")

#A dfm is created before stemming for the purposes of the dictionary-based sentiment
articles_dfm <- dfm(total_tokens)

#Continuing the preprocessing
total_tokens <- tokens_wordstem(total_tokens,language = 'hu')

#We have 11.5B elements
total_dfm <- dfm(total_tokens)
#topfeatures(total_dfm, 50)
#We have 1.4 B elements after trimming.
total_dfm <- dfm_trim(total_dfm, min_docfreq = 15, verbose=TRUE)
#Removing features occurring: - in fewer than 15 documents: 203,831
#Total features removed: 203,831 (88.0%).

textplot_wordcloud(total_dfm, max_words = 100,color = c("lightcyan4","black", "azure4"),scale=c(3,.5),rot.per=0)

##### training and test set separately

training_corpus <- corpus(training_set,text_field="full_text")
training_tokens <- tokens(training_corpus, remove_punct = T, remove_symbols = T, remove_numbers = T, remove_separators = T) 
training_tokens <- tokens_tolower(training_tokens)
training_tokens <- tokens_select(training_tokens,pattern = swords, selection = "remove")
training_tokens <- tokens_wordstem(training_tokens,language = 'hu')
training_dfm <- dfm(training_tokens)
training_dfm <- dfm_trim(training_dfm, min_docfreq = 15, verbose=TRUE)

test_corpus <- corpus(test_set,text_field="full_text")
test_tokens <- tokens(test_corpus, remove_punct = T, remove_symbols = T, remove_numbers = T, remove_separators = T) 
test_tokens <- tokens_tolower(test_tokens)
test_tokens <- tokens_select(test_tokens,pattern = swords, selection = "remove")
test_tokens <- tokens_wordstem(test_tokens,language = 'hu')
test_dfm <- dfm(test_tokens)
test_dfm <- dfm_trim(test_dfm, min_docfreq = 15, verbose=TRUE)

### #for the evaluation set: training set (0.8-0.2 split)
modell_corpus <- corpus(modell_ertekeles,text_field="full_text")
modell_tokens <- tokens(modell_corpus, remove_punct = T, remove_symbols = T, remove_numbers = T, remove_separators = T) 
modell_tokens <- tokens_tolower(modell_tokens)
modell_tokens <- tokens_select(modell_tokens,pattern = swords, selection = "remove")
modell_tokens <- tokens_wordstem(modell_tokens,language = 'hu')

modell_dfm <- dfm(modell_tokens)
#We have 10M tokens after trimming
modell_dfm <- dfm_trim(modell_dfm, min_docfreq = 15, verbose=TRUE)

### #for the second evaluation set: 2020 november

modell2_corpus <- corpus(modell_ertekeles2,text_field="full_text")
modell2_tokens <- tokens(modell2_corpus, remove_punct = T, remove_symbols = T, remove_numbers = T, remove_separators = T) 
modell2_tokens <- tokens_tolower(modell2_tokens)
modell2_tokens <- tokens_select(modell2_tokens,pattern = swords, selection = "remove")

modell2_tokens <- tokens_wordstem(modell2_tokens,language = 'hu')

modell2_dfm <- dfm(modell2_tokens)
#We have 14M tokens after trimming
modell2_dfm <- dfm_trim(modell2_dfm, min_docfreq = 15, verbose=TRUE)

############################ Dictionary-based sentiment analysis ##############################

positive <- read.csv("positive_wordlist_hu.csv",header= TRUE)
negative <- read.csv("negative_wordlist_hu.csv",header= TRUE)

positive=list(positive)
negative=list(negative)

szentiment_szotar <- dictionary(list(pos = positive, neg = negative))

###### Running the sentiment analysis
art_szentiment <- dfm_lookup(articles_tf_dfm, dictionary = szentiment_szotar)

#Storing the values
docvars(total_corpus, "pos") <- as.numeric(art_szentiment[, 1])
docvars(total_corpus, "neg") <- as.numeric(art_szentiment[, 2])

#Converting the results to a dataframe
articles_dict_df <- convert(total_corpus, to = "data.frame")

#Aggregating to monthly level
monthly_pred_dict  <- articles_dict_df %>%
  group_by(year,month) %>%
  summarise(a_sum=sum(as.numeric(pred)),a_count=n(),monthly_sent=a_sum/a_count)

#Creating a date variable
monthly_pred_dict$date <- make_date(monthly_pred$year,monthly_pred$month)

#Normalizing the result
monthly_pred_dict$szent_rescaled <- scale(monthly_pred_dict$monthly_sent)

############################ Fitting model and evaluating it (on the split train set) ##############################

####SVM

SVMmodel <- svm(
  x=modell_dfm[modell_dfm@docvars$modell_ertekeles==1,], 
  y =modell_dfm[modell_dfm@docvars$modell_ertekeles==1,]@docvars$kezi_besorolas,
  kernel="linear", 
  cost  =0.1, 
  probability=TRUE
)

predictions <- predict(SVMmodel, modell_dfm[modell_dfm@docvars$modell_ertekeles==0,])
modell_predictions <- cbind(modell_ertekeles[modell_dfm@docvars$modell_ertekeles==0,], predictions)

predictions_label <- ifelse(predictions > 0.6, 1, ifelse(predictions < -0.6, -1,0))
modell_predictions <- cbind(modell_predictions, predictions_label)

table(modell_predictions$predictions_label, modell_predictions$kezi_besorolas)

#Results checked on a random subset of the training set (cutoff: 0.6)

#   -1  0  1
#-1 62 28 23
#0  78 91 56
#1  34 38 62

#Tokenized data, cutoff at 0.6

#Different cutoff: 0.7
modell_predictions = subset(modell_predictions, select = -c(predictions_label) )
predictions_label <- ifelse(predictions > 0.7, 1, ifelse(predictions < -0.7, -1,0))
modell_predictions <- cbind(modell_predictions, predictions_label)

table(modell_predictions$predictions_label, modell_predictions$kezi_besorolas)
#Results checked on a random subset of the training set (cutoff: 0.7)
#   -1  0  1
#-1 52 31 19
#0  93 88 68
#1  29 38 54

#Different cutoff: 0.4
modell_predictions = subset(modell_predictions, select = -c(predictions_label) )
predictions_label <- ifelse(predictions > 0.4, 1, ifelse(predictions < -0.4, -1,0))
modell_predictions <- cbind(modell_predictions, predictions_label)

table(modell_predictions$predictions_label, modell_predictions$kezi_besorolas)
#Results checked on a random subset of the training set (cutoff: 0.4)
#   -1  0  1
#-1 77 44 30
#0  52 60 43
#1  45 53 68


############################ Fitting model (on the train set) and evaluating it (on 2020 nov. observations) ##############################

####SVM

SVMmodel <- svm(
  x=modell2_dfm[modell2_dfm@docvars$training==1,], 
  y =modell2_dfm[modell2_dfm@docvars$training==1,]@docvars$kezi_besorolas,
  kernel="linear", 
  cost  =0.1, 
  probability=TRUE
)

predictions2 <- predict(SVMmodel, modell2_dfm[modell2_dfm@docvars$training==0,])
modell2_predictions <- cbind(modell_ertekeles2[modell2_dfm@docvars$training==0,], predictions2)

#Cutoff: 0.4

predictions2_label <- ifelse(predictions2 > 0.4, 1, ifelse(predictions2 < -0.4, -1,0))
modell2_predictions <- cbind(modell2_predictions, predictions2_label)
table(modell2_predictions$predictions2_label, modell2_predictions$kezi_besorolas)

#    -1   0   1
#-1  43  42  32
#0   79  61  71
#1   39  46 107

#Different cutoff: 0.6

modell2_predictions = subset(modell2_predictions, select = -c(predictions2_label) )

predictions2_label <- ifelse(predictions2 > 0.6, 1, ifelse(predictions2 < -0.6, -1,0))
modell2_predictions <- cbind(modell2_predictions, predictions2_label)
table(modell2_predictions$predictions2_label, modell2_predictions$kezi_besorolas)

#   -1  0  1
#-1 36 32 26
#0  95 85 91
#1  30 32 93

#Different cutoff: 0.7

modell2_predictions = subset(modell2_predictions, select = -c(predictions2_label) )

predictions2_label <- ifelse(predictions2 > 0.7, 1, ifelse(predictions2 < -0.7, -1,0))
modell2_predictions <- cbind(modell2_predictions, predictions2_label)
table(modell2_predictions$predictions2_label, modell2_predictions$kezi_besorolas)

#    -1   0   1
#-1  31  32  22
#0  108  88 101
#1   22  29  87

######################## NAIVE BAYES ###########################

training_dfm <- modell2_dfm[modell2_dfm@docvars$training==1,]
november_dfm <- modell2_dfm[modell2_dfm@docvars$training==0,]

class_nb <- textmodel_nb(training_dfm, training_dfm$kezi_besorolas)
dfm_matched <- dfm_match(november_dfm, features = featnames(training_dfm))

actual_class <- dfm_matched$kezi_besorolas
predicted_class <- predict(class_nb, newdata = dfm_matched)
tab_class <- table(actual_class, predicted_class)
tab_class

#             predicted_class
#actual_class    -1   0   1
#             -1  92  33  36
#             0   51  48  50
#             1   43  36 131


######################## Random Forest ###########################
#LONG RUNNING TIME
#library(randomForest)

#X <- as.matrix(modell_dfm)
#Y <- as.matrix(training_dfm)
#Z <- as.matrix(november_dfm)

#rf <- randomForest(x=Y, 
#                   y=factor(training_dfm$kezi_besorolas),
#                   xtest=Z,
#                   ytest=factor(november_dfm$kezi_besorolas),
#                   importance=TRUE,
#                   mtry=20,
#                   ntree=100
#)

#rf

####Based on the results I will proceed with the Naive Bayes, but also test the results with the Random Forest

################ CLASSIFY THE REAL TEST SET WITH THE CHOSEN MODEL ##############

training_dfm <- total_dfm[total_dfm@docvars$training==1,]
test_dfm <- total_dfm[total_dfm@docvars$training==0,]

test_set <- total_set[total_set$training==0,]

class_nb <- textmodel_nb(training_dfm, training_dfm$kezi_besorolas)
dfm_matched <- dfm_match(test_dfm, features = featnames(training_dfm))

predicted_class <- predict(class_nb, newdata = dfm_matched)

modell_predictions <- cbind(test_set,predicted_class )
training_set$predicted_class <- training_set$kezi_besorolas

modell_predictions <- rbind (modell_predictions,training_set)

##################Starting a new session, loading the data

modell_predictions <- read.csv('')
#The results of the Random Forest were better by manual validation, so the analysis is conducted with that series.
traintest_randomforest <- read.csv('')

#### Aggregating, plotting

#Monthly resolution

monthly_pred  <- traintest_randomforest %>%
  group_by(year,month) %>%
  summarise(a_sum=sum(as.numeric(pred)),a_count=n(),monthly_sent=a_sum/a_count)

#Creating a date variable
monthly_pred$date <- make_date(monthly_pred$year,monthly_pred$month)

#Loading the Consumer Confidence Index
cci <- read.csv('consumer_confidence_index.csv')
cci$date <- as.Date(paste(cci$TIME,"-01",sep=""))
cci$value_sztend <- scale(cci$Value)

#Merging the time series into one dataframe, creating moving averages
monthly_pred$cci <- cci$value_sztend
monthly_pred$monthly_sent_sztend <- scale(monthly_pred$monthly_sent)
monthly_pred$sent_sma <-SMA(monthly_pred$monthly_sent_sztend,n=6)

#Appending the dictionary-based results to the dataframe
monthly_pred$sent_dict <- monthly_pred_dict$szent_rescaled
monthly_pred$sent_dict_sma <-SMA(monthly_pred$sent_dict,n=6)

#Plot 0.: CCI before scaling
p0 <- ggplot(cci, aes(x = date)) +
  geom_line(aes(y=100),color="gray66") +
  geom_line(aes(y=Value,colour='CCI')) +
  ggtitle("Consumer Confidence Index, 2010-2020") +
  xlab("Date") + ylab("Monthly sentiment") +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  scale_color_manual(values = c(
    'CCI' = 'black')) +
  labs(color = 'Series names')
p0 <- p0 +theme_bw() +theme(legend.position="none") + theme(legend.title = element_blank())

#Plot 1.: Two Sentiment Indices (dictionary and supervised)
p1 <- ggplot(monthly_pred, aes(x = date)) +
  geom_line(aes(y=sent_dict_sma,colour='News sentiment (dictionary)',linetype='News sentiment (dictionary)')) +
  geom_line(aes(y=sent_sma,colour='News sentiment (supervised)',linetype='News sentiment (supervised)')) +
  ggtitle("News Sentiment: dictionary-based and supervised ML-based, moving average (n=6), 2010-2020") +
  xlab("Date") + ylab("Monthly sentiment") +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  scale_color_manual(values = c(
    'News sentiment (dictionary)' = 'grey17','News sentiment (supervised)'= 'grey16')) +
  labs(color = 'Series names') +
  scale_linetype_manual('Series names',values=c('News sentiment (dictionary)'=3,'News sentiment (supervised)'=2))
p1 <- p1 +theme_bw() +theme(legend.position="top") + theme(legend.title = element_blank())

#Plot 2.: CCI + News Sentiment (dictionary) moving average
p2 <- ggplot(monthly_pred, aes(x = date)) +
  geom_line(aes(y=sent_dict_sma,colour='News sentiment (dictionary)',linetype='News sentiment (dictionary)')) +
  geom_line(aes(y=cci,colour='CCI',linetype='CCI'))+
  ggtitle("CCI and the two Sentiment Indices, moving average (n=6), 2010-2020") +
  xlab("Date") + ylab("Monthly sentiment") +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  scale_color_manual(values = c(
  'News sentiment (dictionary)' = 'grey17','CCI'='black'))+
  labs(color = 'Series names')  +
  scale_linetype_manual('Series names',values=c('News sentiment (dictionary)'=3,"CCI"=1))

p2 <- p2 +theme_bw() +theme(legend.position="top") + theme(legend.title = element_blank())

#Plot 3.: CCI + News Sentiment (supervised) moving average
p3 <- ggplot(monthly_pred, aes(x = date)) +
  geom_line(aes(y=sent_sma,colour='News sentiment (supervised)',linetype='News sentiment (supervised)')) +
  geom_line(aes(y=cci,colour='CCI',linetype='CCI'))+
  ggtitle("CCI and the two Sentiment Indices, moving average (n=6), 2010-2020") +
  xlab("Date") + ylab("Monthly sentiment") +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  scale_color_manual(values = c(
    'News sentiment (supervised)'= 'grey17','CCI'='black')) +
  labs(color = 'Series names') +
  scale_linetype_manual('Series names',values=c('News sentiment (supervised)'=2,"CCI"=1))
p3 <- p3 +theme_bw() +theme(legend.position="top") + theme(legend.title = element_blank())

p3_2 <- ggplot(monthly_pred, aes(x = date)) +
  geom_line(aes(y=monthly_sent_sztend,colour='News sentiment (supervised)',linetype='News sentiment (supervised)')) +
  geom_line(aes(y=cci,colour='CCI',linetype='CCI'))+
  ggtitle("CCI and the two Sentiment Indices, moving average (n=6), 2010-2020") +
  xlab("Date") + ylab("Monthly sentiment") +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  scale_color_manual(values = c('News sentiment (supervised)'= 'grey17','CCI'='black')) +
  labs(color = 'Series names') +
  scale_linetype_manual('Series names',values=c('News sentiment (supervised)'=2,'CCI'=1))
p3_2 <- p3_2 +theme_bw() +theme(legend.position="top") + theme(legend.title = element_blank())

### Model
  
#Dickey-Fuller test for the level time series
library(tseries)
adf_1<- adf.test(monthly_pred$monthly_sent_sztend)
adf_2<- adf.test(monthly_pred$cci)
adf_1


szent_ts<- ts(monthly_pred$monthly_sent_sztend, 
                   start = c(2010, 1), 
                   end = c(2020, 12), 
                   frequency = 12)

cci_ts<- ts(monthly_pred$cci, 
                 start = c(2010, 1), 
                 end = c(2020, 12), 
                 frequency = 12)


# test if term spread is stationairy (cointegration of interest rates) using ADF
ur.df(window(szent_ts, c(2010, 1), c(2020, 12)) - window(cci_ts, c(2010, 1), c(2020 ,12)), 
      lags = 2, 
      selectlags = "BIC", 
      type = "drift")

#Calculating lags

monthly_pred$sent_lags <- as.numeric(lag(monthly_pred$monthly_sent_sztend))
monthly_pred$cci_lags <- as.numeric(lag(monthly_pred$cci))
monthly_pred$sent_dict_lags <- as.numeric(lag(monthly_pred$sent_dict))

#Calculating the first differences

monthly_pred = monthly_pred  %>% mutate(sent_diff = monthly_sent_sztend-sent_lags)
monthly_pred = monthly_pred  %>% mutate(cci_diff = cci-cci_lags)
monthly_pred = monthly_pred  %>% mutate(sent_dict_diff = sent_dict-sent_dict_lags)

#Dickey-Fuller test for the first difference time series
adf_3<- adf.test(monthly_pred$sent_diff[2:132])
adf_4<- adf.test(monthly_pred$cci_diff[2:132])
adf_5<- adf.test(monthly_pred$sent_dict_diff[2:132])

#plotting the stationary time series

#Plot 4.: CCI + News Sentiment (supervised) first differences
p4 <- ggplot(monthly_pred, aes(x = date)) +
  geom_line(aes(y=sent_diff,colour='News sentiment (supervised, firstdiff)',linetype='News sentiment (supervised, firstdiff)')) +
  geom_line(aes(y=cci_diff,colour='CCI (firstdiff)',linetype='CCI (firstdiff)'))+
  ggtitle("CCI and Sentiment Index (supervised) - first differences") +
  xlab("Date") + ylab("Monthly sentiment") +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  scale_color_manual(values = c(
    'News sentiment (supervised, firstdiff)'= 'grey17','CCI (firstdiff)'='black')) +
  labs(color = 'Series names') +
  scale_linetype_manual('Series names',values=c('News sentiment (supervised, firstdiff)'=2,'CCI (firstdiff)'=1))
p4 <- p4 +theme_bw() +theme(legend.position="top") + theme(legend.title = element_blank())

#Plot 5.: CCI + News Sentiment (dictionary) first differences
p5 <- ggplot(monthly_pred, aes(x = date)) +
  geom_line(aes(y=sent_dict_diff,colour='News sentiment (dictionary, firstdiff)',linetype='News sentiment (dictionary, firstdiff)')) +
  geom_line(aes(y=cci_diff,colour='CCI (firstdiff)',linetype='CCI (firstdiff)'))+
  ggtitle("CCI and Sentiment Index (dictionary) - first differences") +
  xlab("Date") + ylab("Monthly sentiment") +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year") +
  scale_color_manual(values = c(
    'News sentiment (dictionary, firstdiff)'= 'grey17','CCI (firstdiff)'='black')) +
  labs(color = 'Series names') +
  scale_linetype_manual('Series names',values=c('News sentiment (dictionary, firstdiff)'=3,'CCI (firstdiff)'=1))
p5 <- p5 +theme_bw() +theme(legend.position="top") + theme(legend.title = element_blank())

#Converting to time series format

szent_diff_ts<- ts(monthly_pred$sent_diff, 
                   start = c(2010, 2), 
                   end = c(2020, 12), 
                   frequency = 12)

cci_diff_ts<- ts(monthly_pred$cci_diff, 
                   start = c(2010, 2), 
                   end = c(2020, 12), 
                   frequency = 12)

szent_dict_diff_ts<- ts(monthly_pred$sent_dict_diff, 
                   start = c(2010, 3), 
                   end = c(2020, 12), 
                   frequency = 12)


##### Decide about lag length by the Bayes Information Criterion

# compute BIC for AR model objects of class 'dynlm'

BIC <- function(model) {
  
  ssr <- sum(model$residuals^2)
  t <- length(model$residuals)
  npar <- length(model$coef)
  
  return(
    round(c("p" = npar - 1,
            "BIC" = log(ssr/t) + npar * log(t)/t,
            "R2" = summary(model)$r.squared), 4)
  )
}

#Apply to an intercept only
BIC(dynlm(ts(cci_diff_ts) ~ 1))

order <- 1:6

BICs <- sapply(order, function(x) 
  BIC(dynlm(cci_diff_ts ~ L(cci_diff_ts, 1:x) + L(szent_diff_ts, 1:x), 
            start = c(2010, 1), end = c(2020, 12))))

capture.output(BICs , file="bayes_informationcriteria.txt")

BICs
# select the ADL model with the smallest BIC
BICs[, which.min(BICs[2, ])]

###########So based on the BIC the lag value should be 2 ##########

##### ADL MODEL

###cci - Supervised Sentiment Index
# estimate the ADL(2,2) model of CCI
cci_diff_ADL22 <- dynlm(cci_diff_ts ~ L(cci_diff_ts) +L(cci_diff_ts,2)+ L(szent_diff_ts) +  L(szent_diff_ts,2), 
                        start = c(2010, 2), end = c(2020, 12))
cci_diff_ADL22_coeftest <- coeftest(cci_diff_ADL22, vcov. = sandwich)
cci_diff_ADL22_coeftest

###cci - Dictionary Sentiment Index

cci_dict_diff_ADL22 <- dynlm(cci_diff_ts ~ L(cci_diff_ts) +L(cci_diff_ts,2)+ L(szent_dict_diff_ts) +  L(szent_dict_diff_ts,2), 
                        start = c(2010, 2), end = c(2020, 12))
cci_dict_diff_ADL22_coeftest <- coeftest(cci_dict_diff_ADL22, vcov. = sandwich)
cci_dict_diff_ADL22_coeftest


##### GRANGER CAUSALITY

#However, the Granger causality test shows that we can reject the H0, and the sentiment is helpful in predicting consumer sentiment
granger_1 <- grangertest(szent_diff_ts,cci_diff_ts,order=2,na.action = na.omit)
granger_1

