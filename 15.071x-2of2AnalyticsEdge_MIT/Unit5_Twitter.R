# Unit 5 - Twitter


# VIDEO 5

# Read in the data

tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)

str(tweets)


# Create dependent variable

tweets$Negative = as.factor(tweets$Avg <= -1)

table(tweets$Negative)


# Install new packages

install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)


# Create corpus
 
corpus = Corpus(VectorSource(tweets$Tweet))

# Look at corpus
corpus

corpus[[1]]


# Convert to lower-case

corpus = tm_map(corpus, tolower)

corpus[[1]]

# IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line before continuing (it converts corpus to a Plain Text Document). This is a recent change having to do with the tolower function that occurred after this video was recorded.

corpus = tm_map(corpus, PlainTextDocument)


# Remove punctuation

corpus = tm_map(corpus, removePunctuation)

corpus[[1]]

# Look at stop words 
stopwords("english")[1:10]

# Remove stopwords and apple

corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))

corpus[[1]]

# Stem document 

corpus = tm_map(corpus, stemDocument)

corpus[[1]]




# Video 6

# Create matrix

frequencies = DocumentTermMatrix(corpus)

frequencies

# Look at matrix 

inspect(frequencies[1000:1005,505:515])

# Check for sparsity

findFreqTerms(frequencies, lowfreq=20)
findFreqTerms(frequencies, lowfreq=100)
# Remove sparse terms

sparse = removeSparseTerms(frequencies, 0.995)
sparse

# Convert to a data frame

tweetsSparse = as.data.frame(as.matrix(sparse))

# Make all variable names R-friendly

colnames(tweetsSparse) = make.names(colnames(tweetsSparse))

# Add dependent variable

tweetsSparse$Negative = tweets$Negative

# Split the data

library(caTools)

set.seed(123)

split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)

trainSparse = subset(tweetsSparse, split==TRUE)
testSparse = subset(tweetsSparse, split==FALSE)



# Video 7

# Build a CART model

library(rpart)
library(rpart.plot)

tweetCART = rpart(Negative ~ ., data=trainSparse, method="class")

prp(tweetCART)

# Evaluate the performance of the model
predictCART = predict(tweetCART, newdata=testSparse, type="class")

table(testSparse$Negative, predictCART)

# Compute accuracy

(294+18)/(294+6+37+18)

# Baseline accuracy 

table(testSparse$Negative)

300/(300+55)

#Logistic regression model

tweetLog = glm(Negative ~ ., data=trainSparse, family=binomial)

predictions = predict(tweetLog, newdata=testSparse, type="response")
table(testSparse$Negative, predictions > 0.5)
(254+37)/(254+37+46+18)

# Random forest model

library(randomForest)
set.seed(123)

tweetRF = randomForest(Negative ~ ., data=trainSparse)

# Make predictions:
predictRF = predict(tweetRF, newdata=testSparse)

table(testSparse$Negative, predictRF)

# Accuracy:
(293+21)/(293+7+34+21)


###UNIT 5 HW

wiki = read.csv("wiki.csv", stringsAsFactors=FALSE)   
str(wiki)
wiki$Vandal = as.factor(wiki$Vandal)
table(wiki$Vandal)

library(tm)
library(SnowballC)

corpusAdded=Corpus(VectorSource(wiki$Added))  

corpusAdded[[1]]
stopwords("english")[1:10]
corpusAdded = tm_map(corpusAdded, removeWords, c(stopwords("english")))  

corpusAdded = tm_map(corpusAdded, removeWords, sw)

corpusAdded = tm_map(corpusAdded, stemDocument)
dtmAdded=DocumentTermMatrix(corpusAdded)
length(stopwords("english"))  #length of stop words
dtmAdded
inspect(dtmAdded[1000:1005,505:515])  
findFreqTerms(dtmAdded, lowfreq=20) 
sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded

wordsAdded=as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

###
Removed

corpusRemoved=Corpus(VectorSource(wiki$Removed))  

corpusRemoved[[1]]
stopwords("english")[1:10]
corpusRemoved = tm_map(corpusRemoved, removeWords, c(stopwords("english")))  

corpusRemoved = tm_map(corpusRemoved, removeWords, sw)

corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved=DocumentTermMatrix(corpusRemoved)
length(stopwords("english"))  #length of stop words
dtmRemoved
inspect(dtmRemoved[1000:1005,505:515])  
findFreqTerms(dtmRemoved, lowfreq=20) 
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved

wordsRemoved=as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
wordsRemoved
str(wordsRemoved)

wikiWords = cbind(wordsAdded, wordsRemoved)
str(wikiWords)

# Add dependent variable

wikiWords$Vandal = wiki$Vandal

# Split the data

library(caTools)

set.seed(123)

split = sample.split(wikiWords$Vandal, SplitRatio =0.7)

split =sample.split(wikiWords$Vandal, SplitRatio=0.7)

trainwikiWords = subset(wikiWords, split==TRUE)
testwikiWords = subset(wikiWords, split==FALSE)

##Baseline accuracy

table(testwikiWords$Vandal)
#0   1 
#618 545 

618/(545+618)

library(rpart)
library(rpart.plot)

wikiCART = rpart(Vandal ~ ., data=trainwikiWords, method="class")

prp(wikiCART)
predictwiki = predict(wikiCART, newdata=testwikiWords, type="class")

table(testwikiWords$Vandal, predictwiki)

(618+12)/ (618+12+0+533)

#copy of dataframe
wikiWords2 = wikiWords

#Make a new column in wikiWords2 that is 1 if "http" was in Added:
  
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)

table(wikiWords2$HTTP)

wikiTrain2 = subset(wikiWords2, split==TRUE)
wikiTest2 = subset(wikiWords2, split==FALSE)

wikiCART2 = rpart(Vandal ~ ., data=wikiTrain2, method="class")

prp(wikiCART2)
predictwiki2 = predict(wikiCART2, newdata=wikiTest2, type="class")

table(wikiTest2$Vandal, predictwiki2)

(609+57)/(609+9+488+57)

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))

wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))



mean(wikiWords2$NumWordsAdded)

head(wikiWords2)

wikiTrain3 = subset(wikiWords2, split==TRUE)
wikiTest3 = subset(wikiWords2, split==FALSE)

wikiCART3 = rpart(Vandal ~ ., data=wikiTrain3, method="class")

prp(wikiCART3)
predictwiki3 = predict(wikiCART3, newdata=wikiTest3, type="class")

table(wikiTest3$Vandal, predictwiki3)
(514+248)/(514+248+104+297)


wikiWords3 = wikiWords2

wikiWords3$Minor = wiki$Minor

wikiWords3$Loggedin = wiki$Loggedin

#
wikiTrain3 = subset(wikiWords3, split==TRUE)
wikiTest3 = subset(wikiWords3, split==FALSE)

wikiCART3 = rpart(Vandal ~ ., data=wikiTrain3, method="class")

prp(wikiCART3)
predictwiki3 = predict(wikiCART3, newdata=wikiTest3, type="class")

table(wikiTest3$Vandal, predictwiki3)

(595+241)/(595+241+23+304)


##HW#2

trials=read.csv("clinical_trial.csv", stringsAsFactors=FALSE)

summary(trials)
str(trials)

table(ifelse(nchar(trials$abstract)==0,1,0))

max(nchar(trials$abstract))
which.min(nchar(trials$title))
trials$title[1258]

#1)
corpusAbstract= Corpus(VectorSource(trials$abstract))  
corpusTitle= Corpus(VectorSource(trials$title))  
#2)
corpusAbstract = tm_map(corpusAbstract, tolower)    #tolower=function in R
corpusTitle = tm_map(corpusTitle, tolower)    #tolower=function in R

corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)
#3)
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract  = tm_map(corpusAbstract , removePunctuation)
#4)
corpusTitle = tm_map(corpusTitle, removeWords, c(stopwords("english")))  
corpusAbstract = tm_map(corpusAbstract, removeWords, c(stopwords("english")))  

#5)
corpusTitle  = tm_map(corpusTitle , stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)
#6)
dtmTitle = DocumentTermMatrix(corpusTitle) 
dtmAbstract = DocumentTermMatrix(corpusAbstract) 


dtmTitle 
dtmAbstract

#7)

dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)

dtmTitle 
dtmAbstract

#8)
dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

length(stopwords("english")) 

dtmTitle 
dtmAbstract

inspect(dtmAbstract)

sort(colSums(inspect(dtmAbstract)))
#3.1
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))

colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

#3.2
dtm = cbind(dtmTitle, dtmAbstract)


dtm$trial = trials$trial

#3.3


library(caTools)

set.seed(144)

split = sample.split(dtm$trial, SplitRatio = 0.7)

train = subset(dtm, split==TRUE)
test = subset(dtm, split==FALSE)


#33

table(train$trial)

(730)/(572+730)



# Build a CART model (Predictive)

library(rpart)
library(rpart.plot)

trialCART = rpart(trial ~ ., data=train,method="class")

prp(trialCART)


# Evaluate the performance of the model
predictCART = predict(trialCART, newdata=test, type="class")

which.max(predictCART)
predictCART[5]
#3.7

table(test$trial, predictCART)

#test
(162+261)/(162+261+52+83)

#training:
(631+441)/(631+441+131+99)
441/(441+131)
631/(631+99)

library(ROCR)

PredictROC = predict(trialCART, newdata = test)  ####type =”prob” for auc
pred = prediction(PredictROC[,2], test$trial)   #2nd argument is true outcome values
as.numeric(performance(pred, "auc")@y.values)




###______HW#3

emails

emails = read.csv("emails.csv", stringsAsFactors=FALSE)   #extra argument  for text data 
str(emails)


sort(nchar(emails$text))
which.min(nchar(emails$text))


emails$spam = as.factor(emails$spam)
table(emails$spam)

library(tm)
library(SnowballC)

corpus = Corpus(VectorSource(emails$text))  
corpus = tm_map(corpus, tolower) 
corpus = tm_map(corpus, PlainTextDocument)  #CONVERT BACK TO PT, LOWER CAUSES ISSUES
corpus = tm_map(corpus, removePunctuation)
#corpus = tm_map(corpus, removeWords, sw) 
corpus = tm_map(corpus, removeWords, c(stopwords("english")))  
corpus = tm_map(corpus, stemDocument)

dtm=DocumentTermMatrix(corpus) 
dtm
spdtm = removeSparseTerms (dtm, 0.95)
spdtm

emailsSparse = as.data.frame(as.matrix(spdtm))

colnames(emailsSparse) = make.names(colnames(emailsSparse))

sort(colSums(emailsSparse))
emails$spam

emailsSparse$spam = emails$spam
str(emailsSparse)
tail(emailsSparse)



sort(colSums(subset(emailsSparse, spam == 1)))

emailsSparse2=emailsSparse
emailsSparse2$spam=as.numeric(emailsSparse2$spam)
sort(colSums(subset(emailsSparse2, spam == 0)))
sort(colSums(subset(emailsSparse, spam == 0)))

emailsSparse$spam = as.factor(emailsSparse$spam)

library(caTools)

set.seed(123)

split = sample.split(emailsSparse$spam, SplitRatio = 0.7)

train = subset(emailsSparse, split==TRUE)
test = subset(emailsSparse, split==FALSE)


# Logistic Regression Model
SpamLog = glm(spam ~ ., data=train, family=binomial)
summary(SpamLog)
predictSpamLog = predict(SpamLog, newdata=test, type="response") # give probabilities
str(predictSpamLog)
predictSpamLog

howmanySL1=subset(predictSpamLog, predictSpamLog > 0.00001 & predictSpamLog < 0.99999)
str(howmanySL1)

table(test$spam, predictSpamLog > 0.5)
(1257+376)/(1257+376+34+51)

library(ROCR)
ROCRpredTest = prediction(predictSpamLog, test$spam)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)

auc


# Build a CART model (Predictive)

library(rpart)
library(rpart.plot)

spamCART = rpart(spam ~ ., data=train, method="class")
PredictCART = predict(spamCART, newdata = test, type = "class")    #majority class predictions ~ threshold of 0.5
prp(spamCART)

PredictCART = predict(spamCART, newdata = train, type = "class") 
table(PredictCART, test$spam)
(1228+386)/(1228+386+80+24)
#
PredictCART = predict(spamCART, newdata = train, type = "class")    #majority class predictions ~ threshold of 0.5
library(ROCR)
#
PredictROC = predict(spamCART, newdata = train, type="prob")
PredictROC
pred = prediction(PredictROC[,2], train$spam)   #2nd argument is true outcome values
as.numeric(performance(pred, "auc")@y.values)


# Build random forest model

library(randomForest)


#train$spam = as.factor(train$spam)
#test$spam = as.factor(test$spam)

spamRF = randomForest(spam ~ ., data = train)

PredictForest = predict(spamRF, newdata = train)

table(train$spam, PredictForest)
(958+3046)/(958+3046+6)

library(ROCR)
#
PredictROC = predict(spamRF, newdata = train, type="prob")
PredictROC
pred = prediction(PredictROC[,2], train$spam)   #2nd argument is true outcome values
as.numeric(performance(pred, "auc")@y.values)
#OR
predictTest = predict(spamRF, type="response",newdata=test) # give probabilities

ROCRpredTest = prediction(predictTest, test$spam)


(#caTools package)
  
  auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
  
  auc
  
