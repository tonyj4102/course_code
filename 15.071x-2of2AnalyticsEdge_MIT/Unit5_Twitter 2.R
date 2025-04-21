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
  
  
  ################################Competition#######################
  
  # Unit 5 - Twitter
  
  
  # VIDEO 5
  
  # Read in the data
  
  #tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)
  Train=read.csv("NYTimesBlogTrain.csv",stringsAsFactors=FALSE)
  Test=read.csv("NYTimesBlogTest.csv",stringsAsFactors=FALSE)
  str(Train)
  str(Test)
  #str(tweets)
  
  TrainWC=Train  
  str(TrainWC)
  TrainWC$Popular  <- NULL
  str(TrainWC)
  
  total <- rbind(TrainWC, Test) 
 
   
  # Create dependent variable
  
  #tweets$Negative = as.factor(tweets$Avg <= -1)
  #Train$Popular = as.factor(Train$Popular)
  
  #table(tweets$Negative)
  table(Train$Popular)
  
  #>0    1 
  #>5439 1093
  
  # Install new packages
  
  install.packages("tm")
  library(tm)
  install.packages("SnowballC")
  library(SnowballC)
  
    
  #TrainC=Train$Abstract
  #TestC=Test$Abstract
  
  ##str(TrainC)
  #str(TestC)
  
 # Fullcorpus=rbind(TrainC,TestC)
  
  # Create corpus
  
 # corpus = Corpus(VectorSource(tweets$Tweet))
  corpus = Corpus(VectorSource(total$Abstract))
#Testcorpus = Corpus(VectorSource(Test$Abstract))
  
  # Look at corpus
  corpus
  
  corpus[[1]]
  
  
  # Convert to lower-case
  
  corpus = tm_map(corpus, tolower)
 #Testcorpus = tm_map(Testcorpus, tolower)
 
  corpus[[1]]
  
  # IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line before continuing (it converts corpus to a Plain Text Document). This is a recent change having to do with the tolower function that occurred after this video was recorded.
  
  corpus = tm_map(corpus, PlainTextDocument)
 #Testcorpus = tm_map(Testcorpus, PlainTextDocument)
  
  # Remove punctuation
  
  corpus = tm_map(corpus, removePunctuation)
# Testcorpus = tm_map(Testcorpus, removePunctuation)
 
  corpus[[1]]
  
  # Look at stop words 
  stopwords("english")[1:10]
  
  # Remove stopwords and apple
  
  #corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
  corpus = tm_map(corpus, removeWords, c(stopwords("english")))
 #Testcorpus = tm_map(Testcorpus, removeWords, c(stopwords("english")))
 
  corpus[[1]]
  
  # Stem document 
  
  corpus = tm_map(corpus, stemDocument)
 #Testcorpus = tm_map(Testcorpus, stemDocument)
 
  corpus[[1]]
  
  
  
  
  # Video 6
  
  # Create matrix
  
  frequencies = DocumentTermMatrix(corpus)
 #Testfrequencies = DocumentTermMatrix(Testcorpus)
 
  frequencies
  #Testfrequencies
  # Look at matrix 
  
  inspect(frequencies[1000:1005,505:515])
 #inspect(Testfrequencies[1000:1005,505:515]) 
 
  # Check for sparsity
  
  findFreqTerms(frequencies, lowfreq=20)
  findFreqTerms(Testfrequencies, lowfreq=100)
  # Remove sparse terms
  
  sparse = removeSparseTerms(frequencies, 0.995)
  sparse
 #Testsparse = removeSparseTerms(Testfrequencies, 0.995)
 #Testsparse
 
 
  # Convert to a data frame
  
  #tweetsSparse = as.data.frame(as.matrix(sparse))
  Sparse = as.data.frame(as.matrix(sparse))
 #TestSparse = as.data.frame(as.matrix(Testsparse))
 
  str(Sparse)
 #str(TestSparse)
 
  # Make all variable names R-friendly
  
  #colnames(tweetsSparse) = make.names(colnames(tweetsSparse))
  colnames(Sparse) = make.names(colnames(Sparse))
 #colnames(TestSparse) = make.names(colnames(TestSparse))
 
 
#split into training and test set

str(Sparse)
6532+1870

Train2=Sparse[1:6532,]
Test2=Sparse[6533:8402,]

str(Train2)
str(Test2)

# Add dependent variable
  
  # tweetsSparse$Negative = tweets$Negative
   Train2$Popular = Train$Popular
  

### Method #1   compine TestSparse and TrainSparse

  # Split the data
  
  #install.packages("caTools")
  #library(caTools)
  
 # set.seed(123)
  
  #split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
  
  #trainSparse = subset(tweetsSparse, split==TRUE)
  #testSparse = subset(tweetsSparse, split==FALSE)
  
  #split = sample.split(TrainSparse$Popular, SplitRatio = 0.7)
  
  #trainSparse = subset(tweetsSparse, split==TRUE)
  #testSparse = subset(tweetsSparse, split==FALSE)
  
  
  
  
  
  # Video 7
  
  # Build a CART model
  
  library(rpart)
 install.packages("rpart.plot")
  library(rpart.plot)
  
  #tweetCART = rpart(Negative ~ ., data=trainSparse, method="class")
 AbstractCART = rpart(Popular ~ ., data=Train2, method="class")
  
  #prp(tweetCART)
 prp(AbstractCART)
 
  # Evaluate the performance of the model
 
#predictCART = predict(tweetCART, newdata=testSparse, type="class")
predictCART = predict(AbstractCART, newdata=Test2, type="prob")

str(predictCART)
 
  table(testSparse$Negative, predictCART)
  
  # Compute accuracy
  
  #(294+18)/(294+6+37+18)
  
  # Baseline accuracy 
  
  table(predictCART)
  
  300/(300+55)
  
  #Logistic regression model
##1 --allvariables
  tweetLog =  
  summary(tweetLog)
#AIC 4946.9
predictions = predict(tweetLog, newdata=Test2, type="response") 

tweetLogstep=step(tweetLog)

predictions3 = predict(tweetLogstep, newdata=Test2, type="response")

##2--significant variables
tweetLog2 = glm(Popular ~ accus+acquisit+action+across+agre+america+appl+articl+artist+ask+bank+becom+billion+center+chang+chief+china+chines+choos+clinton+compani+cover+cultur+debut+design+director+discuss+dont+explor+facebook+fall+famili+fashion+festiv+find+fund+futur+gift+global+googl+gov+govern+great+group+grow+health+hedg+hold+hous+industri+internet+john+larg+legal+like+live+may+mean+media+million+music+need+new+north+obama+offer+onlin+open+owner+park+peter+photo+photograph+play+post+problem+produc+project+public+puzzl+race+reader+recent+respons+restaur+run+sale+sell+senat+seri+serv+share+show+small+star+startup+still+studi+style+support+take+theater+thought+time+today+tuesday+twitter+washington+watch+wednesday+week+will+without+women+write+writer+year, data=Train2, family=binomial)
summary(tweetLog2)
#AIC 4898.3   
predictions2 = predict(tweetLog2, newdata=Test2, type="response")

tweetLog2step=step(tweetLog2)

predictions4 = predict(tweetLog2step, newdata=Test2, type="response") 

##3 --best combination of variables



  #table(testSparse$Negative, predictions > 0.5)

  #(254+37)/(254+37+46+18)
  
  # Random forest model
  
  library(randomForest)
  set.seed(123)
  
  tweetRF = randomForest(Negative ~ ., data=trainSparse)
  
  # Make predictions:
  predictRF = predict(tweetRF, newdata=testSparse)
  
  table(testSparse$Negative, predictRF)
  
  # Accuracy:
  (293+21)/(293+7+34+21)

###############
###############
###############


################################Competition#######################


Train=read.csv("NYTimesBlogTrain.csv",stringsAsFactors=FALSE)
Test=read.csv("NYTimesBlogTest.csv",stringsAsFactors=FALSE)
str(Train)
str(Test)
head(Test)

TrainWC=Train  
str(TrainWC)
TrainWC$Popular  <- NULL
str(TrainWC)


# Combine train and test set

total <- rbind(TrainWC, Test) 


# Create dependent variable


table(Train$Popular)


install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)



# Create corpus

corpus = Corpus(VectorSource(total$Abstract))


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

corpus = tm_map(corpus, removeWords, c(stopwords("english")))

corpus[[1]]


# Stem document 

corpus = tm_map(corpus, stemDocument)

corpus[[1]]


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

Sparse = as.data.frame(as.matrix(sparse))

str(Sparse)


# Make all variable names R-friendly

colnames(Sparse) = make.names(colnames(Sparse))


#split into training and test set

str(Sparse)
6532+1870

Train2=Sparse[1:6532,]
Test2=Sparse[6533:8402,]

str(Train2)
str(Test2)


# Add dependent variable


Train2$Popular = Train$Popular


# Build a CART model

library(rpart)
install.packages("rpart.plot")
library(rpart.plot)


AbstractCART = rpart(Popular ~ ., data=Train2, method="class")


prp(AbstractCART)

# Evaluate the performance of the model

#predictCART = predict(tweetCART, newdata=testSparse, type="class")
predictCART = predict(AbstractCART, newdata=Test2, type="prob")

head(predictCART)
table(predictCART)


#****************************submission1 ****************

submission1 = data.frame(UniqueID = Test$UniqueID, Probability1 = predictCART)
head(submission1)
submission1$Probability1 = ifelse(submission1$Probability1.0>submission1$Probability1.1,submission1$Probability1.0,submission1$Probability1.1)
head(submission1)
submission1r = data.frame(submission1$UniqueID,submission1$Probability1)
head(submission1r)
write.csv(submission1r,"submission1.csv", row.names=TRUE)


#*******************************************************

str(predictCART)

#table(testSparse$Negative, predictCART)

# Compute accuracy

#(294+18)/(294+6+37+18)

# Baseline accuracy 

table(predictCART)

300/(300+55)


#Logistic regression model

##1 --allvariables
tweetLog =  glm(Popular ~ ., data=Train2, family=binomial)
summary(tweetLog)
#AIC 4946.9
predictions = predict(tweetLog, newdata=Test2, type="response")

#***********   SUBMISSION#2 *********************

submission2 = data.frame(UniqueID = Test$UniqueID, Probability1 = predictions)

head(submission2)
write.csv(submission2, "submission2.csv", row.names=TRUE)


## SCORE = 0.77590

#April 29, 2015 -- moved up to 285 positions to #1817


tweetLogstep=step(tweetLog)

# with step library

predictions3 = predict(tweetLogstep, newdata=Test2, type="response")

#***********   SUBMISSION#4 *********************

submission4 = data.frame(UniqueID = Test$UniqueID, Probability1 = predictions3)

head(submission4)
write.csv(submission4, "submission4.csv", row.names=TRUE)



##2--significant variables
tweetLog2 = glm(Popular ~ accus+acquisit+action+across+agre+america+appl+articl+artist+ask+bank+becom+billion+center+chang+chief+china+chines+choos+clinton+compani+cover+cultur+debut+design+director+discuss+dont+explor+facebook+fall+famili+fashion+festiv+find+fund+futur+gift+global+googl+gov+govern+great+group+grow+health+hedg+hold+hous+industri+internet+john+larg+legal+like+live+may+mean+media+million+music+need+new+north+obama+offer+onlin+open+owner+park+peter+photo+photograph+play+post+problem+produc+project+public+puzzl+race+reader+recent+respons+restaur+run+sale+sell+senat+seri+serv+share+show+small+star+startup+still+studi+style+support+take+theater+thought+time+today+tuesday+twitter+washington+watch+wednesday+week+will+without+women+write+writer+year, data=Train2, family=binomial)
summary(tweetLog2)
#AIC 4898.3   

predictions2 = predict(tweetLog2, newdata=Test2, type="response")


#***********   SUBMISSION#3 *********************

submission3 = data.frame(UniqueID = Test$UniqueID, Probability1 = predictions2)

head(submission3)
write.csv(submission3, "submission3.csv", row.names=TRUE)



tweetLog2step=step(tweetLog2)

# with step library

predictions4 = predict(tweetLog2step, newdata=Test2, type="response") 

##3 


#table(testSparse$Negative, predictions > 0.5)

#(254+37)/(254+37+46+18)

# Random forest model

install.packages("randomForest")

library(randomForest)
set.seed(123)



#OPTION 1 -- tooo long
tweetRF = randomForest(Popular ~ ., data=Train2)

#OPTION 2 -- with significant variables from the glm model
#accus+acquisit+action+across+agre+america+appl+articl+artist+ask+bank+becom+billion+center+chang+chief+china+chines+choos+clinton+compani+cover+cultur+debut+design+director+discuss+dont+explor+facebook+fall+famili+fashion+festiv+find+fund+futur+gift+global+googl+gov+govern+great+group+grow+health+hedg+hold+hous+industri+internet+john+larg+legal+like+live+may+mean+media+million+music+need+new+north+obama+offer+onlin+open+owner+park+peter+photo+photograph+play+post+problem+produc+project+public+puzzl+race+reader+recent+respons+restaur+run+sale+sell+senat+seri+serv+share+show+small+star+startup+still+studi+style+support+take+theater+thought+time+today+tuesday+twitter+washington+watch+wednesday+week+will+without+women+write+writer+year

tweetRF = randomForest(Popular ~ accus+acquisit+action+across+agre+america+appl+articl+artist+ask+bank+becom+billion+center+chang+chief+china+chines+choos+clinton+compani+cover+cultur+debut+design+director+discuss+dont+explor+facebook+fall+famili+fashion+festiv+find+fund+futur+gift+global+googl+gov+govern+great+group+grow+health+hedg+hold+hous+industri+internet+john+larg+legal+like+live+may+mean+media+million+music+need+new+north+obama+offer+onlin+open+owner+park+peter+photo+photograph+play+post+problem+produc+project+public+puzzl+race+reader+recent+respons+restaur+run+sale+sell+senat+seri+serv+share+show+small+star+startup+still+studi+style+support+take+theater+thought+time+today+tuesday+twitter+washington+watch+wednesday+week+will+without+women+write+writer+year, data=Train2)


summary(tweetRF)

# Make predictions:
predictRF = predict(tweetRF, newdata=Test2)

#***********   SUBMISSION#6 (OPTION#2-trees with significant variables) *********************

submission6 = data.frame(UniqueID = Test$UniqueID, Probability1 = predictRF)

head(submission6)
write.csv(submission6, "submission6.csv", row.names=TRUE)



#Logistic regression model

##1 --allvariables
#sig var to 3***
tweetLog2 =  glm(Popular ~ american+appl+design+play+puzzl+senat+share+show+tuesday+will, data=Train2, family=binomial)
summary(tweetLog2)
predictions2 = predict(tweetLog2, newdata=Test2, type="response")

#***********   SUBMISSION#7 *********************
submission7 = data.frame(UniqueID = Test$UniqueID, Probability1 = predictions2)
head(submission7)
write.csv(submission7, "submission7.csv", row.names=TRUE)

#SCORE = 0.55818

tweetLog3 =  glm(Popular ~ year+women+week++still+seri+photo+photograph+open+north+new+music+live+fashion+cultur+china+artist+america+american+appl+design+play+puzzl+senat+share+show+tuesday+will, data=Train2, family=binomial)
summary(tweetLog3)
predictions3 = predict(tweetLog3, newdata=Test2, type="response")

#***********   SUBMISSION#8 *********************
submission8 = data.frame(UniqueID = Test$UniqueID, Probability1 = predictions3)
head(submission8)
write.csv(submission8, "submission8.csv", row.names=TRUE)

#SCORE = 0.65826




# Add dependent variable




Train2$NewsDesk=as.factor(Train$NewsDesk)
Train2$SectionName=as.factor(Train$SectionName)
Train2$SubsectionName=as.factor(Train$SubsectionName)
Train2$WordCount=as.numeric(Train$WordCount)
TrainDateConvert = strptime(Train$PubDate, "%Y-%m-%d %H:%M:%S")
Train2$mon <- NULL #as.factor(TrainDateConvert$mon)
Train2$mday <- as.factor(TrainDateConvert$mday)
Train2$hour <- as.factor(TrainDateConvert$hour)
Train2$wday <- as.factor(TrainDateConvert$wday)

Test2$NewsDesk=as.factor(Test$NewsDesk)
Test2$SectionName=as.factor(Test$SectionName)
Test2$SubsectionName=as.factor(Test$SubsectionName)
Test2$WordCount=as.numeric(Test$WordCount)
TestDateConvert = strptime(Test$PubDate, "%Y-%m-%d %H:%M:%S")
Test$mon <- as.factor(TestDateConvert$mon)
Test2$mday <- as.factor(TestDateConvert$mday)
Test2$hour <- as.factor(TestDateConvert$hour)
Test2$wday <- as.factor(TestDateConvert$wday)
str(Test2$wday)

head(Train2$mon)
#Logistic regression model +++other variables date etc,except headling,etc.


##1-- all Significant variables + other original variables

tweetLog6 =  glm(Popular ~ NewsDesk+SectionName+SubsectionName+WordCount+day+hour+wday+american+appl+design+play+puzzl+senat+share+show+tuesday+will, data=Train2, family=binomial)
summary(tweetLog6)
#AIC: 3164.3

predictions6 = predict(tweetLog6, newdata=Test2, type="response")
#***********   SUBMISSION#9 *********************
submission11 = data.frame(UniqueID = Test$UniqueID, Probability1 = predictions6)
head(submission11)
write.csv(submission11, "submission11.csv", row.names=TRUE)


## Score:  0.90541

#Your Best Entry ↑
#You improved on your best score by 0.12951.
#You just moved up 819 positions on the leaderboard.


##2-- all Significant variables(***) + other original variables * step function


tweetLog6 =  glm(Popular ~ NewsDesk+SectionName+SubsectionName+WordCount+day+hour+wday+american+appl+design+play+puzzl+senat+share+show+tuesday+will, data=Train2, family=binomial)
summary(tweetLog6)

tweetLog6step=step(tweetLog6)
#Step:  AIC=3161.36

predictions7 = predict(tweetLog6step, newdata=Test2, type="response")
#***********   SUBMISSION#9 *********************
submission12 = data.frame(UniqueID = Test$UniqueID, Probability1 = predictions7)
head(submission12)
write.csv(submission12, "submission12.csv", row.names=TRUE)



##2-- all Significant variables(***/**) + other original variables * step function

tweetLog7 =  glm(Popular ~ NewsDesk+SectionName+SubsectionName+WordCount+day+hour+wday+american+appl+design+play+puzzl+senat+share+show+tuesday+will+year+women+week+still+seri+photo+photograph+open+north+new+music+live+fashion+cultur+china+artist+america, data=Train2, family=binomial)
summary(tweetLog7)
##  AIC: 3136.6

tweetLog7A =  glm(Popular ~ ., data=Train2, family=binomial)
summary(tweetLog7A)
##  AIC: 3430

#keep in significant from above (***.**,.)
tweetLog7B =  glm(Popular ~ NewsDesk+SectionName+SubsectionName+WordCount+hour+wday+american+appl+design+play+share+tuesday+year+women+north+music+live+america, data=Train2, family=binomial)
summary(tweetLog7B)
##  AIC: 3127

#keep in significant from above (***.**)
tweetLog7C =  glm(Popular ~ NewsDesk+SectionName+SubsectionName+WordCount+hour+wday+appl+play+women+music+live, data=Train2, family=binomial)
summary(tweetLog7C)
##  AIC: 3146

#keep in significant from above (***)
tweetLog7D =  glm(Popular ~ NewsDesk+SectionName+SubsectionName+WordCount+hour+wday+appl, data=Train2, family=binomial)
summary(tweetLog7D)
##  AIC: 3183


tweetLog7Bstep=step(tweetLog7B)

#Step: 3126.97



tweetLog7step=step(tweetLog7)
summary(tweetLog7step)
# 3122.


predictions8 = predict(tweetLog7step, newdata=Test2, type="response")
#***********   SUBMISSION#13 *********************
submission13 = data.frame(UniqueID = Test$UniqueID, Probability1 = predictions8)
head(submission13)
write.csv(submission13, "submission13.csv", row.names=TRUE)

#Your Best Entry ↑
#You improved on your best score by 0.00571.
#You just moved up 277 positions on the leaderboard

#Score 0.91112


## -- submission 14 + heading corpus

str(Train)
str(Test)
head(Test)

TrainH=Train$Headline
TestH=Train$Headline

# Combine train and test set
totalH <- rbind(TrainH, TestH) 

# Create corpus
corpusH = Corpus(VectorSource(totalH))

# Look at corpus
corpusH
corpusH[[1]]

# Convert to lower-case
corpusH = tm_map(corpusH, tolower)
corpus[[1]]

# IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line before continuing (it converts corpus to a Plain Text Document). This is a recent change having to do with the tolower function that occurred after this video was recorded.
corpusH = tm_map(corpusH, PlainTextDocument)

# Remove punctuation
corpusH = tm_map(corpusH, removePunctuation)
corpusH[[1]]

# Look at stop words 
stopwords("english")[1:10]

# Remove stopwords and apple
corpusH = tm_map(corpusH, removeWords, c(stopwords("english")))
corpusH[[1]]

# Stem document 
corpusH = tm_map(corpusH, stemDocument)
corpusH[[1]]

# Create matrix
frequenciesH = DocumentTermMatrix(corpusH)
frequenciesH

# Look at matrix 
inspect(frequenciesH[1000:1005,505:515])

# Check for sparsity
findFreqTerms(frequenciesH, lowfreq=20)
findFreqTerms(frequenciesH, lowfreq=100)

# Remove sparse terms
sparseH = removeSparseTerms(frequenciesH, 0.995)
sparseH

# Convert to a data frame
SparseH = as.data.frame(as.matrix(sparseH))
str(SparseH)

# Make all variable names R-friendly
colnames(SparseH) = make.names(colnames(SparseH))

#split into training and test set
str(SparseH)
6532+1870

Train2H=SparseH[1:6532,]
Test2H=SparseH[6533:8402,]

str(Train2H)
str(Test2H)


## -- submission 14 + heading corpus + (*,.) significant variables


Train2H$Popular=Train$Popular

**famili+fashion+test+share+week
*behind+busi+hong+oct+protest+question+read
.insid+look+name+onlin

#**
tweetLog8H= glm(Popular ~famili+fashion+test+share+week,data=Train2H, family=binomial)
summary(tweetLog8H)
#AIC: 5885.2

#**+*
tweetLog9H= glm(Popular ~famili+fashion+test+share+week+behind+busi+hong+oct+protest+question+read,data=Train2H, family=binomial)
summary(tweetLog9H)

#AIC: 5873

#**+*+.
tweetLog10H= glm(Popular ~insid+look+name+onlin+famili+fashion+test+share+week+behind+busi+hong+oct+protest+question+read,data=Train2H, family=binomial)

summary(tweetLog10H)

#AIC: 5860

TrainAll=cbind(Train2H, Train2)
TestAll=cbind(Test2H, Test2)

tweetLog11All =  glm(Popular ~ insid+look+name+onlin+famili+fashion+test+share+week+behind+busi+hong+oct+protest+question+read+NewsDesk+SectionName+SubsectionName+WordCount+day+hour+wday+american+appl+design+play+puzzl+senat+share+show+tuesday+will+year+women+week+still+seri+photo+photograph+open+north+new+music+live+fashion+cultur+china+artist+america, data=TrainAll, family=binomial)
summary(tweetLog11All)
## AIC 3176.7


tweetLog11Allstep=step(tweetLog11All)
#Step: AIC 3142



predictions9 = predict(tweetLog11Allstep, newdata=TestAll, type="response")
#***********   SUBMISSION#14 *********************
submission14 = data.frame(UniqueID = Test$UniqueID, Probability1 = predictions9)
head(submission14)
write.csv(submission14, "submission14.csv", row.names=TRUE)


## scored 0.90600, which is not an improvement of your best score. Keep trying! 


## -- 14 (Incl. only sig)


## -- ALL variables + heading corpus

str(TrainAll)

tweetLog12All =  glm(Popular ~., data=TrainAll, family=binomial)
summary(tweetLog12All)
## AIC 5902



tweetLog11Allstep=step(tweetLog11All)
#Step: AIC 3142

#####  - - -on cloud ALL ------

predictions9 = predict(tweetLog8step, newdata=Test2H, type="response")
#***********   SUBMISSION#15 *********************
submission14 = data.frame(UniqueID = Test$UniqueID, Probability1 = predictions9)
head(submission14)
write.csv(submission14, "submission14.csv", row.names=TRUE)


######  --- step tweet ALL ---



## Random Forest #1 with all

library(randomForest)
set.seed(123)

Train2H$Popular=as.factor(Train2H$Popular)

tweetRF = randomForest(Popular ~ ., data=Train2H,ntree=200, nodesize=25)

# Make predictions:
predictRF = predict(tweetRF, newdata=Test2H)

#***********   SUBMISSION#16 *********************
submission16 = data.frame(UniqueID = Test$UniqueID, Probability1 = predictRF)
head(submission16)
write.csv(submission16, "submission16.csv", row.names=TRUE)

## Train2H,scored 0.50036



## Random Forest #2  main variables + puzzl (Train2)

Train2F<-lapply(Train2, factor)
Test2F<-lapply(Test2, factor)

Train2S<-Train2$Popular
head(Train2S)
Train2S<-as.matrix(Train2S)
colnames(Train2S) <- c("Popular")
head(Train2S)

Train2S<-data.frame(Popular=factor())
Train2S<-as.data.frame(Train$Popular)
head(Train2S)
colnames(Train2S)[1] <- "Popular"
head(Train2S)
Train2S$Popular<-as.factor(Train2S$Popular)

Train2S$NewsDesk<-(Train2$NewsDesk)
Train2S$SectionName<-(Train2$SectionName)
Train2S$SubsectionName<-(Train2$SubsectionName)
Train2S$WordCount<-(Train2$WordCount)
Train2S$day<-as.factor(Train2$day)
Train2S$hour<-(Train2$hour)
Train2S$wday<-(Train2$wday)
Train2S$puzzl<-as.factor(Train2$puzzl)

Train2S$american<-as.factor(Train2$american)
Train2S$appl<-as.factor(Train2$appl)
Train2S$design<-as.factor(Train2$design)
Train2S$play<-as.factor(Train2$play)
Train2S$senat<-as.factor(Train2$senat)
Train2S$share<-as.factor(Train2$share)
Train2S$show<-as.factor(Train2$show)
Train2S$tuesday<-as.factor(Train2$tuesday)
Train2S$will<-as.factor(Train2$will)
Train2S$year<-as.factor(Train2$year)
Train2S$women<-as.factor(Train2$women)
Train2S$week<-as.factor(Train2$week)
Train2S$still<-as.factor(Train2$still)
Train2S$seri<-as.factor(Train2$seri)
Train2S$photo<-as.factor(Train2$photo)
Train2S$photograph<-as.factor(Train2$photograph)
Train2S$open<-as.factor(Train2$open)
Train2S$north<-as.factor(Train2$north)
Train2S$new<-as.factor(Train2$new)
Train2S$music<-as.factor(Train2$music)
Train2S$live<-as.factor(Train2$live)
Train2S$fashion<-as.factor(Train2$fashion)
Train2S$cultur<-as.factor(Train2$cultur)
Train2S$china<-as.factor(Train2$china)
Train2S$artist<-as.factor(Train2$artist)
Train2S$america<-as.factor(Train2$america)


Test2S<-data.frame(NewsDesk=factor())
head(Test2S)
Test2S<-as.data.frame(Test2$NewsDesk)
head(Test2S)
colnames(Test2S)[1] <- "NewsDesk"
head(Test2S)
summary(Test2S)

Test2S$SectionName<-(Test2$SectionName)
Test2S$SubsectionName<-(Test2$SubsectionName)
Test2S$WordCount<-as.numeric(Test2$WordCount)
Test2S$day<-(Test2$day)
Test2S$hour<-(Test2$hour)
Test2S$wday<-(Test2$wday)
Test2S$puzzl<-(Test2$puzzl)

Test2S$american<-as.factor(Test2$american)
Test2S$appl<-as.factor(Test2$appl)
Test2S$design<-as.factor(Test2$design)
Test2S$play<-as.factor(Test2$play)
Test2S$senat<-as.factor(Test2$senat)
Test2S$share<-as.factor(Test2$share)
Test2S$show<-as.factor(Test2$show)
Test2S$tuesday<-as.factor(Test2$tuesday)
Test2S$will<-as.factor(Test2$will)
Test2S$year<-as.factor(Test2$year)
Test2S$women<-as.factor(Test2$women)
Test2S$week<-as.factor(Test2$week)
Test2S$still<-as.factor(Test2$still)
Test2S$seri<-as.factor(Test2$seri)
Test2S$photo<-as.factor(Test2$photo)
Test2S$photograph<-as.factor(Test2$photograph)
Test2S$open<-as.factor(Test2$open)
Test2S$north<-as.factor(Test2$north)
Test2S$new<-as.factor(Test2$new)
Test2S$music<-as.factor(Test2$music)
Test2S$live<-as.factor(Test2$live)
Test2S$fashion<-as.factor(Test2$fashion)
Test2S$cultur<-as.factor(Test2$cultur)
Test2S$china<-as.factor(Test2$china)
Test2S$artist<-as.factor(Test2$artist)
Test2S$america<-as.factor(Test2$america)

str(Train2)
str(Test2)
head(Train2)
head(Test2)

Test2S$day=as.factor(Test2S$day)
Test2S$puzzl=as.factor(Test2S$puzzl)

levels(Train2S$day) = levels(Test2S$day)
levels(Test2S$SubsectionName) = levels(Train2S$SubsectionName)
levels(Test2S$SectionName) = levels(Train2S$SectionName)
levels(Test2S$NewsDesk) = levels(Train2S$NewsDesk)
levels(Test2S$appl ) = levels(Train2S$appl)
levels(Test2S$american ) = levels(Train2S$american)
levels(Test2S$show ) = levels(Train2S$show)
levels(Test2S$year ) = levels(Train2S$year)
levels(Train2S$open) = levels(Test2S$open)
levels(Test2S$north ) = levels(Train2S$north)
levels(Test2S$new ) = levels(Train2S$new)
levels(Test2S$fashion ) = levels(Train2S$fashion)
levels(Test2S$cultur ) = levels(Train2S$cultur)
levels(Test2S$artist ) = levels(Train2S$artist)
levels(Test2S$american ) = levels(Train2S$american)
levels(Test2S$music ) = levels(Train2S$music)
Test2S$artis <-NULL 

Train2$Popular<-NULL 
TestTrain2=rbind(Train2, Test2)



Train2=TestTrain2[1:6532,]
Test2=TestTrain2[6533:8402,]

Train2$Popular=Train$Popular

Test2S %in% Train2S

Train2S (is.element(Test2S))


NewsDesk+SectionName+SubsectionName+WordCount+day+hour+wday+puzzl  // not including wordcount

newYTRF3 = randomForest(Popular ~ NewsDesk+SectionName+SubsectionName+WordCount+day+hour+wday+american+appl+design+play+puzzl+senat+share+show+tuesday+will+year+women+week+still+seri+photo+photograph+open+north+new+music+live+fashion+cultur+china+artist+america, data=Train2,ntree=10000, nodesize=25)

# Make predictions:
predictRF3 = predict(newYTRF3, newdata=Test2)


#***********   SUBMISSION#3R *********************
submission3R = data.frame(UniqueID = Test$UniqueID, Probability1 = predictRF3)
head(submission3R)
write.csv(submission3R, "submission3R.csv", row.names=TRUE)


#Your Best Entry ↑
#You improved on your best score by 0.01551.
#You just moved up 542 positions on the leaderboard.



#########TOP PLUS ALL SIGNIFICANT


newYTRF4 = randomForest(Popular ~ accus+acquisit+action+across+agre+america+appl+articl+artist+ask+bank+becom+billion+center+chang+chief+china+chines+choos+clinton+compani+cover+cultur+debut+design+director+discuss+dont+explor+facebook+fall+famili+fashion+festiv+find+fund+futur+gift+global+googl+gov+govern+great+group+grow+health+hedg+hold+hous+industri+internet+john+larg+legal+like+live+may+mean+media+million+music+need+new+north+obama+offer+onlin+open+owner+park+peter+photo+photograph+play+post+problem+produc+project+public+puzzl+race+reader+recent+respons+restaur+run+sale+sell+senat+seri+serv+share+show+small+star+startup+still+studi+style+support+take+theater+thought+time+today+tuesday+twitter+washington+watch+wednesday+week+will+without+women+write+writer+year, data=Train2,ntree=10000, nodesize=25)

# Make predictions:
predictRF4 = predict(newYTRF4, newdata=Test2)
summary(predictRF4)

#***********   SUBMISSION#4R *********************
submission4R = data.frame(UniqueID = Test$UniqueID, Probability1 = predictRF4)
head(submission4R)
write.csv(submission4R, "submission4R.csv", row.names=TRUE)



###Ensemble

ensemble3 = read.csv("submission2submit.csv", stringsAsFactors=FALSE)
ensemble3 = read.csv("submission13Submit", stringsAsFactors=FALSE)
ensemble3 = read.csv("submission2RSubmit", stringsAsFactors=FALSE)

2
13
2R

predEnsem = (predictRF_test2 + predictNNT_test + predictRF_test )/3


########SupportVectorMachine


best model features


######## STEP #5 look at the features that are significant and use those only & text analytics library features



####################
####################

##insid+look+name+onlin+famili+fashion+test+share+week+behind+busi+hong+oct+protest+question+read+NewsDesk+SectionName+SubsectionName+WordCount+day+hour+wday+american+appl+design+play+puzzl+senat+share+show+tuesday+will+year+women+week+still+seri+photo+photograph+open+north+new+music+live+fashion+cultur+china+artist+america

tweetRF = randomForest(Popular ~ insid+look+name+onlin+famili+fashion+test+share+week+behind+busi+hong+oct+protest+question+read+NewsDesk+SectionName+SubsectionName+WordCount+day+hour+wday+american+appl+design+play+puzzl+senat+share+show+tuesday+will+year+women+week+still+seri+photo+photograph+open+north+new+music+live+fashion+cultur+china+artist+america, data=TrainAll,ntree=10000, nodesize=25)

# Make predictions:
predictRF = predict(tweetRF, newdata=TestAll)

#***********   SUBMISSION#16 *********************
submission16 = data.frame(UniqueID = Test$UniqueID, Probability1 = predictRF)
head(submission16)
write.csv(submission16, "submission16.csv", row.names=TRUE)

###########
#############



tweetLog7 =  glm(Popular ~ NewsDesk+SectionName+SubsectionName+WordCount+day+hour+wday+american+appl+design+play+puzzl+senat+share+show+tuesday+will+year+women+week+still+seri+photo+photograph+open+north+new+music+live+fashion+cultur+china+artist+america, data=Train2, family=binomial)
summary(tweetLog7)
##  AIC: 3136.6
tweetLog7step=step(tweetLog7)
summary(tweetLog7step)
# 3122

tweetLog7bstep=step(tweetLog7,direction = c("both"))
summary(tweetLog7bstep)

tweetLog7bkstep=step(tweetLog7,direction = c("backward"))
summary(tweetLog7bkstep)













tweetLog4 =  glm(Popular ~ ., data=Train2, family=binomial)
summary(tweetLog4)
predictions4 = predict(tweetLog4, newdata=Test2, type="response")
#***********   SUBMISSION#10 *********************
submission9 = data.frame(UniqueID = Test$UniqueID, Probability1 = predictions4)
head(submission9)
write.csv(submission9, "submission9.csv", row.names=TRUE)

tweetLog5 =  glm(Popular ~ ., data=Train2, family=binomial)
summary(tweetLog5)
predictions5 = predict(tweetLog5, newdata=Test2, type="response")
#***********   SUBMISSION#11 *********************
submission10 = data.frame(UniqueID = Test$UniqueID, Probability1 = predictions5)
head(submission10)
write.csv(submission7, "submission10.csv", row.names=TRUE)

#AIC 4946.9

predictions = predict(tweetLog, newdata=Test2, type="response")



