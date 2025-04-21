# Unit 7 - Lecture 2, Predictive Policing


# VIDEO 3 - A Basic Line Plot

# Load our data:
mvt = read.csv("mvt.csv", stringsAsFactors=FALSE)  #

str(mvt)
# Convert the Date variable to a format that R will recognize:
mvt$Date = strptime(mvt$Date, format="%m/%d/%y %H:%M")

# Extract the hour and the day of the week:
mvt$Weekday = weekdays(mvt$Date)
mvt$Hour = mvt$Date$hour

# Let's take a look at the structure of our data again:
str(mvt)

# Create a simple line plot - need the total number of crimes on each day of the week. We can get this information by creating a table:
table(mvt$Weekday)

# Save this table as a data frame:
WeekdayCounts = as.data.frame(table(mvt$Weekday))

str(WeekdayCounts) 


# Load the ggplot2 library:
library(ggplot2)

# Create our plot
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1))  

# Make the "Var1" variable an ORDERED factor variable
WeekdayCounts$Var1 = factor(WeekdayCounts$Var1, ordered=TRUE, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday"))

# Try again:
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1))

# Change our x and y labels:
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1), alpha=0.3) + xlab("Day of the Week") + ylab("Total Motor Vehicle Thefts")



# VIDEO 4 - Adding the Hour of the Day

# Create a counts table for the weekday and hour:
table(mvt$Weekday, mvt$Hour)

# Save this to a data frame:
DayHourCounts = as.data.frame(table(mvt$Weekday, mvt$Hour))

str(DayHourCounts)

# Convert the second variable, Var2, to numbers and call it Hour:
DayHourCounts$Hour = as.numeric(as.character(DayHourCounts$Var2))

# Create out plot:
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1))

# Change the colors
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Var1), size=2)

# Separate the weekends from the weekdays:
DayHourCounts$Type = ifelse((DayHourCounts$Var1 == "Sunday") | (DayHourCounts$Var1 == "Saturday"), "Weekend", "Weekday")

# Redo our plot, this time coloring by Type:
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Type), size=2) 
  

# Make the lines a little transparent:
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Type), size=2, alpha=0.5) 



# Fix the order of the days:
DayHourCounts$Var1 = factor(DayHourCounts$Var1, ordered=TRUE, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Make a heatmap:
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq))

# Change the label on the legend, and get rid of the y-label:

ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name="Total MV Thefts") + theme(axis.title.y = element_blank())
  
# Change the color scheme
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name="Total MV Thefts", low="white", high="red") + theme(axis.title.y = element_blank())




# VIDEO 5 - Maps

# Install and load two new packages:
install.packages("maps")
install.packages("ggmap")
library(maps)
library(ggmap)

# Load a map of Chicago into R:
chicago = get_map(location = "chicago", zoom = 11)

# Look at the map
ggmap(chicago)

# Plot the first 100 motor vehicle thefts:
ggmap(chicago) + geom_point(data = mvt[1:100,], aes(x = Longitude, y = Latitude))

# Round our latitude and longitude to 2 digits of accuracy, and create a crime counts data frame for each area:
LatLonCounts = as.data.frame(table(round(mvt$Longitude,2), round(mvt$Latitude,2)))
LatLonCounts2 = subset(LatLonCounts,LatLonCounts$Freq > 0)
str(LatLonCounts)

# Convert our Longitude and Latitude variable to numbers:
LatLonCounts$Long = as.numeric(as.character(LatLonCounts$Var1))
LatLonCounts$Lat = as.numeric(as.character(LatLonCounts$Var2))
1638-686

# Plot these points on our map:
ggmap(chicago) + geom_point(data = LatLonCounts, aes(x = Long, y = Lat, color = Freq, size=Freq))

# Change the color scheme:
ggmap(chicago) + geom_point(data = LatLonCounts, aes(x = Long, y = Lat, color = Freq, size=Freq)) + scale_colour_gradient(low="yellow", high="red")

# We can also use the geom_tile geometry
ggmap(chicago) + geom_tile(data = LatLonCounts, aes(x = Long, y = Lat, alpha = Freq), fill="red")

###no ocean data
# Convert our Longitude and Latitude variable to numbers:
LatLonCounts2$Long = as.numeric(as.character(LatLonCounts2$Var1))
LatLonCounts2$Lat = as.numeric(as.character(LatLonCounts2$Var2))

# Plot these points on our map:
ggmap(chicago) + geom_point(data = LatLonCounts2, aes(x = Long, y = Lat, color = Freq, size=Freq))

# Change the color scheme:
ggmap(chicago) + geom_point(data = LatLonCounts2, aes(x = Long, y = Lat, color = Freq, size=Freq)) + scale_colour_gradient(low="yellow", high="red")

# We can also use the geom_tile geometry
ggmap(chicago) + geom_tile(data = LatLonCounts2, aes(x = Long, y = Lat, alpha = Freq), fill="red")



# VIDEO 6 - Geographical Map on US

# Load our data:
murders = read.csv("murders.csv")

str(murders)

# Load the map of the US
statesMap = map_data("state")

str(statesMap)
table(statesMap$group)

# Plot the map:
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") 

# Create a new variable called region with the lowercase names to match the statesMap:
murders$region = tolower(murders$State)

# Join the statesMap data and the murders data into one dataframe:
murderMap = merge(statesMap, murders, by="region")
str(murderMap)

# Plot the number of murder on our map of the United States:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Murders)) + geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")

# Plot a map of the population:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Population)) + geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")

# Create a new variable that is the number of murders per 100,000 population:
murderMap$MurderRate = murderMap$Murders / murderMap$Population * 100000
murderMap$gunOwnershipRate = murderMap$GunOwnership/ murderMap$Population * 100000

# Redo our plot with murder rate:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = gunOwnershipRate)) + geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")

# Redo the plot, removing any states with murder rates above 10:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = MurderRate)) + geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend", limits = c(0,10))

###Homework



# Plot the map:
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") 

# Create a new variable called region with the lowercase names to match the statesMap:
murders$region = tolower(murders$State)

# Join the statesMap data and the murders data into one dataframe:
murderMap = merge(statesMap, murders, by="region")
str(murderMap)

# Plot the number of murder on our map of the United States:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Murders)) + geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")

# Plot a map of the population:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Population)) + geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")

# Create a new variable that is the number of murders per 100,000 population:
murderMap$MurderRate = murderMap$Murders / murderMap$Population * 100000
murderMap$gunOwnershipRate = murderMap$GunOwnership/ murderMap$Population * 100000

# Redo our plot with murder rate:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = gunOwnershipRate)) + geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")

# Redo the plot, removing any states with murder rates above 10:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = MurderRate)) + geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend", limits = c(0,10))

###Homework

str(statesMap)
table(statesMap$group)
polling=read.csv("PollingImputed.csv")
str(polling)
Train = subset(polling,Year == 2004 & 2008)
Test = subset(polling,Year == 2012)
mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
TestPrediction = predict(mod2, newdata=Test, type="response")
summary(mod2)
mean(TestPrediction)
TestPredictionBinary = as.numeric(TestPrediction > 0.5)
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)
table(predictionDataFrame$TestPredictionBinary)
str(predictionDataFrame)
summary(predictionDataFrame)
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
predictionMap = predictionMap[order(predictionMap$order),]
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

?geom_polygon


HW###3

edges.csv
users.csv

edges=data.frame(read.csv("edges.csv"))
users=data.frame(read.csv("users.csv"))
str(edges)
str(users)
head(users)
summary(users)
table(users$school, users$locale)
table(users$school, users$gender)
install.packages("igraph")
library("igraph")
?graph.data.frame
g = graph.data.frame(edges, FALSE, users) 
plot(g, vertex.size=5, vertex.label=NA)
g
table(degree(g)) 
summary(g)
table(degree(g) >= 10)
friends10=subset(counttotal, total>9)
friends10
V(g)$size = degree(g)/2+2
V(g)$size
plot(g, vertex.label=NA)

V(g)$color = "black"
plot(g, vertex.label=NA)
V(g)$color[V(g)$gender == "A"] = "red"
plot(g, vertex.label=NA)
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)

V(g)$color[V(g)$school == "B"] = "blue"
V(g)$color[V(g)$school == "AB"] = "red"
plot(g, vertex.label=NA)

V(g)$color = "black"
plot(g, vertex.label=NA)
V(g)$color[V(g)$locale == "A"] = "red"
plot(g, vertex.label=NA)
V(g)$color[V(g)$locale == "B"] = "gray"
plot(g, vertex.label=NA)

plot(g, vertex.label=NA)
?igraph.plotting 

g <- graph.ring(10)
g$layout <- layout.circle
plot(g)
tkplot(g)
rglplot(g)
# plotting a random graph, set the parameters in the command arguments
g <- barabasi.game(100)
plot(g, layout=layout.fruchterman.reingold, vertex.size=4,
     vertex.label.dist=0.5, vertex.color="red", edge.arrow.size=0.5)

# plot a random graph, different color for each component
g <- erdos.renyi.game(100, 1/100)
comps <- clusters(g)$membership
colbar <- rainbow(max(comps)+1)
V(g)$color <- colbar[comps+1]
plot(g, layout=layout.fruchterman.reingold, vertex.size=5, vertex.label=NA)

# plot communities in a graph
g <- graph.full(5) %du% graph.full(5) %du% graph.full(5)
g <- add.edges(g, c(1,6, 1,11, 6,11))
com <- spinglass.community(g, spins=5)
V(g)$color <- com$membership+1
g <- set.graph.attribute(g, "layout", layout.kamada.kawai(g))
plot(g, vertex.label.dist=1.5)

# draw a bunch of trees, fix layout
igraph.options(plot.layout=layout.reingold.tilford)
plot(graph.tree(20, 2))
plot(graph.tree(50, 3), vertex.size=3, vertex.label=NA)
tkplot(graph.tree(50, 2, mode="undirected"), vertex.size=10,
       vertex.color="green")

##HW#3

tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)   #extra argument  for text data 
install.packages("tm")
library(tm)
install.packages("SnowballC")  #lets us use the tm package 
library(SnowballC)

# Create corpus (collection of documents)

corpus = Corpus(VectorSource(tweets$Tweet))  

# Look at corpus
corpus

<<VCorpus (documents: 1181, metadata (corpus/indexed): 0/0)>>
  
  corpus[[1]]   
#show us the first tweet

<<PlainTextDocument (metadata: 7)>>
  I have to say, Apple has by far the best customer care service I have ever received! @Apple @AppStore

# Convert to lower-case (stemming)

corpus = tm_map(corpus, tolower)    #tolower=function in R
corpus[[1]]
[1] "i have to say, apple has by far the best customer care service i have ever received! @apple @appstore"

# IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line before continuing (it converts corpus to a Plain Text Document). This is a recent change having to do with the tolower function that occurred after this video was recorded.

corpus = tm_map(corpus, PlainTextDocument)


# Remove punctuation

corpus = tm_map(corpus, removePunctuation)
corpus[[1]]

<<PlainTextDocument (metadata: 7)>>
  i have to say apple has by far the best customer care service i have ever received apple appstore

# Look at stop words 
stopwords("english")[1:10]
[1] "i"         "me"        "my"        "myself"    "we"        "our"       "ours"      "ourselves" "you"       "your"  

# Remove stopwords and apple

corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))  #id ones to remove =apple, english stop words
corpus[[1]]
<<PlainTextDocument (metadata: 7)>>
  say    far  best customer care service   ever received  appstore

or

tm_map(corpusTitle, removeWords, sw)


# Stem document 

corpus = tm_map(corpus, stemDocument)
corpus[[1]]

<<PlainTextDocument (metadata: 7)>>
  say    far  best custom care servic   ever receiv  appstor


# prepare corpus for a prediction problem


# Create matrix
IF ERROR:
  Error in UseMethod("meta", x) : 
  no applicable method for 'meta' applied to an object of class "try-error"
In addition: Warning message:
  In mclapply(unname(content(x)), termFreq, control) :
  all scheduled cores encountered errors in user code

DO:
  corpus <- tm_map(corpus, PlainTextDocument)

allTweets2 = DocumentTermMatrix(corpus)  #rows=documents, columns=words in tweets, #times word occurs in matrix


allTweets = as.data.frame(as.matrix(allTweets2))

wordcloud(colnames(allTweets), colSums(allTweets))

install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()
brewer.pal() 
