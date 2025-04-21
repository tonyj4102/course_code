

IBM=read.csv("IBMStock.csv")
GE=read.csv("GEStock.csv")
PG=read.csv("ProcterGambleStock.csv")
CC=read.csv("CocaColaStock.csv")
Boeing=read.csv("BoeingStock.csv")

CPSData=read.csv("CPSData.csv")
MetroAreaCodes=read.csv("MetroAreaCodes.csv")


IBM$Date = as.Date(IBM$Date, "%m/%d/%y")

GE$Date = as.Date(GE$Date, "%m/%d/%y")

CC$Date = as.Date(CC$Date, "%m/%d/%y")

PG$Date = as.Date(PG$Date, "%m/%d/%y")

Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

max(IBM$Date)
max(CC$Date)
max(Boeing$Date)
max(PG$Date)
max(GE$Date)

summary(IBM)

plot(CC$Date, CC$StockPrice, xlab = "Date", ylab = "StockPrice", col ="red")

lines(PG$Date, PG$StockPrice, col="blue",lty=1)

abline(v=as.Date(c("2000-03-01")), lwd=2)

abline(v=as.Date(c("1983-03-01")), lwd=2)

plot(CC$Date[301:432], CC$StockPrice[301:432], type="l", col="red", ylim=c(0,210))


lines(PG$Date, PG$StockPrice, lty=2, col="blue")
lines(Boeing$Date, Boeing$StockPrice, col="green")
lines(GE$Date, GE$StockPrice, col="purple")
lines(IBM$Date, IBM$StockPrice, col="orange")

abline(v=as.Date(c("2000-03-01")), lwd=2)
abline(v=as.Date(c("1997-09-01")), lwd=2)
abline(v=as.Date(c("1997-11-01")), lwd=2)


tapply(IBM$StockPrice, months(IBM$Date),mean, na.rm=TRUE)

tapply(CC$StockPrice, months(CC$Date),mean, na.rm=TRUE)

tapply(GE$StockPrice, months(GE$Date),mean, na.rm=TRUE)


#############-pg3-


CP=as.data.frame(CPSData)

sort(table(CP$Region))

table(CP$Race,CP$Hispanic)

summary(CP)
str(CP)

table(CP$Region, is.na(CP$Married))
table(CP$Sex, is.na(CP$Married))
table(CP$Age, is.na(CP$Married))
table(CP$Citizenship, is.na(CP$Married))
table(CP$Region, is.na(CP$Married))

MetroAreaCode

table(CP$Region, is.na(CP$MetroAreaCode))

tapply(is.na(CP$MetroAreaCode),CP$State, mean)

MetroAreaMap=read.csv("MetroAreaCodes.csv")
CountryMap=read.csv("CountryCodes.csv")

c

is.na(CP$MetroArea)
tapply(is.na(CP$Hispanic),CP$MetroArea,mean)

sort(tapply(CP$Race == "Asian",CP$MetroArea,mean))

sort(tapply(CP$Education == "No high school diploma", CP$MetroArea, mean, na.rm=TRUE))

str(CountryMap)
str(CountryCodes)
'data.frame':  149 obs. of  2 variables:
  $ Code   : int  57 66 73 78 96 100 102 103 104 105 ...
$ Country: Factor w/ 149 levels "Afghanistan",..: 139 57'data.frame':  149 obs. of  2 variables:
  
str(MetroAreaCodes)


str(MetroAreaMap)

> str(MetroAreaCodes)
'data.frame':  271 obs. of  2 variables:
  $ Code     : int  460 3000 3160 3610 3720 6450 10420 10500 10580 10740 ...
$ MetroArea: Factor w/ 271 levels "Akron, OH","Albany-Schenectady-T

CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)

CP = merge(CP, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)

CP = merge(CP, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)

str(CP)tr


