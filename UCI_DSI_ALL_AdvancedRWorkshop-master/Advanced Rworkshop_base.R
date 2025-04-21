install.packages("knitr")
install.packages("tidyr")
install.packages("magrittr")
install.packages("dplyr")
install.packages("shiny")
install.packages("ggplot2")
install.packages("dygraphs")
install.packages("rticles")
install.packages("pdflatex")

# pass a list to a shiny ap

rmarkdown::render("/Users/aj/Documents/AdvancedRWorkshop-master/RMarkdown/RCodes/stock-report.Rmd, params=list(symbol = "GOOG")

stSymb<-c("TSLA", "GOOG", "AMAN", "ETE")
                  
                  for (i in 1:4){")

?render outfile= specify name each time

install.packages("devtools")

devtools::install_github("garrettgman/DSR")
library(DSR)
#tubercolosis

table1
table2

library(tidyr)
library()
spread(table2,key,value)
> str(spread)

function (data, key, value, fill = NA, convert = FALSE, drop = TRUE) 

covert =TRUE string to integer
drop= keep/drop levels with no observations

table4
gather(table4, "year", "cases", 2:3)

install.packages("nycflight13")
library(nycflights13)
dim(flights)
  
as.data.frame(flights)
str(flights)

group_by(flights,tailnum)

delay = mean(dep_delay, na.rm = TRUE))

summarize(flights, count=n(flights), avgdist=mean(distance, na.rm=TRUE), )

delaycarr=group_by(flights, carrier)
delaycarr

delaycarr

ggplot(delayData, aes(dist, delay)) + geom_point(aes(size=count),alpha=1/2)+geom_smooth() + scale_size_area()


#alpha - superimposed data


