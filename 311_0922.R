Requests <- read.csv("Requests.csv", stringsAsFactors = F)
attach(Requests)
OPEN_DT[1:10]
year <-substr(OPEN_DT, start=7, stop=10)
month <-substr(OPEN_DT,start=1,stop=2)
table(year)
table(month)
library(ggplot2)
qplot(month)

#Too many variables! Slow loading. Let's just focus on Time and Type.#
s <- data.frame(Requests$OPEN_DT,Requests$TYPE,stringsAsFactors=FALSE)
#Add a new column indicate months.
s$month <- substr(OPEN_DT,start=1,stop=2)
#Pick up requests in Feb.
ss <- subset(s, month=="02")
#Find out which is the most frequent request.
sort(table(ss$Requests.TYPE),decreasing=TRUE)
#Draw the picture using barplot.
count <- sort(table(ss$Requests.TYPE),decreasing = TRUE)
barplot(count)
