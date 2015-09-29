Requests <- read.csv("Requests.csv", stringsAsFactors = F)
attach(Requests)
head(Requests)
#Data cleaning, no NA in year or month or type
OPEN_DT[1:10]
typeof(OPEN_DT)
year <- substr(OPEN_DT, start=7, stop=10)
month <- substr(OPEN_DT,start=1,stop=2)
type <- Requests$TYPE
sum(is.na(month))
sum(is.na(year))
sum(is.na(type))

#Create new data frame with year, month and type
Input <- data.frame(year, month, type)
head(Input)
#For 2014...
Input2014 <- subset(Input,year=="2014")
head(Input2014)
table(Input2014$type)
count <- sort(table(Input2014$type),decreasing = TRUE)
count[1:10]

#For "schedule a Bulk Item Pickup" in 2014...
library(ggplot2)
Input2014.Bulk <- subset(Input2014,type=="Schedule a Bulk Item Pickup")
head(Input2014.Bulk)
qplot(data=Input2014.Bulk, month)
#Maybe because of moving in summer?

Input2014.Pothole <- subset(Input2014,type=="Request for Pothole Repair")
qplot(data=Input2014.Pothole, month)
#Maybe because of snow?
