library(ggplot2)
Requests <- read.csv("c:/311__Service_Requests.csv", header = TRUE)
attach(Requests)

s <- data.frame(Requests$OPEN_DT,Requests$TYPE,Requests$land_usage,Requests$Department,Requests$neighborhood,stringsAsFactors=F)
colnames(s) <- c("opendt","type","land","department","neighborhood")
head(s)
s$opendt[1:10]
year <-substr(s$opendt, start=9, stop=10)
month <-substr(s$opendt,start=1,stop=2)
s$yymm <- paste(year,month,sep="")
table(s$yymm)
tail(s)

#Q1 months and number of requests pattern.
qplot(s$yymm)

#Q2 In each month, time and number of requests.
#Convert AM/PM to 24h
a <- substr(s$opendt,start=12,stop=22)
s$hour <- substr(strptime(a, "%I:%M:%S %p"),start=12,stop=13)
s$month <- substr(s$opendt,start=1,stop=2)
#Check
tail(s)
p <- qplot(data=s, x=hour,geom="histogram")
p + facet_wrap(~ month,ncol=2)
#It seems there's no special pattern in months. So just put them in one figure.
qplot(data=s, x=hour,geom="histogram")


#Q3 months and number of requests, filled by land usage.
s$month <-substr(s$opendt,start=1,stop=2)
table(s$land)
head(s)
s$usage[s$land=="R1"] <- "Residential"
s$usage[s$land=="R2"] <- "Residential"
s$usage[s$land=="R3"] <- "Residential"
s$usage[s$land=="R4"] <- "Residential"
s$usage[s$land=="A"] <- "Residential"
s$usage[s$land=="RL"] <- "Residential"
s$usage[s$land=="CD"] <- "Residential"
s$usage[s$land=="CP"] <- "Residential"
s$usage[s$land=="CC"] <- "Commercial"
s$usage[s$land=="CM"] <- "Commercial"
s$usage[s$land=="C"] <- "Commercial"
s$usage[s$land=="RC"] <- "Commercial"
s$usage[s$land=="CL"] <- "Commercial"
s$usage[s$land=="I"] <- "Industrial"
s$usage[s$land=="E"] <- "Exempt"
s$usage[s$land=="EA"] <- "Exempt"
table(s$usage)
qplot(data=s,x=month,fill=s$usage)

#Q4
table(s$type)
count <- sort(table(s$type),decreasing = T)
count
count[1:5]
t1 <- subset(s,type=="Request for Snow Plowing")
t2 <- subset(s,type=="Schedule a Bulk Item Pickup")
t3 <- subset(s,type=="Requests for Street Cleaning")
t4 <- subset(s,type=="Missed Trash/Recycling/Yard Waste/Bulk Item")
t5 <- subset(s,type=="Pothole Repair (Internal)")
t <- rbind(t1,t2,t3,t4,t5)
ggplot(t, aes(month, fill=t$type)) + geom_bar()


#Q5 department
table(s$department)
part<-sort(table(s$department),decreasing = T)
part
part[1:5]
p1<-subset(s,Department=="PWDx")
p2<-subset(s,Department=="ISD")
p3<-subset(s,Department=="BTDT")
p4<-subset(s,Department=="INFO")
p5<-subset(s,Department=="PARK")
p<-rbind(p1,p2,p3,p4,p5)
ggplot(p,aes(month,fill=p$department))+geom_bar() #month
ggplot(p,aes(hour,fill=p$department))+geom_bar()  #time interval

#Q6 neighborhood
table(s$neighborhood)
neighbor <-sort(table(s$neighborhood),decreasing = T)
neighbor
neighbor[1:10]
n1<-subset(s,neighborhood=="Dorchester")
n2<-subset(s,neighborhood=="Roxbury")
n3<-subset(s,neighborhood=="Allston / Brighton")
n4<-subset(s,neighborhood=="Jamaica Plain")
n5<-subset(s,neighborhood=="South Boston / South Boston Waterfront")
n<-rbind(n1,n2,n3,n4,n5)
ggplot(n,aes(month,fill=n$neighborhood))+geom_bar()

#