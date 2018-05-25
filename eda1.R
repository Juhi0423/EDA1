
install.packages("XML")
library(XML)
url<- "C:/Users/Administrator/Desktop/eda/EDA/cd_catalog.xml"
url
xmldoc<- xmlParse(url)
rootNode<-xmlRoot(xmldoc)
rootNode[1]
data<- xmlSApply(rootNode,function(x) xmlSApply(x, xmlValue))
data
cd.catalog <- data.frame(t(data),row.names = NULL)          
cd.catalog[1:2,]                
t(data)

write.csv(cd.catalog,"C:/Users/Administrator/Desktop/New folder/cd_catalog.csv")



install.packages("jsonlite")
library("jsonlite")
dat.1 <- fromJSON("C:/Users/Administrator/Desktop/eda/EDA/students.json")
dat.2 <- fromJSON("C:/Users/Administrator/Desktop/eda/EDA/student-courses.json")
dat.1
dat.2

a <- toJSON(dat.1)
a


install.packages("httr")
library("httr")
url<- "http://finance.yahoo.com/webservice/v1/symbols/allcurrencies/quote?format=json"
jsonDoc<-fromJSON(url)
jsonDoc

dat <- jsonDoc$list$resources$resource$fields
dat
dat[1:2,]
class(dat)

jsoncars<-toJSON(mtcars,pretty=TRUE)
jsoncars

cat(jsoncars)



install.packages("RJSONIO")
library(RJSONIO)

install.packages("RCurl")
library("RCurl")
 
json_file = getURL("https://finance.yahoo.com/webservice/v1/symbols/allcurrencies/quote?format=json")
json_file2= RJSONIO::fromJSON(json_file) 
json_file2
head(json_file2)

json_file3 = as.data.frame(json_file2,pretty=TRUE)
json_file3
json_file3[1:2,]
View(t(json_file3))

################case study 1########################
install.packages("hflights") 
library("hflights")
 data("hflights")

#2.	Convert to a local data frame (use command tbl_df)
library(dplyr)
flights1 = tbl_df(hflights)
class(flights1)



#3.	Convert to a normal data frame to see all of the columns
flights2<- data.frame(flights1)
flights2
class(flights2)

#4.	base R approach & dplyr approach to view all flights on January 1 
View(flights2)
flights2 %>% group_by(Month==1)%>% filter(DayofMonth==1) 
flights2[flights2$Month == 1& flights2$DayofMonth ==1,]
View(hflights)

#5.	Pick columns by name - DepTime, ArrTime, FlightNum using base r and dplyr
flights2[,c("DepTime","ArrTime","FlightNum")]
select(flights2,"DepTime","ArrTime","FlightNum")
flights2%>% select("DepTime","ArrTime","FlightNum")


#6.	select UniqueCarrier and DepDelay columns and filter for delays over 60 minutes in dplyr
flights2%>% filter(ArrDelay > 60) %>% select ("UniqueCarrier","DepDelay")




#7.use mutate to create a new variable Speed (in mph) and  display distance ,airtime and speed columns

flights2 %>% select("Distance","AirTime")%>%mutate(Speed= Distance/AirTime *60)


#9.create a dataset grouped by Dest, and then summarise each group by taking the mean of ArrDelay

flights2%>% group_by(Dest)%>% summarise(mean(ArrDelay,na.rm=TRUE))



#10.	for each day of the year, count the total number of flights and sort in descending order
flights2%>% group_by(DayofMonth,Month,Year)%>%count()%>%arrange(desc(n))






#########case study 2##############



setwd("C:\\Users\\Administrator\\Desktop\\eda")
x<-read.csv("bank.csv",header=T,stringsAsFactors = F,sep = ";")
View(x)


#1.Check for Missing Values
x
#no missing values 


#2.Get the frequency distribution of each dimension column
m <- colnames(x)
lol <- list()

for(i in seq(length(m))){
  lol <- list(lol,x %>% group_by(get(m[i])) %>% summarise(n()))
}

lol

#3.Frequency distribution of numerical 
cols<-c("balance","day","duration")
for(i in cols)
  print(x%>%group_by(get(i))%>%summarize(count=(n)))




#4.Histogram on bank balance
hist(x$balance)




#5.No of campaigns across different months
x%>%group_by(month)%>%count(campaign)



#6.Average bal across each category
x%>% group_by(job)%>% summarise(mean(balance))


#7.Response rate by job category
x%>%group_by(job)%>%count(y)





