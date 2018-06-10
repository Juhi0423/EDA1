###########exploring numerical data######

setwd("C:\\Users\\Administrator\\Desktop\\eda\\exploring numerical data")
car<-read.csv("cars04.csv")
car
library(ggplot2)
str(car)
ggplot(car,aes(x=city_mpg))+geom_histogram()+facet_wrap(~suv)
#or
car %>% ggplot(aes(x=city_mpg))+geom_histogram()+facet_wrap(~suv)
table(car$city_mpg,car$suv)

library(dplyr)
unique(car$ncyl)
common_cyl<-car %>%filter(ncyl == c(4,6,8))
common_cyl<-car %>%filter(ncyl %in% c(4,6,8))
common_cyl1 <-car[car$ncyl %in% c(4,6,8),]
OR
common_cyl1<-filter(car,ncyl %in% c(4,6,8)) ##
ggplot(common_cyl,aes(x=as.factor(ncyl),y=city_mpg))+geom_boxplot()
ggplot(common_cyl,aes(x=city_mpg,fill="ncyl"))+geom_density()

library(dplyr)
library(ggplot2)
 car%>% filter(eng_size < 2.0)%>% ggplot(aes(x = hwy_mpg))+geom_histogram()
 
car%>% filter(eng_size < 2.0)%>% ggplot(aes(x = hwy_mpg))+geom_histogram(binwidth = 5)

car%>% filter(eng_size<2.0) %>% ggplot(aes(x = hwy_mpg))+ geom_density(bw=5)

##############marginal and conditinal#####
car %>%ggplot(aes(x=horsepwr)) +geom_histogram()+ggtitle("Distribution of horsepwr")

car %>% 
  filter(msrp < 25000) %>%
  ggplot(aes(horsepwr)) +
  geom_histogram()+
  xlim(c(90, 550)) +
  ggtitle("Distribution of horsepwr for cars that have an msrp < 25000")


car %>%
  ggplot(aes(horsepwr)) +
  geom_histogram(binwidth = 3) +
  ggtitle("Distribution of Horsepwr - Bin width (3)")

car %>%
  ggplot(aes(horsepwr)) +
  geom_histogram(binwidth = 30) +
  ggtitle("Distribution of Horsepwr - Bin width (30)")

car %>%
  ggplot(aes(horsepwr)) +
  geom_histogram(binwidth = 60) +
  ggtitle("Distribution of Horsepwr - Bin width (60)")




