setwd("C:\\Users\\Administrator\\Desktop\\EDA1")
iris
View(iris)
colnames(iris)
str(iris)
levels(iris$Species)
table(iris$Species)



#scatterplot
plot(iris$Sepal.Length~iris$Petal.Length,xlab="petal lenght",ylab="sepal length",
     main = "sepal length vs petal length")
plot(iris$Sepal.Length~iris$Petal.Length,xlab="petal lenght",ylab="sepal length",
     main = "sepal length vs petal length",col="red",pch=16)


#frequency distribution of sepal.width
range(iris$Sepal.Width)
table(iris$Sepal.Width)
bins<-seq(1,5,by=2)
intervals<-cut(iris$Sepal.Width,bins)
tab<-transform(table(intervals))


#Histogram
hist(iris$Sepal.Width,xlab="sepal width",main="Distribution of sepal width",
     col="aquamarine")

#boxplot
boxplot(iris$Sepal.Width)
boxplot(iris$Sepal.Length~iris$Species)
boxplot(iris$Sepal.Length~iris$Species,xlab="species",ylab="sepal length",
        main="sepal length of different species",col="burlywood")
boxplot(iris$Species,iris$Sepal.Length,xlab="species",ylab="sepal length",
        main="sepal length of different species",col="blue")




######ggplot
library(ggplot2)
ggplot(data=iris)
colnames(iris)
ggplot(iris,aes(y=Sepal.Length,x=Petal.Length))
ggplot(iris,aes(x=Petal.Length,y=Sepal.Length))+geom_point()
ggplot(iris,aes(x=Petal.Length,y=Sepal.Length))+geom_point(col="red")
ggplot(iris,aes(x=Petal.Length,y=Sepal.Length,col=Species))+geom_point()
ggplot(iris,aes(x=Petal.Length,y=Sepal.Length,shape=Species))+geom_point()
ggplot(iris,aes(x=Petal.Length,y=Sepal.Length,col=Species,shape=Species))+geom_point()


##histogram
ggplot(iris,aes(x=Sepal.Width))+geom_histogram()
ggplot(iris,aes(x=Sepal.Width))+geom_histogram(bins = 50,
            fill="palegreen",col="red") ## here we use fill as an attribute

ggplot(iris,aes(x=Sepal.Width,fill=Species))+geom_histogram() ## here we use fill as an aesthetic
table(iris$Sepal.Width,iris$Species)
ggplot(iris,aes(x=Sepal.Width,fill=Species))+
      geom_histogram(position = "fill") ####gives proportion
ggplot(iris,aes(x=Sepal.Width,fill=Species))+geom_histogram(position = "identity") 


##barplot
ggplot(iris,aes(x=Species))+geom_bar()
ggplot(iris,aes(x=Species))+geom_bar(fill="red",col="blue")
ggplot(iris,aes(x=Species))+geom_bar(col="red")

View(iris)
##frequency polygon
ggplot(iris,aes(x=Sepal.Width))+geom_freqpoly()
ggplot(iris,aes(x=Sepal.Width))+geom_freqpoly(bins=50)
ggplot(iris,aes(x=Sepal.Width,fill=Species,col="red"))+geom_freqpoly(bins=50)
ggplot(iris,aes(x=Sepal.Width,fill=Species))+geom_freqpoly(bins=50,col="red")


##boxplot
ggplot(iris,aes(x=Species,y=Sepal.Length))+geom_boxplot()
ggplot(iris,aes(x=Species,y=Sepal.Length,fill=Species))+geom_boxplot()
