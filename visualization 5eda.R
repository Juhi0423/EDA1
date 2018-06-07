ChickWeight
plot(ChickWeight)

library(MASS)
plot(UScereal$sugars,UScereal$calories)
library(lattice)
xyplot(MPG.city~Horsepower | Cylinders , data = Cars93)

##rgraphics
#ex1
#Creating an exploratory plot array
MASS::whiteside
plot(MASS::whiteside)

#Creating an explanatory scatterplot
plot(whiteside$Gas,whiteside$Temp,xlab = "Outside temperature",ylab = "Heating gas consumption")

#The plot() function is generic
plot(whiteside$Insul)


#sunflowerplot
sunflowerplot(Boston$rad,Boston$tax,main="sunflowerplot")

boxplot(crim = rad, data = Boston , log="y" )

mosaicplot(cyl ~ gear, data =mtcars, main="Mosaicplot")


#ex2
#.	Load the MASS package to make the Cars93 data frame available
MASS::Cars93

plot(Cars93$Max.Price,Cars93$Price)
plot(Cars93$Max.Price,Cars93$Price,pch=17,col="red")
points(Cars93$Min.Price,Cars93$Price,pch=16,col="blue")
abline(lm(Cars93$Price~Cars93$Max.Price))
abline(a=0,b=1)

install.packages("robustbase")
library( robustbase )
animals<-robustbase::Animals2
par(mfrow=1:2)
plot(animals$brain,animals$body)+title("Original Representation")
plot(animals$brain,animals$body,log ="xy")
title("Log-log plot")

install.packages("insuranceData")
library( insuranceData )
data("dataCar")
par(mfrow=c(1:2))
tbl<-sort(table(dataCar$veh_body), decreasing = T)
pie(tbl)
title("Pie Chart")
barplot(tbl,las=0,cex.names = 0.5 )+title("Bar Chart")



library(MASS)
plot(medv~rm,data = Boston)

mtC0r<-cor(mtcars)
library


#diffrent plot
#ex1
library(MASS)
par(mfrow=c(1:2))
hist(Cars93$Horsepower,main = "hist() plot")
truehist(Cars93$Horsepower,main = "truehist() plot")

#Density plots as smoothed histograms
ChickWeight 
index16<-which(ChickWeight$Time==16)
weights <- ChickWeight$weight[index16]
hist(weights)
truehist(weights)
lines(density(weights))

#Using the qqPlot() function to see many details in data
install.packages("car")
library(car)
qqPlot(weights)

qqPlot(Boston)

#ex2
#The sunflowerplot() function for repeated numerical data
plot(Boston$rad,Boston$zn)
title("Standard scatterplot")
sunflowerplot(Boston$rad,Boston$zn)
title("Sunflower plot")

#Useful options for the boxplot() function
boxplot(Boston$crim~Boston$rad,varwidth = T,log = "y",las=1)
title("Crime rate vs. radial highway index")

#Using the mosaicplot() function
 mosaicplot(mtcars$carb~mtcars$cyl) 

 
#ex3
 #
 install.packages("aplpack")
library( aplpack ) 
boxplot(Cars93$Min.Price,Cars93$Max.Price) 
bagplot(Cars93$Min.Price,Cars93$Max.Price,cex = 20)
abline(a=0,b=1)

#
install.packages("corrplot")
library(corrplot )
numericalVars<- UScereal[,2:10]
str( UScereal )
corrMat<-cor(numericalVars)
corrplot(corrMat,method = "ellipse")


#
install.packages("rpart")
library(rpart)
tree_model<-rpart(medv~ . , data=Boston)

plot(tree_model) 
text(tree_model,cex=0.70,col="red",xpd=T) 


#pie chart
x = table(dataCar$veh_age)
pie(x,main = "my best piechart",radius = 1,cex=0.8,
       clockwise = T,col = rainbow(length(tbl)))
legend("topright",c("1","2","3","4"),cex=0.8,fill=rainbow(length(x)))
       