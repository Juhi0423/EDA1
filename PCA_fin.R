library(ggplot2) #support scatterplot

library(psych) #PCA package
train <- read.csv("C:/Users/mathumitham/Desktop/EDA/Extra/NHLtrain.csv")

str(train)
names(train)

##standardize data with mean 0 and standard deviation of 1.

y = train[,-1:-2]
train.scale <- scale(train[, -1:-2])

nhl.cor <- cor(train.scale)
cor.plot(nhl.cor)
#Shots_For is correlated with Goals_For and 
#conversely, Shots_Against with Goals_Against. 
#There also is some negative correlation with PP_perc and PK_perc with Goals_Against.

pca <- principal(train.scale, rotate="none")
#use screeplot to determine what should be the number of components to retain.

#A scree plot can aid you in assessing the components that explain the most variance in the data. 
#It shows the Component number on the x-axis and their associated Eigenvalues on the y-axis:

plot(pca$values, type="b", ylab="Eigenvalues", xlab="Component")

#What you are looking for is a point in the scree plot where the rate of change decreases. 
#This will be what is commonly called an elbow or bend in the plot. 
#That elbow point in the plot captures the fact that additional variance explained by a component does not differ greatly from one component to the next. 
#In other words, it is the break point where the plot flattens out. 
#In this plot, five components look pretty compelling.

#Orthogonal rotation and interpretation

#the point behind rotation is to maximize the loadings of the variables on a specific component, which helps in simplifying the interpretation by reducing/eliminating the correlation among these components.
#The method to conduct orthogonal rotation is known as "varimax"

pca.rotate <- principal(train.scale, nfactors = 5, rotate = 
                          "varimax") ##nfactors = 5 for 5 components

##with component one that Goals_Against and Shots_Against have high positive loadings, while PP_perc and PK_perc have high negative loadings
#the high loading for component two is Goals_For. 
#Component five has high loadings with Shots_For, ff, and OZFOperc_pp.
#Component three seems to be only about the variables take 
#while component four is about hits.

#the table starting with the sum of square, SS loadings. Here, the numbers are the eigenvalues for each component.

#When they are normalized, you will end up with the Proportion Explained row, which as you may have guessed, stands for the proportion of the variance explained by each component
#You can see that component one explains 28 percent of all the variance explained by the five rotated components.
#Well, if you look at the Cumulative Var row, you see that these five rotated components account for 74% of the total 

#Creating factor scores from the components
#We will now need to capture the rotated component loadings as the factor scores for each individual team. 
#These scores indicate how each observation (in our case, the NHL team) relates to a rotated component
#capture the scores in a data frame as we will need to use it for our regression analysis:

pca.scores <- data.frame(pca.rotate$scores)

#These are simply the variables for each observation multiplied by the loadings on each component and then summed.


train.scale$pts = as.numeric(train.scale$pts)
train.scale$Def = pca.scores$PC1
train.scale$Off = pca.scores$PC3
train.scale$PPlay = pca.scores$PC2

nhl.lm = lm(pts~Def+Off+PPlay, data=nhl)

summary(nhl.lm)