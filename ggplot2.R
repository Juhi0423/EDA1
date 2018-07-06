setwd("C:\\Users\\Administrator\\Desktop\\eda\\EDA1\\r files")
library(ggplot2)

mammals

##scatter plot
ggplot(mammals,aes(x=body,y=brain))+geom_point()
ggplot(mammals,aes(x=body,y=brain))+geom_point(alpha=0.6)+
  stat_smooth(method = "lm",col="red",se=F)
ggplot(mammals,aes(x=body,y=brain))+geom_point()+coord_fixed()+scale_x_log10()+scale_y_log10()+
  stat_smooth(method = "lm",col="red",se=F,size=1)

ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width))+
  geom_jitter(alpha=0.6) #to avoid overploting

ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width))+
  geom_jitter(alpha=0.6)+facet_grid(.~Species)

ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width,shape=factor(Species)))+
  geom_jitter(alpha=0.6)+facet_grid(.~Species)

ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width))+
  geom_jitter(alpha=0.6)+facet_grid(.~Species)+
  stat_smooth(method="lm",se=F,col="red")

ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width))+
  geom_jitter(alpha=0.6)+facet_grid(.~Species)+
  stat_smooth(method="lm",se=F,col="red")+
  scale_y_continuous("Sepal Width (cm)",
                     limits = c(2,5),
                      expand = c(0,0))+
  scale_x_continuous("sepal length (Cm)",
                     limits = c(4,8),
                     expand = c(0,0))+ coord_equal()


ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width))+
  geom_jitter(alpha=0.6)+facet_grid(.~Species)+
  stat_smooth(method="lm",se=F,col="red")+
  scale_y_continuous("Sepal Width (cm)",
                     limits = c(2,5),
                     expand = c(0,0))+
  scale_x_continuous("sepal length (Cm)",
                     limits = c(4,8),
                     expand = c(4,0))+ coord_equal()
             

ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width))+
  geom_jitter(alpha=0.6)+facet_grid(.~Species)+
  stat_smooth(method="lm",se=F,col="red")+
  scale_y_continuous("Sepal Width (cm)",
                     limits = c(2,5),
                     expand = c(4,0))+
  scale_x_continuous("sepal length (Cm)",
                     limits = c(4,8),
                     expand = c(4,0))+ coord_equal()



ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width))+
  geom_jitter(alpha=0.6)+facet_grid(.~Species)+
  stat_smooth(method="lm",se=F,col="red")+
  scale_y_continuous("Sepal Width (cm)",
                     limits = c(2,5),
                     expand = c(0,0))+
  scale_x_continuous("sepal length (Cm)",
                     limits = c(4,8),
                     expand = c(0,0))+ coord_equal()+
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_blank(),
        title = element_blank())
        


#####ggplot
####ex1
#1 - Explore and Explain
C.


#2 - Exploring ggplot2, part 1
str(mtcars)
ggplot(mtcars,aes(x=cyl,y=mpg))+geom_point(col="red")


#3 - Exploring ggplot2, part 2
ggplot(mtcars,aes(x=factor(cyl),y=mpg))+geom_point() 
    #for categorical variable always give factor()



####EX2
#4 - Exploring ggplot2, part 3

ggplot(mtcars, aes(x = wt, y = mpg)) +geom_point()
ggplot(mtcars, aes(x = wt, y = mpg, color = disp)) +
  geom_point()
ggplot(mtcars, aes(x = wt, y = mpg, color = "disp")) +
  geom_point()
ggplot(mtcars, aes(x = wt, y = mpg, size = disp)) +
  geom_point()
ggplot(mtcars, aes(x = wt, y = mpg, size = "disp")) +
  geom_point()



#5 -Understanding Variables
B.

ggplot(mtcars, aes(x = wt, y = mpg, shape = disp)) + ## for shape argument variable should be categorical
  geom_point()

ggplot(mtcars, aes(x = wt, y = mpg, shape = factor(cyl))) +
  geom_point()




####EX3
#1 - Exploring ggplot2, part 4
d<-diamonds
str(d)
dia<-head(d,1000)
ggplot(dia,aes(x=carat,y=price))+geom_point()
ggplot(dia,aes(x=carat,y=price))+geom_point()+geom_smooth()


#2 - Exploring ggplot2, part 5
ggplot(dia,aes(x=carat,y=price))+geom_smooth()
ggplot(dia,aes(x=carat,y=price,col=clarity))+geom_smooth()
ggplot(dia,aes(x=carat,y=price,color=clarity))+geom_point(alpha=0.4)


#3 - Understanding the grammar, part 1
dia_plot<-ggplot(dia,aes(x=carat,y=price))
dia_plot+geom_point()
dia_plot+geom_point(aes(col=clarity))


#4 - Understanding the grammar, part 2
dia_plot <- ggplot(diamonds, aes(x = carat, y = price))
dia_plot+ geom_point(alpha=0.2)
dia_plot+ geom_point(alpha=0.2)+geom_smooth(se=F)
dia_plot+ geom_point(alpha=0.2)+geom_smooth(se=T) #shades errors
dia_plot+ geom_point(alpha=0.2)+geom_smooth(aes(col=clarity),se=F)
dia_plot+ geom_point(alpha=0.2)+geom_smooth(col=clarity,se=F) #error if we dont give aes


#####diifrence between base r and ggplot
iris
#base R
plot(iris$Sepal.Length,iris$Sepal.Width)
points(iris$Petal.Length,iris$Petal.Width,col="red")

#ggplot
p<-ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width))+geom_point()
p+geom_jitter()
ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width))+geom_point()+
  geom_point(aes(x=Petal.Length,y=Petal.Width,col="red"))
library(tidyr)




####EX1
#1 - Base package and ggplot2, part 1 - plot
plot(mtcars$wt,mtcars$mpg)
plot(mtcars$wt,mtcars$mpg,col=factor(mtcars$cyl))
colnames(mtcars)
fcyl<-as.factor(mtcars$cyl)
library(dplyr)
mtcars<-mtcars %>% mutate(fcyl)
plot(mtcars$wt,mtcars$mpg,col=factor(fcyl))


#2 - Base package and ggplot2, part 2 - lm


carmodel<-lm(mpg~wt,data=mtcars)
mtcars$fcyl<-as.factor(mtcars$cyl)
plot(mtcars$wt,mtcars$mpg,col=levels(mtcars$fcyl))
abline(carmodel,lty=2)
legend(x=5,y=30,legend=levels(mtcars$fcyl),col=levels(mtcars$fcyl),pch=1,bty="n")


#3 - Base package and ggplot2, part 3
ggplot(mtcars,aes(x=wt,y=mpg,col=cyl))+geom_point()
ggplot(mtcars,aes(x=wt,y=mpg,col=cyl))+geom_point()+geom_smooth(method = "lm",se=F) 
ggplot(mtcars,aes(x=wt,y=mpg,col=fcyl))+geom_point()+geom_smooth(method = "lm",se=F)+
  geom_smooth()
ggplot(mtcars,aes(x=wt,y=mpg,col=fcyl))+geom_point()+geom_smooth(method = "lm",se=F)+
  geom_smooth(aes(group=1),method = "lm",se=F)
ggplot(mtcars,aes(x=wt,y=mpg,col=fcyl))+geom_point()+geom_smooth(method = "lm",se=F)+
  geom_smooth((aes(group=1)),method = "lm",se=F,lty=2)


#4 - ggplot2 compared to base package
D.

######
####EX3
#1 - Variables to visuals, part 1
#2 - Variables to visuals, part 1b


head(iris)

iris.wide <- gather(iris, Part, Value, -Species)
head(iris.wide)

iris.wide1 <- separate(iris.wide, Part, c("Part","Measure"))
head(iris.wide1)

ggplot(iris.wide1, aes(x = Species, y = Value, color= Part)) +
  geom_jitter() +
  facet_wrap(~Measure)
#3 - Variables to visuals, part 2
#4 - Variables to visuals, part 2b

iris.tidy <- iris %>% mutate(flower = 1:nrow(iris))
head(iris.tidy)

iris.tidy <- gather(iris.tidy, Part, Value, -c(Species, flower))             
head(iris.tidy)

iris.tidy <- separate(iris.tidy, Part,c("Part","Measure"))
head(iris.tidy)

iris.tidy <- spread(iris.tidy,Measure,Value)
head(iris.tidy)

ggplot(iris.tidy, aes(x = Length, y = Width, color = Part)) +
  geom_jitter() +
  facet_wrap(~Species)


#mapping
ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width))+geom_point(col="red")
ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width,col=Species))+geom_point()
ggplot(iris)+geom_point(aes(x=Sepal.Length,y=Sepal.Width,col=Species))



######aesthetic

####EX1
#1 - All about aesthetics, part 1
head(mtcars)
ggplot(mtcars,aes(x=mpg,y=cyl))+geom_point()
ggplot(mtcars,aes(x=cyl,y=mpg))+geom_point()
ggplot(mtcars,aes(x=wt,y=mpg,col=cyl))+geom_point()
ggplot(mtcars,aes(x=wt,y=mpg,col=cyl))+geom_point(shape=1,size=4)
#or
ggplot(mtcars)+geom_point(aes(x=wt,y=mpg,col=cyl),shape=1,size=4)


#2 - All about aesthetics, part 2
ggplot(mtcars,aes(x=wt,y=mpg,fill=cyl))+geom_point()
ggplot(mtcars,aes(x=wt,y=mpg,fill=cyl))+geom_point(shape=21,size=4)
ggplot(mtcars)+geom_point(aes(x=wt,y=mpg,fill=cyl),shape=21,size=4)
ggplot(mtcars)+geom_point(aes(x=wt,y=mpg,fill=factor(am)),shape=21,size=4,alpha=0.6)



#3 - All about aesthetics, part 3
ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()
ggplot(mtcars,aes(x=wt,y=mpg))+geom_point(aes(size=cyl))
ggplot(mtcars,aes(x=wt,y=mpg))+geom_point(aes(alpha=cyl))
ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()
ggplot(mtcars,aes(x=wt,y=mpg))+geom_point(aes(shape=factor(cyl)))

ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()+geom_text(aes(label=cyl))


#4 - All about attributes, part 1
ggplot(mtcars,aes(x=wt,y=mpg,col=cyl))+geom_point()
my_color="#4ABEFF"
ggplot(mtcars,aes(x=wt,y=mpg,col=cyl))+geom_point(col=my_color)
ggplot(mtcars,aes(x=wt,y=mpg,fill=cyl))+geom_point(col=my_color,
                                              size=10,shape=23)


#5 - All about attributes, part 2
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl))+geom_point(alpha=0.5)
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl))+geom_point(shape=24,col="yellow")
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl))+
  geom_text(aes(label=rownames(mtcars),col="red"))



#6 - Going all out
ggplot(mtcars,aes(x=mpg,y=qsec,col=factor(cyl)))+geom_point()

ggplot(mtcars,aes(x=mpg,y=qsec,col=factor(cyl),shape=factor(am),size=hp/wt))+geom_point()
#or
ggplot(mtcars,aes(x=mpg,y=qsec,col=factor(cyl)))+
  geom_point(aes(shape=factor(am)))
ggplot(mtcars,aes(x=mpg,y=qsec,col=factor(cyl)))+
  geom_point(aes(shape=factor(am),size=hp/wt))


#7- Aesthetics for categorical and continuous variables
C.
