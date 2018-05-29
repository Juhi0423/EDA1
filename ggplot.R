setwd("C:\\Users\\Administrator\\Desktop\\EDA1\\r files")
######statistcs

####EX1

#1 - Smoothing
str(mtcars)
library(ggplot2)

ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()

ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()+geom_smooth()

ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()+geom_smooth(method = "lm")


ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()+geom_smooth(method = "lm",se=F)


ggplot(mtcars,aes(x=wt,y=mpg))+geom_smooth(method = "lm",se=F)



#2 - Grouping variables

ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) +geom_point() +
  stat_smooth(method = "lm", se = F)

ggplot(mtcars, aes(x = wt, y = mpg, col =factor(cyl))) +geom_point() +
  stat_smooth(method = "lm", se = F)


ggplot(mtcars, aes(x = wt, y = mpg, col =factor(cyl))) +geom_point() +
  stat_smooth(method = "lm", se = F,aes(group=1))



#3 - Modifying stat_smooth

ggplot(mtcars, aes(x = wt, y = mpg, col =factor(cyl)))  +
  geom_smooth(method = "lm", se =F,spam=0.7)

ggplot(mtcars, aes(x = wt, y = mpg, col =factor(cyl)))  + geom_point()+
  stat_smooth(method = "loess", se =F,spam=0.7)

ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl)))  + geom_point()+
  stat_smooth(method = "loess", se =F,spam=0.7,group=1)+
  stat_smooth(aes(col="All"))

ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl)))  + geom_point()+
  stat_smooth(method = "loess", se =F,spam=0.7,group=1)+
  stat_smooth()

library(RColorBrewer)
myColors<-c(brewer.pal(3,"Dark2"),"black")

ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl)))  + geom_point()+
  stat_smooth(method = "loess", se =F,spam=0.7,group=1)+
  stat_smooth()+scale_color_manual("Cylinders",values = myColors)




#4 - Modifying stat_smooth 
View(Vocab)
ggplot(Vocab,aes(x=education,y=vocabulary))+geom_jitter()                                          

ggplot(Vocab,aes(x=education,y=vocabulary))+geom_jitter()+stat_smooth(method = "lm",se=F)

ggplot(Vocab,aes(x=education,y=vocabulary))+stat_smooth(method = "lm",se=F)

ggplot(Vocab,aes(x=education,y=vocabulary, col = factor(year)))+
  stat_smooth(method = "lm",se=F)

ggplot(Vocab,aes(x=education,y=vocabulary, col = factor(year)))+
  stat_smooth(method = "lm",se=F)+
  scale_color_manual()


ggplot(Vocab,aes(x=education,y=vocabulary, group = factor(year)))+
  stat_smooth(method = "lm",se=F)+
  scale_color_manual()

ggplot(Vocab,aes(x=education,y=vocabulary, group = factor(year)))+
  stat_smooth(method = "lm",se=F,alpha=0.6,size=2)+
  scale_color_manual()


  
#5 - Quantiles

ggplot(Vocab,aes(x=education,y=vocabulary, group = factor(year)))+
  stat_quantile(alpha=0.6,size=2)+
  scale_color_manual()


ggplot(Vocab,aes(x=education,y=vocabulary, group = factor(year)))+
  stat_quantile(alpha=0.6,size=2,quantiles = 0.5)+
  scale_color_manual()

blues<-brewer.pal(9,"Blues")
blue_range<-colorRampPalette(blues)
ggplot(Vocab,aes(x=education,y=vocabulary,col=year,group=factor(year)))+
  stat_smooth(method="lm",se=F,alpha=0.6,size=2)+
  scale_color_gradientn(colors=brewer.pal(9,"YlOrRd"))


#6 - Sum
p<-ggplot(Vocab, aes(x = education, y = vocabulary)) + 
  stat_smooth(method = "loess", se =F,aes(col="x"))+
    stat_smooth(method="lm",aes(col="y"),se=F)+
  scale_color_discrete("Model",labels=c("x"="LOESS","y"="lm"))
p+stat_sum()
p+stat_sum()+scale_size(range = c(1,10))



####EX2
#1-	Preparations
str(mtcars)
MTCARS<-mtcars
MTCARS$cyl<-as.factor(MTCARS$cyl)
MTCARS$am<-as.factor(MTCARS$am)
str(MTCARS)
summary(MTCARS)

posn.d<-position_dodge(width=0.1)
posn.jd<-position_jitterdodge(jitter.width =0.1,dodge.width=0.2)
posn.j<-position_jitter(width=0.2)

wt.cyl.am<-ggplot(MTCARS,aes(x=cyl,y=wt,col=am,fill=am,group=am))



#2 - Plotting variations
install.packages("Hmisc")
library(Hmisc)

wt.cyl.am +geom_point(position = posn.jd, alpha=0.6)

wt.cyl.am+geom_point(position = posn.jd, alpha=0.6)+
  stat_summary(fun.data=mean_sdl,fun.args=list(mult = 1),position=posn.d) 

wt.cyl.am+geom_point(position = posn.jd, alpha=0.6)+
  stat_summary(fun.data=mean_cl_normal ,position=posn.d) 

wt.cyl.am + stat_summary(geom = "point" ,fun.y = mean)


wt.cyl.am + stat_summary(geom = "point" ,fun.y = mean)+
  stat_summary(geom = "errorbar" ,fun.data = mean_sdl)



#Custom Functions

xx<-rnorm(100)
mean_sdl(xx, mult = 1)
gg_range<-function(x)
{

   data.frame(ymin=min(x),ymax=max(x))
}
gg_range(xx)

med_IQR<-function(x)
{
  data.frame(y=median(x),ymin=quantile(x)[2],ymax=quantile(x)[4] )
}

med_IQR(xx)



#Custom Functions (2)
wt.cyl.am<-ggplot(MTCARS,aes(x=cyl,y=wt,col=am,fill=am,group=am))

wt.cyl.am + stat_summary(geom="linerange",fun.data =med_IQR,position = posn.d)

wt.cyl.am +stat_summary(geom="linerange",fun.data =med_IQR,position = posn.d)+
  stat_summary(geom="linerange",fun.data =gg_range,alpha=0.4,size=3)


wt.cyl.am +stat_summary(geom="linerange",fun.data =med_IQR,position = posn.d)+
  stat_summary(geom="linerange",fun.data =gg_range,alpha=0.4,size=3)+
  stat_summary(geom="point",col="black",shape="x")


#####coordinate

library(reshape2)

######g and f
#####EX1


#Zooming In
library(ggplot2)

p<-ggplot(mtcars,aes(x=wt,y=mpg,col=factor(cyl)))+geom_point()+
  stat_smooth(method = "lm",span=0.7,se=F,aes(group=1))+
 stat_smooth(method="loess",se=F)
p+ scale_x_continuous(limits = c(3, 6),expand = c(0, 0))
p+ coord_cartesian(xlim =  c(3, 6))


#Aspect Ratio
head(iris)
base.plot<-ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width,col=Species))+geom_point()
base.plot
base.plot+coord_equal() #it means 1:1 aspect ratio 


#Pie Charts
thin.bar<-ggplot(mtcars,aes(x=1,fill=factor(cyl)))+stat_bin()
thin.bar
thin.bar+coord_polar(theta = "y")

wide.bar<-ggplot(mtcars,aes(x=1,fill=factor(cyl)))+geom_bar(width = 1)
wide.bar
wide.bar+coord_polar()


####EX2
#Facets: the basics

ggplot(MTCARS,aes(x=wt,y=mpg))+geom_point()+facet_grid(am~.)

ggplot(MTCARS,aes(x=wt,y=mpg))+geom_point()+facet_grid(.~cyl)

ggplot(MTCARS,aes(x=wt,y=mpg))+geom_point()+facet_grid(am~cyl)


#Many variables
library(RColorBrewer)
mycol<-c(brewer.pal(3,"Dark2"),"black")
ggplot(MTCARS,aes(x=wt,y=mpg,col=cyl))+geom_point()+
  scale_color_manual("cyl_am",values = mycol)

ggplot(MTCARS,aes(x=wt,y=mpg,col=cyl))+geom_point()+
  scale_color_manual("cyl_am",values = mycol)+
  facet_grid(gear~vs)


ggplot(MTCARS,aes(x=wt,y=mpg,col=cyl))+geom_point(aes(size=disp))+
  scale_color_manual("cyl_am",values = mycol)+
  facet_grid(gear~vs)



#Dropping levels
setwd("C:\\Users\\Administrator\\Desktop\\eda\\7 march")
library(xlsx)
mamsleep<-read.xlsx("mammal_sleep.xlsx",sheetName = "Sheet2")

library(ggplot2)
ggplot(mamsleep,aes(x=time,y=name,col=sleep))+geom_point()

ggplot(mamsleep,aes(x=time,y=name,col=sleep)) + geom_point()+facet_grid(vore~.)

ggplot(mamsleep,aes(x=time,y=name,col=sleep)) + geom_point()+
  facet_grid(vore~.,scale = "free_y" ,space = "free_y")
