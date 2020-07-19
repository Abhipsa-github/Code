iris$variety
write.csv(iris,"iris.csv",row.names = FALSE)
library(ggplot2)
iris
#Summary measures of sepal.length
summary(iris$sepal.length,digits=3)

#plotting a histogram using ggplot2
#For single continuous variable and uses stat_bin by default
ggplot(iris,aes(x=Sepal.Length))+geom_histogram()
#1making it more precise to reveal the hidden facts
ggplot(iris,aes(x=Sepal.Length))+geom_histogram(bins=15,fill="blue",colour="white",size=0.2)
ggplot(iris,aes(x=Sepal.Length))+geom_histogram(bins=40,fill="blue",colour="white",size=0.2)
#2 lets use bandwidth in place of bin
ggplot(iris,aes(x=Sepal.Length))+geom_histogram(binwidth = 0.1,fill="blue",colour="white",size=0.2)
ggplot(iris,aes(x=Sepal.Length))+geom_histogram(binwidth = 0.05,fill="blue",colour="white",size=0.2)
ggplot(iris,aes(x=Sepal.Length))+geom_histogram(binwidth = 0.9,fill="blue",colour="white",size=0.2)
ggplot(iris,aes(x=Sepal.Length))+geom_histogram(binwidth = 0.3,fill="blue",colour="white",size=0.2)




# Histogram with normal density curve
library(tibble)
x<-seq(4.3,7.9,length.out = 150)
data<-with(iris,tibble(x=x,y=dnorm(x,mean(Sepal.Length),sd(Sepal.Length))))
ggplot(iris,aes(x=Sepal.Length,y=..density..))+geom_histogram(bins=15,fill="blue",colour="white",sep=0.2)+geom_line(data=data,aes(x=x,y=y),colour="red")


# Frequency Polygon
ggplot(iris,aes(x=Sepal.Length))+geom_freqpoly(colour="blue")
ggplot(iris,aes(x=Sepal.Length))+geom_freqpoly(bins=15,colour="blue")
ggplot(iris,aes(x=Sepal.Length))+geom_freqpoly(bins=30,colour="blue")
ggplot(iris,aes(x=Sepal.Length))+geom_freqpoly(bins=40,colour="blue")


#Comparision of distribution
ggplot(iris,aes(x=Sepal.Length, color=Species))+geom_freqpoly(bins=15)
ggplot(iris,aes(x=Sepal.Length, color=Species))+geom_freqpoly(bandwidth=0.1)
ggplot(iris,aes(x=Sepal.Length, color=Species))+geom_freqpoly(bandwidth=1)
ggplot(iris,aes(x=Sepal.Length, color=Species))+geom_freqpoly(bandwidth=1.5)
ggplot(iris,aes(x=Sepal.Length, color=Species))+geom_freqpoly(binwidth=1.5)
ggplot(iris,aes(x=Sepal.Length, color=Species))+geom_freqpoly(binwidth=1.2)



# Barplots
ggplot(iris,aes(x=Sepal.Length))+geom_bar()
ggplot(iris,aes(x=Sepal.Length))+geom_bar(fill="blue")
ggplot(iris,aes(x=Sepal.Length))+geom_bar()
ggplot(iris,aes(x=class))+geom_bar()
iris$class
iris$variables
iris$types
ggplot(iris,aes(x=Sepal.Length))+geom_bar(fill="blue",colour="red")

# Initial values of iris dataset
head(iris)
head(iris,4)


#To find out any missing data
is.na(iris)
sum(is.na(iris))

#More details of iris summary statistics
library(skimr)
install.packages(skimr)
skimr(iris)
skim(iris)


#summary statistics by species
iris%>%
  dplyr::group_by(species)%>%
  skim()

iris%>%
  dplyr::group_by(species)%>%
  skim()


library(dplyr)
iris%>%
  dplyr::group_by(species)%>%
  skim()

# pair plot
plot(iris,col="red")

# scatter plot
plot(iris$Sepal.Length,iris$Petal.Length)
plot(iris$Sepal.Length,iris$Petal.Length,xlab = "Sepal.Length",ylab = "Petal.Length",main = "Scatter Plot",col="iris$Species")


#histogram
hist(iris$Sepal.Length,col="red")
hist(iris$Sepal.Length,col="iris$Species")

#Feature Plot
featurePlot(x=iris[,1:4]
            y=iris$Species,
            plot="box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales=list(x=list(relation="free"),
                        y=list(relation="free")))
#something went wrong
library(featurePlot)





















