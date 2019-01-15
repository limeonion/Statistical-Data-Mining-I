#######HOMEWORK 1###########

#QUESTION 1
library("ISLR")
?Auto
dim(Auto)
names(Auto)
plot(mpg ~ ., data = Auto)

#QUESTION 2
reg <- lm(mpg~ ., data=Auto)
s <- summary(reg)
s
s$adj.r.squared

reg1 <- lm(mpg~ cylinders, data=Auto)
s1 <- summary(reg1)
cylinders1<-s1$adj.r.squared
cylinders1

reg2 <- lm(mpg~ displacement, data=Auto)
s2 <- summary(reg2)
d<-s2$adj.r.squared
d

reg3 <- lm(mpg~ horsepower, data=Auto)
s3 <- summary(reg3)
h<-s3$adj.r.squared
h

reg4 <- lm(mpg~ weight, data=Auto)
s4 <- summary(reg4)
w<-s4$adj.r.squared
w

reg5 <- lm(mpg~ acceleration, data=Auto)
s5 <- summary(reg5)
a<-s5$adj.r.squared
a
ls("package:ElemStatLearn")
reg6 <- lm(mpg~ year, data=Auto)
s6 <- summary(reg6)
y<-s6$adj.r.squared
s6$coefficients
y

confint(reg6)

#q4

library("MASS")
?Boston
x11()
plot(tax ~ indus, data=Boston)
x11()
plot(rm ~ lstat, data=Boston)
x11()
plot(lstat ~ black, data=Boston)
x11()
plot(indus ~ crim, data=Boston)
x11()
plot(dis ~ medv, data=Boston)
x11()
plot(zn ~ nox, data=Boston)


#q3

install.packages("ElemStatLearn")
?zip.test
?zip.train
i<-seq(1,13,2)
for (x in i){
  ggplot(train,test,x)
}
