set.seed(1)
x=rnorm(100)
y=x-2*x^2+rnorm(100)
simData<-data.frame(x,y)

#install.packages("bootstrap")
library(boot)
library(bootstrap)

fit1<-glm(y~x,data=simData)
error1<-cv.glm(simData,fit1)
error1$delta[1]

fit2<-glm(y~poly(x,2),data=simData)
error2<-cv.glm(simData,fit2)
error2$delta[1]

fit3<-glm(y~poly(x,3),data=simData)
error3<-cv.glm(simData,fit3)
error3$delta[1]

fit4<-glm(y~poly(x,4),data=simData)
error4<-cv.glm(simData,fit4)
error4$delta[1]

err.all1 <- c(error1$delta[1],error2$delta[1],error3$delta[1],error4$delta[1])

x11()
barplot(err.all1)
print(err.all1)
