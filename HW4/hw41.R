rm(list=ls())
library(MASS)
library(ElemStatLearn)
?prostate
data(prostate)
names(prostate)
library(leaps)
library(boot)
library(bootstrap)
prostate$train<-as.numeric(prostate$train)
a <- sample(1:nrow(prostate), round(nrow(prostate)/2))
trainset<-prostate[a,]
testset<-prostate[-a,]

fit<-lm(lpsa~., data=trainset)
aic<-extractAIC(fit)

error2<-cv.lm(testset,fit,m=10)

glm.fit = glm(lpsa~., data=prostate)
cv.error5 = cv.glm(prostate,glm.fit,K=5)
cv.error5$delta[1]

cv.error10 = cv.glm(prostate,glm.fit,K=10)
cv.error10$delta[1]


bestss <- regsubsets(trainset$lpsa~., data = trainset, nvmax = 10, method = "exhaustive")
sum.bss <- summary(bestss)

my_summary = summary(bestss)
names(my_summary)
my_summary$cp
my_summary$bic
min(my_summary$cp)
min(my_summary$bic)

which.min(my_summary$cp) 
which.min(my_summary$bic)


