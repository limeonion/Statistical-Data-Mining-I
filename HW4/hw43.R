rm(list=ls())
#install.packages("gbm")
library(magrittr)
library(rpart)
library(gbm)
library(randomForest)
library(class)

data(Seatbelts)
seatbelts<-as.data.frame(Seatbelts)
names(seatbelts)
seatbelts$law<-as.numeric(seatbelts$law)
a <- sample(1:nrow(seatbelts), round(nrow(seatbelts)/2))
train<-seatbelts[a,]
test<-seatbelts[-a,]
y_true<-test$law

###########
#Boosting
###########
boost.fit<-gbm(law~., data=train,n.trees = 1000,shrinkage = 0.1, interaction.depth = 3, distribution = "bernoulli")
boost.predict<-round(predict(boost.fit, newdata = test, n.trees = 1000, type = "response"))
error<-mean(abs(boost.predict - y_true))
error

boost.fit1<-gbm(law~., data=train,n.trees = 1000,shrinkage = 0.6, interaction.depth = 3, distribution = "bernoulli")
boost.predict1<-round(predict(boost.fit1, newdata = test, n.trees = 1000, type = "response"))
error1<-mean(abs(boost.predict1 - y_true))
error1
#########
##Bagging
#########
bag.fit <- randomForest(law~., data=train, n.tree=1000, mtry=length(seatbelts)-1)
x11()
varImpPlot(bag.fit)

bagy <- predict(bag.fit, newdata = test, type = 'response')
bagerror<-mean(abs(bagy-y_true))
bagerror
######
#Random Forest
######
rf.fit <- randomForest(law~., data=train, n.tree=1000)
x11()
varImpPlot(rf.fit)

rfy <- predict(rf.fit, newdata = test, type = 'response')
Errorrf <- mean(abs(y_true - rfy))
Errorrf

########
#Logistic regression
########

glm.fit <- glm(law ~., data = train, family = "binomial")
glm.probs.test <- predict(glm.fit, newdata = test, type = "response")
y_lr_test <- round(glm.probs.test)
Errorlr <- mean(abs(y_true - y_lr_test))

#######
#Knn
#######
knnError <- vector()
for(i in 1:5){
  knn.pred <- knn(train, test, cl=train$law, k=i)
  err.knn_cv1 <- mean(knn.pred!=test$law)
  knnError<-c(knnError,err.knn_cv1)
}
knnError
test.err_knn_CV <- min(knnError)
cat("Error in logistic regression is ",Errorlr)
cat("Error in KNN is ",test.err_knn_CV)
cat("Error in Random Forest is ",Errorrf)
cat("Error in bagging is ",bagerror)
cat("Error in boosting is ",error)
