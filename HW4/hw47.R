rm(list=ls())
library(ISLR)
data(OJ)
set.seed(9004)
OJ$Purchase<-as.numeric(OJ$Purchase)-1
a <- sample(1:nrow(OJ), round(nrow(OJ)/2))
OJ.train = OJ[a, ]
OJ.test = OJ[-a, ]
library(e1071)
y_true.train<-OJ.train$Purchase
y_true.test<-OJ.test$Purchase

###########
#Linear kernel
###########
svm.linear = svm(Purchase ~ ., kernel = "linear", data = OJ.train, cost = 0.01)
summary(svm.linear)
train.pred = predict(svm.linear, OJ.train)
errorLintrain<-mean(abs(y_true.train-train.pred))
errorLintrain


test.pred = predict(svm.linear, OJ.test)
errorLintest<-mean(abs(y_true.test-test.pred))
errorLintest


set.seed(1554)
tune.out = tune(svm, Purchase ~ ., data = OJ.train, kernel = "linear", ranges = list(cost = 10^seq(-2, 
                                                                                                   1, by = 0.25)))
summary(tune.out)
svm.linear1 = svm(Purchase ~ ., kernel = "linear", data = OJ.train, cost = tune.out$best.parameters$cost)
train.pred1 = predict(svm.linear1, OJ.train)
test.pred1 = predict(svm.linear1, OJ.train)
errorLintrain1<-mean(abs(y_true.train-train.pred))
errorLintrain1

test.pred1 = predict(svm.linear1, OJ.test)
errorLintest1<-mean(abs(y_true.test-test.pred1))
errorLintest1

###########
#Radial kernel
###########

set.seed(410)
svm.radial = svm(Purchase ~ ., data = OJ.train, kernel = "radial")
summary(svm.radial)
train.pred = predict(svm.radial, OJ.train)
test.pred = predict(svm.radial, OJ.test)

errorradtrain<-mean(abs(y_true.train-train.pred))
errorradtrain

errorradtest<-mean(abs(y_true.test-test.pred))
errorradtest

set.seed(755)
tune.out = tune(svm, Purchase ~ ., data = OJ.train, kernel = "radial", ranges = list(cost = 10^seq(-2, 
                                                                                                   1, by = 0.25)))
summary(tune.out)
svm.radial = svm(Purchase ~ ., data = OJ.train, kernel = "radial", cost = tune.out$best.parameters$cost)
train.pred = predict(svm.radial, OJ.train)
test.pred = predict(svm.radial, OJ.test)

errorradtrain<-mean(abs(y_true.train-train.pred))
errorradtrain

errorradtest<-mean(abs(y_true.test-test.pred))
errorradtest

#######################
#Polynomial kernel with degree 2
#######################

set.seed(8112)
svm.poly = svm(Purchase ~ ., data = OJ.train, kernel = "poly", degree = 2)
summary(svm.poly)

train.pred = predict(svm.poly, OJ.train)
test.pred = predict(svm.poly, OJ.test)

errorPolytrain<-mean(abs(y_true.train-train.pred))
errorPolytrain

errorPolytest<-mean(abs(y_true.test-test.pred))
errorPolytest

set.seed(322)
tune.out = tune(svm, Purchase ~ ., data = OJ.train, kernel = "poly", degree = 2, 
                ranges = list(cost = 10^seq(-2, 1, by = 0.25)))
summary(tune.out)

svm.poly = svm(Purchase ~ ., data = OJ.train, kernel = "poly", degree = 2, cost = tune.out$best.parameters$cost)

train.pred = predict(svm.poly, OJ.train)
test.pred = predict(svm.poly, OJ.test)

errorPolytrain<-mean(abs(y_true.train-train.pred))
errorPolytrain

errorPolytest<-mean(abs(y_true.test-test.pred))
errorPolytest