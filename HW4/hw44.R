rm(list=ls())
library(randomForest)
library(geneplotter)
library(caret)
data("spam")
a <- sample(1:nrow(spam), round(nrow(spam)/2))
spam$spam<-as.numeric(spam$spam)-1
train<-spam[a,]
test<-spam[-a,]
y_true<-test$spam

rf.fit <- randomForest(spam~., data=spam, n.tree=1000)
x11()
varImpPlot(rf.fit)

rfy <- round(predict(rf.fit, newdata = test, type = 'response'))
Errorrf <- mean(abs(y_true - rfy))
Errorrf
x11()
plot(rf.fit)

t <- tuneRF(train[,-58], train[,58],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 500,
            trace = TRUE,
            improve = 0.05)
varUsed(rf.fit)

