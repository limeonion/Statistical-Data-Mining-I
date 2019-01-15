rm(list = ls())
#install.packages("caret")
library(caret)
library(ISLR)
library(MASS)
data(Boston)
Boston$res<-0
Boston$res[Boston$crim> median(Boston$crim)]<-1
t=cor(Boston)
t=t[1,]
o=order(abs(t))

#########################
#For Best 5 predictors
#########################
col=tail(o,5)
set.seed(123)
a <- sample(1:nrow(Boston), round(nrow(Boston)/2))
data<-na.omit(Boston)
train<-data[a,c(col,15)]
test<-data[-a,c(col,15)]
glm.fit <- glm(res ~., data = train, family = "binomial")
summary(glm.fit)
names(glm.fit)
glm.probs.train <- predict(glm.fit, newdata = train, type = "response")
y_lr_train <- round(glm.probs.train)
glm.probs.test <- predict(glm.fit, newdata = test, type = "response")
y_lr_test <- round(glm.probs.test)

y_true_train<-train$res
y_true_test<-test$res

train_err <- sum(abs(y_lr_train- y_true_train))/length(y_true_train)
test_err <- sum(abs(y_lr_test- y_true_test))/length(y_true_test)

train_err
test_err

conf <- confusionMatrix(y_lr_test, y_true_test)
names(conf)
conf$table

######
#LDA
######
lda.fit <- lda(res~., data = train)
lda.pred.train <- predict(lda.fit, newdata = train)
y_lda_train <- as.numeric(lda.pred.train$class)-1
lda.pred.test <- predict(lda.fit, newdata = test)
y_lda_test <- as.numeric(lda.pred.test$class)-1

lda_train_error <- sum(abs(y_true_train - y_lda_train))/length(y_true_train) # 0.22
lda_test_error <- sum(abs(y_true_test - y_lda_test))/length(y_true_test)  # 0.17
lda_train_error
lda_test_error


conf <- confusionMatrix(y_lda_test, y_true_test)
names(conf)
conf$table

###
#KNN
####
library(class)
knnError <- vector()
for(i in 5:14){
  knn.pred <- knn(train, test, cl=train$res, k=i)
  err.knn_cv1 <- mean(knn.pred!=test$res)
  knnError<-c(knnError,err.knn_cv1)
}
knnError
test.err_knn_CV <- min(knnError)
cat("Error in logistic regression is ",test_err)
cat("Error in LDA is ",lda_test_error)
cat("Error in KNN is ",test.err_knn_CV)

#########################
#For Best 10 predictors
#########################
col=tail(o,10)
set.seed(123)
a <- sample(1:nrow(Boston), round(nrow(Boston)/2))
data<-na.omit(Boston)
train<-data[a,c(col)]
test<-data[-a,c(col)]
glm.fit <- glm(res ~., data = train, family = "binomial")
summary(glm.fit)
names(glm.fit)
glm.probs.train <- predict(glm.fit, newdata = train, type = "response")
y_lr_train <- round(glm.probs.train)
glm.probs.test <- predict(glm.fit, newdata = test, type = "response")
y_lr_test <- round(glm.probs.test)

y_true_train<-train$res
y_true_test<-test$res

train_err <- sum(abs(y_lr_train- y_true_train))/length(y_true_train)
test_err <- sum(abs(y_lr_test- y_true_test))/length(y_true_test)

train_err
test_err

conf <- confusionMatrix(y_lr_test, y_true_test)
names(conf)
conf$table

######
#LDA
######
lda.fit <- lda(res~., data = train)
lda.pred.train <- predict(lda.fit, newdata = train)
y_lda_train <- as.numeric(lda.pred.train$class)-1
lda.pred.test <- predict(lda.fit, newdata = test)
y_lda_test <- as.numeric(lda.pred.test$class)-1

lda_train_error <- sum(abs(y_true_train - y_lda_train))/length(y_true_train) 
lda_test_error <- sum(abs(y_true_test - y_lda_test))/length(y_true_test) 
lda_train_error
lda_test_error


conf <- confusionMatrix(y_lda_test, y_true_test)
names(conf)
conf$table

###
#KNN
####
library(class)
knnError <- vector()
for(i in 5:14){
  knn.pred <- knn(train, test, cl=train$res, k=i)
  err.knn_cv1 <- mean(knn.pred!=test$res)
  knnError<-c(knnError,err.knn_cv1)
}
knnError
test.err_knn_CV <- min(knnError)
cat("Error in logistic regression is ",test_err)
cat("Error in LDA is ",lda_test_error)
cat("Error in KNN is ",test.err_knn_CV)

#########################
#For All the predictors
#########################

set.seed(123)
a <- sample(1:nrow(Boston), round(nrow(Boston)/2))
data<-na.omit(Boston)
train<-data[a,]
test<-data[-a,]
glm.fit <- glm(res ~., data = train, family = "binomial")
summary(glm.fit)
names(glm.fit)
glm.probs.train <- predict(glm.fit, newdata = train, type = "response")
y_lr_train <- round(glm.probs.train)
glm.probs.test <- predict(glm.fit, newdata = test, type = "response")
y_lr_test <- round(glm.probs.test)

y_true_train<-train$res
y_true_test<-test$res

train_err <- sum(abs(y_lr_train- y_true_train))/length(y_true_train)
test_err <- sum(abs(y_lr_test- y_true_test))/length(y_true_test)

train_err
test_err

conf <- confusionMatrix(y_lr_test, y_true_test)
names(conf)
conf$table

######
#LDA
######
lda.fit <- lda(res~., data = train)
lda.pred.train <- predict(lda.fit, newdata = train)
y_lda_train <- as.numeric(lda.pred.train$class)-1
lda.pred.test <- predict(lda.fit, newdata = test)
y_lda_test <- as.numeric(lda.pred.test$class)-1

lda_train_error <- sum(abs(y_true_train - y_lda_train))/length(y_true_train) 
lda_test_error <- sum(abs(y_true_test - y_lda_test))/length(y_true_test) 
lda_train_error
lda_test_error


conf <- confusionMatrix(y_lda_test, y_true_test)
names(conf)
conf$table

###
#KNN
####
library(class)
knnError <- vector()
for(i in 5:14){
  knn.pred <- knn(train, test, cl=train$res, k=i)
  err.knn_cv1 <- mean(knn.pred!=test$res)
  knnError<-c(knnError,err.knn_cv1)
}
knnError
test.err_knn_CV <- min(knnError)
cat("Error in logistic regression is ",test_err)
cat("Error in LDA is ",lda_test_error)
cat("Error in KNN is ",test.err_knn_CV)
