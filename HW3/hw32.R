rm(list = ls())
library(klaR)
library(caret)
med<-read.table("Diabetes.txt")
var<-as.data.frame(med[,5:10])
x11()
pairs(var, col=as.integer(var$V10))
legend(15,2,c("Class 1", "Class 2", "Class 3"), pch=1, col=c(1,2,3))

a <- sample(1:nrow(var), round(nrow(var)/2))
data<-na.omit(var)
train<-data[a,]
test<-data[-a,]
y_true_train<-train$V10
y_true_test<-test$V10

####LDA####
lda.fit <- lda(train$V10~., data = train)
lda.pred.train <- predict(lda.fit, newdata = train)
y_lda_train <- as.numeric(lda.pred.train$class)
lda.pred.test <- predict(lda.fit, newdata = test)
y_lda_test <- as.numeric(lda.pred.test$class)

lda_train_error <- sum(abs(y_true_train - y_lda_train))/length(y_true_train) # 0.22
lda_test_error <- sum(abs(y_true_test - y_lda_test))/length(y_true_test)  # 0.17
lda_train_error
lda_test_error


conf <- confusionMatrix(y_lda_test, y_true_test)
names(conf)
conf$table

####QDA####
qda.fit <- qda(train$V10~., data = train)
qda.pred.train <- predict(qda.fit, newdata = train)
y_qda_train <- as.numeric(qda.pred.train$class)
qda.pred.test <- predict(qda.fit, newdata = test)
y_qda_test <- as.numeric(qda.pred.test$class)

qda_train_error <- sum(abs(y_true_train - y_qda_train))/length(y_true_train) # 0.22
qda_test_error <- sum(abs(y_true_test - y_qda_test))/length(y_true_test)  # 0.17
qda_train_error
qda_test_error


conf <- confusionMatrix(y_qda_test, y_true_test)
names(conf)
conf$table

newTest=as.data.frame(cbind(0.98,122,544,186,184))
colnames(newTest)[1] <- "V5"
colnames(newTest)[2] <- "V6"
colnames(newTest)[3] <- "V7"
colnames(newTest)[4] <- "V8"
colnames(newTest)[5] <- "V9"
#colnames(newTest)[6] <- "V10"
lda.pred.test1 <- predict(lda.fit, newdata = newTest)
qda.pred.test1 <- predict(qda.fit, newdata = newTest)

lda.pred.test1$class
qda.pred.test1$class
