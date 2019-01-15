########################
#Question 1
########################
#install.packages("ISLR")
library(ISLR)
data(College)
?College
my_dats <- College[,1:18]

set.seed(123456)
#Spliting college data into train and test set
a <- sample(1:nrow(X), round(nrow(X)/2))
train<-my_dats[a,]
X.train<-train[,-2]
Y.train<-train[,2]
test<-my_dats[-a,]
X.test<-test[,-2]
Y.test<-test[,2]
#dim(train)
#dim(test)
names(train)

############################
#Applying linear regression
############################

reg<-lm(Apps ~ ., data= train)
#summary(reg)
train_MSE <- mean(reg$residuals^2)
train_MSE
LR_test_MSE1<-mean((Y.test - predict.lm(reg, test)) ^ 2)
LR_test_MSE1

#################
#Ridge Regression
#################

library(glmnet)
#ridge_in<-model.matrix(Y.train ~ X.train , data=train)[,-1]
College$Private<-as.numeric(College$Private)
train1<-College[a,]
X.train1<-train1[,-2]
Y.train1<-train1[,2]
test1<-College[-a,]
X.test1<-test1[,-2]
Y.test1<-test1[,2]
mat_train<-as.matrix(X.train1)
mat_test<-as.matrix(X.test1)
ridge.mod <- cv.glmnet(mat_train, Y.train1, alpha=0)
names(ridge.mod)
bestlam <- ridge.mod$lambda.min
ridge.pred2 <- predict(ridge.mod, s = bestlam, newx = mat_test, type = "response")
y_hat<-ridge.pred2
ridge_test_error <- sum((y_hat - Y.test1)^2)

#################
#LASSO Regression
#################
set.seed(12345)
library(glmnet)
lasso.mod <- cv.glmnet(mat_train, Y.train1, alpha=1)
names(lasso.mod)
bestlam1 <- lasso.mod$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam1, newx = mat_test, type = "response")
y_hat1<-lasso.pred
lasso_test_error <- sum((y_hat1 - Y.test1)^2)
predict(lasso.mod, s=bestlam1, type="coefficients")
#############################
#Principal Component Analysis
#############################

#install.packages("pls")
library(pls)
pcr.fit = pcr(Apps ~ . , data = train1, scale = TRUE, validation = "CV")
summary(pcr.fit)
training_error_store <- c()
test_error_store <- c()
for (i in 1:17){
  pcr.pred.train = predict(pcr.fit, train1, ncomp = i)
  pcr.pred.test = predict(pcr.fit, test1, ncomp = i)
  train.error <- mean((pcr.pred.train- Y.train1)^2)
  test.error <- mean((pcr.pred.test- Y.test1)^2)
  training_error_store <- c(training_error_store, train.error)
  test_error_store <- c(test_error_store, test.error)
}

x11()
plot(training_error_store)

x11()
plot(test_error_store)
minPCR<-min(test_error_store)
minPCR
#which.minPCR
summary(pcr.pred.test)

#####################
#Partial Least Square
#####################
pls.fit = plsr(Y.train ~ ., data = train, ncomp=17, scale = TRUE, validation = "CV")
summary(pls.fit)
pls.pred = predict(pls.fit, test, ncomp=17)
plsERROR<-mean((Y.test - data.frame(pls.pred))^2)

##################
#Comparison
##################
err.all <- c(LR_test_MSE1, ridge_test_error, lasso_test_error, minPCR, plsERROR)
names(err.all) <- c("lm", "ridge", "lasso", "pcr", "pls")
x11()
barplot(err.all)
print(err.all)
