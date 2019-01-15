rm(list = ls())

library(neuralnet)
library(ElemStatLearn)

data("spam")
spam$spam<-as.numeric(spam$spam)-1
n<-names(spam)
f <- as.formula(paste("spam ~", paste(n[!n %in% "spam"], collapse = " + ")))
nn <- neuralnet(f, data = spam, hidden = 1, err.fct = 'ce', linear.output = FALSE)

pr.nn <- compute(nn,test_[,1:57])
pr.nn <- compute(nn,spam[,1:57])
pr.nn_ <- pr.nn$net.result*(max(spam$spam)-min(spam$spam))+min(spam$spam)
test.r <- (spam$spam)*(max(spam$spam)-min(spam$spam))+min(spam$spam)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(spam)
MSE.nn

names(nn)
nn$result.matrix

x11()
plot(nn)