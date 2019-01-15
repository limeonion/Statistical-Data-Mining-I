rm(list=ls())
library(rpart)
library(randomForest)
library(geneplotter)
library(gbm)

winedata<-read.csv("wine.txt")
#data(winedata)
a <- sample(1:nrow(prostate), round(nrow(prostate)/2))
trainwine<-winedata[a,]
testwine<-winedata[-a,]
model.control<-rpart.control(minsplit = 5,xval = 10, cp=0)
treefit<-rpart(class~., data=trainwine, method = "class", control = model.control)
x11()
plot(treefit, uniform = T, compress = T)
text(treefit, cex = 0.5)

x11()

plot(treefit, uniform = T, compress = T)
text(treefit, use.n = T, all = T, cex = 0.1)

x11()
plot(treefit, branch = .4, uniform = T, compress = T)
text(treefit, use.n = T, all = T, cex = 1)



x11()
plot(treefit$cptable[,4], main = "Cp for model selection", ylab = "cv error")

min_cp = which.min(treefit$cptable[,4])
pruned_fit_dig <- prune(treefit, cp = treefit$cptable[min_cp,1])

x11()
plot(pruned_fit_dig, branch = .3, compress=T, main = "Pruned Tree")
text(pruned_fit_dig, cex = .5)

x11()
plot(treefit, branch = .3, compress=T, main = "Full Tree")
text(treefit, cex = .5)
