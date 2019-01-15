gen.data<-replicate(20,sample(15,1000,rep=TRUE))
dm<-as.matrix(gen.data)
betas=matrix(sample.int(15, 20*1, TRUE), 20, 1)
z<-round(runif(8,1,20))
betas[z]<-0
e=matrix(sample.int(5, 1000*1, TRUE), 1000, 1)
y=dm%*%betas+e

set<-cbind(dm,y)
dim(set)
dfset<-as.data.frame(set)
index<-sample(1:1000,100)

trainSet<-dfset[index,]
testSet<-dfset[-index,]

#install.packages("leaps")
library(leaps)
bestss <- regsubsets(V21~., data = trainSet, nvmax = 20, method = "exhaustive")
sum.bss <- summary(bestss)
sum.bss
par(mfrow = c(2,2))
x11()
plot(sum.bss$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
which(sum.bss$cp == min(sum.bss$cp))
which(sum.bss$bic == min(sum.bss$bic))

bss.te <- rep(NA, 20)
for(i in 1:20) {
  pred.full <- predict.regsubsets(bestss, testSet, id=i)
  bss.te[i] <- mean((testSet[,21] - pred.full)^2)
}
x11()
plot(1:20, bss.te, type="b", main="Test MSE", xlab="Number of Predictors")
bss.te
which.min(bss.te)
coef(bestss, id = 13)
betas