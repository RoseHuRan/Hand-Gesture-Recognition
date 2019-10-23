##################
######RAN HU######
##################

######check the number of missing values######

data = read.csv(file.choose())
View(data)
data = read.csv(file.choose(),header = TRUE)
attach(data)
library(mice)
a=md.pattern(data[,1:38])


######the first pruned data###### 

#class1
Class1 = subset(data, Class == "1")[,1:17]
Class1.new = na.omit(Class1)
set.seed(1)
b = sample(1:nrow(Class1.new), 3000, replace=FALSE)
c1 = Class1.new[b,]
setwd("C:/Users/Rose/Desktop/ERG project")
write.table(c1,"c1.csv",sep=",")

#class2
Class2 = subset(data, Class == "2")[,1:35]
Class2.new = na.omit(Class2)
c2 = matrix(rep(NA,nrow(Class2.new)*17), ncol=17)
for (i in 1:nrow(c2)) {
  a0 = c(1,2,3,4,5,6,7,8,9,10,11)
  a1 = sample(a0, 5, replace=FALSE)
  a2 = c()
  for (j in a1){
    a2 = cbind(a2,j*3,j*3+1,j*3+2)
  }
  newline = Class2.new[i,a2]
  for (k in 2:16) {
    c2[i,k+1] = newline[,k-1]
  }
}
c2[,2] = "0"
c2[,1] = "2"
set.seed(2)
b = sample(1:nrow(Class2.new), 3000, replace=FALSE)
c2 = c2[b,]
setwd("C:/Users/Rose/Desktop/ERG project")
write.table(c2,"c2.csv",sep=",")

#class3
Class3 = subset(data, Class == "3")[,1:17]
Class3.new = na.omit(Class3)
set.seed(3)
b = sample(1:nrow(Class3.new), 3000, replace=FALSE)
c3 = Class3.new[b,]
setwd("C:/Users/Rose/Desktop/ERG project")
write.table(c3,"c3.csv",sep=",")

#class4
Class4 = subset(data, Class == "4")[,1:20]
Class4.new = na.omit(Class4)
c4 = matrix(rep(NA,nrow(Class4.new)*17), ncol=17)
for (i in 1:nrow(c4)) {
  a0 = c(1,2,3,4,5,6)
  a1 = sample(a0, 5, replace=FALSE)
  a2 = c()
  for (j in a1){
    a2 = cbind(a2,j*3,j*3+1,j*3+2)
  }
  newline = Class4.new[i,a2]
  for (k in 2:16) {
    c4[i,k+1] = newline[,k-1]
  }
}
c4[,2] = "0"
c4[,1] = "4"
set.seed(4)
b = sample(1:nrow(Class4.new), 3000, replace=FALSE)
c4 = c4[b,]
setwd("C:/Users/Rose/Desktop/ERG project")
write.table(c4,"c4.csv",sep=",")

#class5
Class5 = subset(data, Class == "5")[,1:35]
Class5.new = na.omit(Class5)
c5 = matrix(rep(NA,nrow(Class5.new)*17), ncol=17)
for (i in 1:nrow(c5)) {
  a0 = c(1,2,3,4,5,6,7,8,9,10,11)
  a1 = sample(a0, 5, replace=FALSE)
  a2 = c()
  for (j in a1){
    a2 = cbind(a2,j*3,j*3+1,j*3+2)
  }
  newline = Class5.new[i,a2]
  for (k in 2:16) {
    c5[i,k+1] = newline[,k-1]
  }
}
c5[,2] = "0"
c5[,1] = "5"
set.seed(5)
b = sample(1:nrow(Class5.new), 3000, replace=FALSE)
c5 = c5[b,]
setwd("C:/Users/Rose/Desktop/ERG project")
write.table(c5,"c5.csv",sep=",")


######the second pruned data###### 

#test data  
test.all = data[,1:11][,-2]
setwd("C:/Users/116010078/Desktop/data")
write.table(test.all,"test.all.csv",sep=",")
View(test.all)

#training data
data = read.csv(file.choose())
train.all = data[,1:11][,-2]
setwd("C:/Users/116010078/Desktop/data")
write.table(train.all,"train.all.csv",sep=",")
View(train.all)


######support vector machine######

#linear
svm.fit = svm(Class~., data = traindata, kernal = "linear", cost = 10, scale = TRUE)
pred.svm = predict(svm.fit, testdata)
for (i in 1:length(pred.svm)) {
  if (pred.svm[i]<1.5)
    pred.svm[i] = 1
  if (pred.svm[i]<2.5&pred.svm[i]>1.5)  
    pred.svm[i] = 2
  if (pred.svm[i]<3.5&pred.svm[i]>2.5)  
    pred.svm[i] = 3
  if (pred.svm[i]<4.5&pred.svm[i]>3.5)  
    pred.svm[i] = 4
  if (pred.svm[i]>4.5)  
    pred.svm[i] = 5
} 
table(true=testdata[,1],pred.svm)
mean(testdata[,1]==pred.svm)
#find the best cost, but failded
set.seed(1)
tune.out = tune(svm, Class~., data=trainset, kernel = "linear", ranges = list(cost = c(0.01,1,1000)))
summary(tune.out)
pred.svm = predict(bestmodel, testset)

#non-linear 
svm.fit2 = svm(Class~.,data=trainset,kernel="radial",gamma=5,cost=10, scale = TRUE)
tune.out.2 = tune(svm,Class~.,data=trainset,kernel="radial",
                  ranges=list(cost=c(0.1,10,1000)),gamma = c(0.5,1,5))
summary(tune.out.2)
pred.svm2 = predict(svm.fit2, testset)
pred.svm2
View(testset)
plot(tune.out.2$best.model,trainset)
table(true=data0[testindex,"Class"],pred=predict(tune.out.2$best.model,news=data0.new[testindex]))


######LDA and QDA######
library(MASS)

#LDA
lda.fit = lda(Class~.,data=traindata)
lda.prd=predict(lda.fit, testdata)
table(lda.prd$class,testdata[,1])
mean(lda.prd$class==testdata[,1])

#QDA
qda.fit = qda(Class~.,data=traindata)
qda.prd=predict(qda.fit, testdata)
table(qda.prd$class,testdata[,1])
mean(qda.prd$class==testdata[,1])


######KNN######

#training error
test.u6 = subset(traindata2, User == "6")
train.u6 = subset(traindata2, User != "6")
library(class)
set.seed(1)
knn.pred=knn(train.u6[,-1],test.u6[,-1],train.u6[,1],k=1)
table(knn.pred,test.u6[,1])
mean(knn.pred==test.u6[,1])

#training error after scaling the data
library(class)
standardized.train = scale(train.u6[,-1])
standardized.test = scale(test.u6[,-1])
set.seed(1)
knn.pred.standardized=knn(standardized.train,standardized.test,train.u6[,1],k=1)
table(knn.pred.standardized,test.u6[,1])
mean(knn.pred.standardized==test.u6[,1])

#test error
library(class)
set.seed(1)
knn.pred=knn(traindata,testdata,traindata[,1],k=1)
table(knn.pred,testdata[,1])
mean(knn.pred==testdata[,1])

