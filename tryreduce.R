data = read.csv(file.choose(),header = TRUE)
attach(data)


#class1
Class1.User0 = subset(data, Class == "1" & User == "0")[,1:17]
Class1.User0.new = na.omit(Class1.User0)
set.seed(1)
b = sample(1:nrow(Class1.User0.new), 800, replace=FALSE)
c1.u0 = Class1.User0.new[b,]
#setwd("C:/Users/Rose/Desktop/ERG project")
#write.table(c1.u0,"c1.uo.csv",sep=",")

#class2
Class2.User0 = subset(data, Class == "2" & User == "0")[,1:35]
Class2.User0.new = na.omit(Class2.User0)
c2.u0 = matrix(rep(NA,nrow(Class2.User0.new)*17), ncol=17)
for (i in 1:nrow(c2.u0)) {
  a0 = c(1,2,3,4,5,6,7,8,9,10,11)
  a1 = sample(a0, 5, replace=FALSE)
  a2 = c()
  for (j in a1){
    a2 = cbind(a2,j*3,j*3+1,j*3+2)
    }
  newline = Class2.User0.new[i,a2]
  for (k in 2:16) {
    c2.u0[i,k+1] = newline[,k-1]
    }
}
c2.u0[,2] = "0"
c2.u0[,1] = "2"
set.seed(2)
b = sample(1:nrow(Class2.User0.new), 800, replace=FALSE)
c2.u0 = c2.u0[b,]
#setwd("C:/Users/Rose/Desktop/ERG project")
#write.table(c2.u0,"c2.uo.csv",sep=",")

#class3
Class3.User0 = subset(data, Class == "3" & User == "0")[,1:17]
Class3.User0.new = na.omit(Class3.User0)
set.seed(3)
b = sample(1:nrow(Class3.User0.new), 800, replace=FALSE)
c3.u0 = Class3.User0.new[b,]
#setwd("C:/Users/Rose/Desktop/ERG project")
#write.table(c3.u0,"c3.uo.csv",sep=",")

#class4
Class4.User0 = subset(data, Class == "4" & User == "0")[,1:20]
Class4.User0.new = na.omit(Class4.User0)
c4.u0 = matrix(rep(NA,nrow(Class4.User0.new)*17), ncol=17)
for (i in 1:nrow(c4.u0)) {
  a0 = c(1,2,3,4,5,6)
  a1 = sample(a0, 5, replace=FALSE)
  a2 = c()
  for (j in a1){
    a2 = cbind(a2,j*3,j*3+1,j*3+2)
  }
  newline = Class4.User0.new[i,a2]
  for (k in 2:16) {
    c4.u0[i,k+1] = newline[,k-1]
  }
}
c4.u0[,2] = "0"
c4.u0[,1] = "4"
set.seed(4)
b = sample(1:nrow(Class4.User0.new), 800, replace=FALSE)
c4.u0 = c4.u0[b,]
#setwd("C:/Users/Rose/Desktop/ERG project")
#write.table(c4.u0,"c4.uo.csv",sep=",")

#class5
Class5.User0 = subset(data, Class == "5" & User == "0")[,1:35]
Class5.User0.new = na.omit(Class5.User0)
c5.u0 = matrix(rep(NA,nrow(Class5.User0.new)*17), ncol=17)
for (i in 1:nrow(c5.u0)) {
  a0 = c(1,2,3,4,5,6,7,8,9,10,11)
  a1 = sample(a0, 5, replace=FALSE)
  a2 = c()
  for (j in a1){
    a2 = cbind(a2,j*3,j*3+1,j*3+2)
  }
  newline = Class5.User0.new[i,a2]
  for (k in 2:16) {
    c5.u0[i,k+1] = newline[,k-1]
  }
}
c5.u0[,2] = "0"
c5.u0[,1] = "5"
set.seed(5)
b = sample(1:nrow(Class5.User0.new), 800, replace=FALSE)
c5.u0 = c5.u0[b,]
#setwd("C:/Users/Rose/Desktop/ERG project")
#write.table(c5.u0,"c5.uo.csv",sep=",")



library (e1071)
svmfit =svm(y???., data=dat, kernel ="linear", cost=10, scale=FALSE)
