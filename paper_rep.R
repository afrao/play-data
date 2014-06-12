###### replicating paper

## installing packages
#install.packages("DMwR","e1071")


## loading libraries
library(DMwR)
library(e1071)
library(LiblineaR)


### probably will have to add some options to this depending on csv file
dat <- read.csv("put in the csv file in question")


dat <- read.csv("./paper_rep/abalone.data.txt",
                header=FALSE,colClasses=c("factor",rep("numeric",times=8)))
colnames(dat) <- c("class",colnames(dat)[2:9])
dat <- subset(dat,class %in% c("M","F"))


## have to make the class variable a factor for some reason
dat$class <- factor(dat$class)

# number of neighbors that smote depends on
k = 5
# percent oversampling we want (use multiples of 100)
perc.over = 200
perc.under = 100
## assuming that class vs everything else
## then getting the smoted data
new.dat <- SMOTE(class ~ ., data = dat,perc.over = perc.over,
                 k = k,perc.under=perc.under)

## getting all the classes hoping only 2 classes
classes <- unique(dat$class)


##
ratio.of.classes <- length(which(classes[1] == dat$class))/length(dat$class)
weights <- c(ratio.of.classes,(1-ratio.of.classes))
if(ratio.of.classes >= 1) {
    ratio.of.classes <- 1/ratio.of.classes
    small.class <- classes[2]
    weights <- c((1-ratio.of.classes),ratio.of.classes)
}
names(weights) <- classes

###


##
svm.model <- svm(class ~ .,data = new.dat,class.weights= weights)

class.labs <- factor(new.dat$class)
new.dat.mat <- new.dat[,2:9]
svm.model.liblinear <- LiblineaR(data = new.dat.mat,labels = class.labs,type=1,
                                 wi=weights)



 ### test
     data(iris)
     attach(iris)

     x=iris[,1:4]
     y=factor(iris[,5])
     train=sample(1:dim(iris)[1],100)

     xTrain=x[train,]
     xTest=x[-train,]
     yTrain=y[train]
     yTest=y[-train]

     # Center and scale data
     s=scale(xTrain,center=TRUE,scale=TRUE)

     # Logistic Regression
     t=3

     # Find the best model with the best cost parameter via 10-fold cross-validations
     tryTypes=c(0:7)
     tryCosts=c(1000,100,10,1,0.1,0.01,0.001)
     bestCost=NA
     bestAcc=0
     bestType=NA

     for(ty in tryTypes){
     for(co in tryCosts){
             acc=LiblineaR(data=s,labels=yTrain,type=ty,cost=co,bias=TRUE,cross=10,verbose=FALSE)
             cat("Results for C=",co," : ",acc," accuracy.\n",sep="")
             if(acc>bestAcc){
                     bestCost=co
                     bestAcc=acc
                     bestType=ty
             }
     }
     }

     cat("Best model type is:",bestType,"\n")
     cat("Best cost is:",bestCost,"\n")
     cat("Best accuracy is:",bestAcc,"\n")

     # Re-train best model with best cost value.
     m=LiblineaR(data=s,labels=yTrain,type=bestType,cost=bestCost,bias=TRUE,verbose=FALSE)

     # Scale the test data
     s2=scale(xTest,attr(s,"scaled:center"),attr(s,"scaled:scale"))

     # Make prediction
     pr=FALSE
     if(bestType==0 | bestType==7) pr=TRUE

     p=predict(m,s2,proba=pr,decisionValues=TRUE)

     # Display confusion matrix
     res=table(p$predictions,yTest)
     print(res)

     # Compute Balanced Classification Rate
     BCR=mean(c(res[1,1]/sum(res[,1]),res[2,2]/sum(res[,2]),res[3,3]/sum(res[,3])))
     print(BCR)





#### test
     ## A small example with a data set created artificially from the IRIS
     ## data
     data(iris)
     data <- iris[, c(1, 2, 5)]
     data$Species <- factor(ifelse(data$Species == "setosa","rare","common"))
     ## checking the class distribution of this artificial data set
     table(data$Species)

     ## now using SMOTE to create a more "balanced problem"
     newData <- SMOTE(Species ~ ., data, perc.over = 600,perc.under=100)
     table(newData$Species)
