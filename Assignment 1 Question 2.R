#### Homework 1 Question 2
library(rpart)      # package for trees
library(rpart.plot) # package that enhances plotting capabilities for rpart
library(MASS)  # contains boston housing data

##############
# Out-of-Sample Prediction
##############
#Declare Constant
set.seed(99)
file.loc.root = 'https://raw.githubusercontent.com/ChicagoBoothML/DATA___UsedCars/master/UsedCars.csv'
cars <- read.csv(file.loc.root)

n=nrow(cars)
attach(cars)

## Part 2
ntrain = round(n*.75)

tr = sample(1:n, ntrain)
train=cars[tr,] #training data
test=cars[-tr,] #test data

## Part 3
Q2.lm = lm(price~mileage, data = train)
par(mfrow=c(1,1)) # 1 x 1 Graph
plot(train$mileage,train$price,pch = 20, col = 'black', #Plots of all data
     cex.main = 1, cex.lab = 1, cex.axis = 1, main = "Plot of Price and Mileage")
abline(Q2.lm$coef, col = 'red', lwd = 2)

## Part 4
Q2.lm1 = lm(price~poly(mileage,1), data = train)
Q2.lm2 = lm(price~poly(mileage,2), data = train)
Q2.lm3 = lm(price~poly(mileage,3), data = train)
Q2.lm4 = lm(price~poly(mileage,4), data = train)
Q2.lm5 = lm(price~poly(mileage,5), data = train)

n.folds <- 10
poly <- 5
i.folds <- sample(rep(1:n.folds, length.out = n))
as.data.frame(table(i.folds))   ## OUTPUTS THE TABULATION OF samples for checking
cv <- matrix(NA, nrow = n.folds, ncol = ploy)


for(p in 1:poly) { ## Goes through polynomial

  for(k in 1:n.folds) { ## Goes through folds
    i.test = which (i.folds == k) ##Pulls out index of number
    train = cars[-i.test,]
    test  = cars[i.test, ]
    
    Q2.lm = lm(price~poly(mileage,p), data = train)
    fitted <- predict.lm(Q2.lm, test)
    
    cv[k, p] = mean((test$price- fitted)^2)
  }
}

cv.mean = apply(cv, 2, mean) ##Takes mean of our CV
cv.best = which.min(cv.mean)

## Plots the RMSE
rgy = range(cv)
plot(1:poly, cv[1,1:poly],type="l", ylim = rgy, col = 'grey', #Plots of all data
     cex.main = 1, cex.lab = 1, cex.axis = 1, main = "Plot of MSE | Polynomial Degree",
     xlab = "Degree Polynomial", ylab = "MSE")
for(i in 2:n.folds){ ## Add dem lines
  lines(1:poly, cv[i, 1:poly],type="l", col = 'grey')
}
lines(1:poly, cv.mean,type="l", col = 'red')

Q2.lm.best = lm(price~poly(mileage,cv.best), data = cars)  ##Use Cars or Training? This will have ramificatiosn above



###############################
#Q2.5 KNN and TREES

download.file("https://raw.githubusercontent.com/ChicagoBoothML/HelpR/master/docv.R", "docv.R")
source("docv.R") #this has docvknn used below
#do k-fold cross validation, 5 twice, 10 once
set.seed(99) #always set the seed! 
kv = 2:100 #these are the k values (k as in kNN) we will try
#docvknn(matrix x, vector y,vector of k values, number of folds),
#does cross-validation for training data (x,y).
cv1 = docvknn(matrix(cars$mileage,ncol=1),cars$price,kv,nfold=5)
cv2 = docvknn(matrix(cars$mileage,ncol=1),cars$price,kv,nfold=5)
cv3 = docvknn(matrix(cars$mileage,ncol=1),cars$price,kv,nfold=10)
#docvknn returns error sum of squares, want RMSE
cv1 = sqrt(cv1/length(cars$price))
cv2 = sqrt(cv2/length(cars$price))
cv3 = sqrt(cv3/length(cars$price))

#plot
rgy = range(c(cv1,cv2,cv3))
plot(log(1/kv),cv1,type="l",col="red",ylim=rgy,lwd=2,cex.lab=2.0, xlab="log(1/k)", ylab="RMSE")
lines(log(1/kv),cv2,col="blue",lwd=2)
lines(log(1/kv),cv3,col="green",lwd=2)
legend("topleft",legend=c("5-fold 1","5-fold 2","10 fold"),
       col=c("red","blue","green"),lwd=2,cex=1.5)
#get the min
cv = (cv1+cv2+cv3)/3 #use average
kbest = kv[which.min(cv)]
cat("the best k is: ",kbest,"\n")
#fit kNN with best k and plot the fit.
kfbest = kknn(medv~lstat,Boston,data.frame(lstat=sort(Boston$lstat)),
              k=kbest,kernel = "rectangular")
plot(Boston$lstat,Boston$medv,cex.lab=1.2)
lines(sort(Boston$lstat),kfbest$fitted,col="red",lwd=2,cex.lab=2)







###############################

# fit a tree to boston data just using lstat.
# first get a big tree by appropriately setting rpart.control
#
# see help by typing ?rpart.control for more details
# you will notice that you can set
#   - minsplit - which sets the minimum number of observations needed in a node to consider a plit
#   - cp - splits are considered only if the fit is improved by amount controled by cp
#
#
temp = rpart(medv~lstat, data=Boston, 
             control=rpart.control(minsplit=5,  # we want to have at least 5 observations in a node
                                   cp=0.001,    # we will consider even small improvements in a fit 
                                   xval=0)      # do not run cross-validation now
)
rpart.plot(temp)
#if the tree is too small, make cp smaller!!

# then prune it down to one with 7 leaves
# again, we control the size of the tree through cp 
# which is not the most intuitive of measures
boston.tree = prune(temp, cp=0.01)
rpart.plot(boston.tree)

## plot data with fit
boston.fit = predict(boston.tree) #get training fitted values
plot(Boston$lstat, Boston$medv, cex=.5, pch=16) #plot data
oo=order(Boston$lstat)
lines(Boston$lstat[oo],boston.fit[oo],col="red",lwd=3) #step function fit

# predict at lstat = 15 and 25.
preddf = data.frame(lstat=c(15,25))
yhat = predict(boston.tree,preddf)
points(preddf$lstat,yhat,col="blue",pch="*",cex=3)


