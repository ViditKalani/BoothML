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

###############################
#Q2.2 - Q2.4 
###############################
## Part 2
ntrain = round(n*.75)

tr = sample(1:n, ntrain)
train=cars[tr,] #training data
test=cars[-tr,] #test data

## Part 3
options(scipen=999)
Q2.lm = lm(price~mileage, data = train)
par(mfrow=c(1,1)) # 1 x 1 Graph
plot(train$mileage,train$price,pch = 20, col = 'black', #Plots of all data
     cex.main = 1, cex.lab = 1, cex.axis = .75, main = "Plot of Price and Mileage",
     ylab = "Price", xlab = "Mileage")
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
cv <- matrix(NA, nrow = n.folds, ncol = poly)


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
#Q2.5 KNN 
###############################


download.file("https://raw.githubusercontent.com/ChicagoBoothML/HelpR/master/docv.R", "docv.R")
source("docv.R") #this has docvknn used below
#do k-fold cross validation, 5 twice, 10 once
set.seed(99) #always set the seed! 
kv = c(seq(2, 10, 2),seq(10, 100, 10)) #these are the k values (k as in kNN) we will try
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
kfbest = kknn(price~mileage,cars,data.frame(mileage=sort(cars$mileage)),
              k=kbest,kernel = "rectangular")
plot(cars$mileage,cars$price,cex.lab=1.2)
lines(sort(cars$mileage),kfbest$fitted,col="red",lwd=2,cex.lab=2)

###############################
#Q2.5 TREE 
###############################
big.tree = rpart(price~mileage, data=cars, 
             control=rpart.control(minsplit=5,  # we want to have at least 5 observations in a node
                                   cp=0.0001,    # we will consider even small improvements in a fit 
                                   xval=10))
rpart.plot(big.tree)

nbig = length(unique(big.tree$where))
cat('size of big tree: ',nbig,'\n')

# let us look at the cross-validation results
#
# the following prints out a table summarizing the output of cross-validation
# you want to find cp corrsponding to the smallest value of xerror 
(cptable = printcp(big.tree))
(bestcp = cptable[ which.min(cptable[,"xerror"]), "CP" ])   # this is the optimal cp parameter

plotcp(big.tree) # plot results

# show fit from some trees
oo = order(cars$mileage)
cpvec = c(.0157,bestcp,.004)

par(mfrow=c(3,2))
for(i in 1:3) {
  plot(cars$mileage,cars$price,pch=16,col='blue',cex=.5)
  ptree = prune(big.tree,cp=cpvec[i])
  pfit = predict(ptree)
  lines(cars$mileage[oo],pfit[oo],col='red',lwd=2)
  title(paste('alpha = ',round(cpvec[i],3)))
  rpart.plot(ptree)
}

par(mfrow=c(1,1))
best.tree = prune(big.tree,cp=bestcp)
rpart.plot(best.tree)

###############################
#Q2.5 BEST 
###############################

plot(cars$mileage,cars$price,pch = 20, col = 'black', #Plots of all data
     cex.main = 1, cex.lab = 1, cex.axis = .75, main = "Plot of Price and Mileage",
     ylab = "Price", xlab = "Mileage")
#Adds Reg Line
pfit = predict(Q2.lm.best, data = cars$mileage)
lines(cars$mileage[oo],pfit[oo],col='green',lwd=2) ## Reg
#Adds KNN Line
lines(sort(cars$mileage),kfbest$fitted,col="blue",lwd=2,cex.lab=2)
#Adds Tree Line
pfit = predict(best.tree)
lines(cars$mileage[oo],pfit[oo],col='red',lwd=2) ## Tree
legend("topright",legend=c("Regression","KNN","Tree"),
       col=c("green","blue","red"),lwd=2,cex=.75)

###############################
#Q2.6 MILEAGE AND YEAR
###############################

###############################
#Q2.6 KNN 
###############################
download.file("https://raw.githubusercontent.com/ChicagoBoothML/HelpR/master/docv.R", "docv.R")
source("docv.R") #this has docvknn used below
#do k-fold cross validation, 5 twice, 10 once
set.seed(99) #always set the seed! 
kv = c(seq(2, 10, 2),seq(10, 100, 10)) #these are the k values (k as in kNN) we will try
#docvknn(matrix x, vector y,vector of k values, number of folds),
#does cross-validation for training data (x,y).
cv1 = docvknn(matrix(c(cars$mileage, cars$year),ncol=2),cars$price,kv,nfold=5)
cv2 = docvknn(matrix(c(cars$mileage, cars$year),ncol=2),cars$price,kv,nfold=5)
cv3 = docvknn(matrix(c(cars$mileage, cars$year),ncol=2),cars$price,kv,nfold=10)
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

## This might need adjusting
kfbest = kknn(price~mileage+year,cars,data.frame(mileage=sort(cars$mileage)),
              k=kbest,kernel = "rectangular")
plot(cars$mileage,cars$price,cex.lab=1.2)
lines(sort(cars$mileage),kfbest$fitted,col="red",lwd=2,cex.lab=2)

###############################
#Q2.6 TREE '''Add year into reg eq'
###############################
big.tree = rpart(price~mileage+year, data=cars, 
                 control=rpart.control(minsplit=5,  # we want to have at least 5 observations in a node
                                       cp=0.0001,    # we will consider even small improvements in a fit 
                                       xval=10))
rpart.plot(big.tree)

nbig = length(unique(big.tree$where))
cat('size of big tree: ',nbig,'\n')

# let us look at the cross-validation results
#
# the following prints out a table summarizing the output of cross-validation
# you want to find cp corrsponding to the smallest value of xerror 
(cptable = printcp(big.tree))
(bestcp = cptable[ which.min(cptable[,"xerror"]), "CP" ])   # this is the optimal cp parameter

## Finds improved xerror
min(cptable[,"xerror"])
## Min xerror .2734326 vs .08866526

plotcp(big.tree) # plot results

# show fit from some trees
oo = order(cars$mileage)
cpvec = c(.0157,bestcp,.004)

par(mfrow=c(3,2))
for(i in 1:3) {
  plot(cars$mileage,cars$price,pch=16,col='blue',cex=.5)
  ptree = prune(big.tree,cp=cpvec[i])
  pfit = predict(ptree)
  lines(cars$mileage[oo],pfit[oo],col='red',lwd=2)
  title(paste('alpha = ',round(cpvec[i],3)))
  rpart.plot(ptree)
}

par(mfrow=c(1,1))
best.tree = prune(big.tree,cp=bestcp)
rpart.plot(best.tree)

###############################
#Q2.7 TREE '''Use . to utilize all variables
###############################
big.tree = rpart(price~., data=cars, 
                 control=rpart.control(minsplit=5,  # we want to have at least 5 observations in a node
                                       cp=0.0001,    # we will consider even small improvements in a fit 
                                       xval=10))
rpart.plot(big.tree)

nbig = length(unique(big.tree$where))
cat('size of big tree: ',nbig,'\n')

# let us look at the cross-validation results
#
# the following prints out a table summarizing the output of cross-validation
# you want to find cp corrsponding to the smallest value of xerror 
(cptable = printcp(big.tree))
(bestcp = cptable[ which.min(cptable[,"xerror"]), "CP" ])   # this is the optimal cp parameter

## Finds improved xerror
min(cptable[,"xerror"])
## Min xerror now .05754176

plotcp(big.tree) # plot results

# show fit from some trees
oo = order(cars$mileage)
cpvec = c(.0157,bestcp,.004)

par(mfrow=c(3,2))
for(i in 1:3) {
  plot(cars$mileage,cars$price,pch=16,col='blue',cex=.5)
  ptree = prune(big.tree,cp=cpvec[i])
  pfit = predict(ptree)
  lines(cars$mileage[oo],pfit[oo],col='red',lwd=2)
  title(paste('alpha = ',round(cpvec[i],3)))
  rpart.plot(ptree)
}

par(mfrow=c(1,1))
best.tree = prune(big.tree,cp=bestcp)
rpart.plot(best.tree)