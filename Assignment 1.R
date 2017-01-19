#### Question 1
## Part 1
#Declare Libaries
library(kknn)

#Declare Constants
set.seed(99)
trainsize = 100
testsize = 10000


#Generate x's and errors
x.tr = rnorm(trainsize)
e.tr = rnorm(trainsize)
y.tr = 1.8*x.tr + 2 + e.tr  #Function of Problem
tr.df = data.frame(x=x.tr,y=y.tr) #Creates Dataframe for training data


x.ts = rnorm(testsize)
e.ts = rnorm(testsize)
y.ts = 1.8*x.ts + 2 + e.ts  #Function of Problem
ts.df = data.frame( x=x.ts,y=y.ts) #Creates Dataframe for test data

## Part 2
plot(tr.df, pch = 20, col = 'green',
     cex.main = 1, cex.lab = 1, cex.axis = 1, main = "Question 1.2 - Training and True Fit") #Plots training data per instructor 
abline(1.8, 2, col = "black", lwd = 4)

## Part 3
Q1.lm = lm(tr.df$y~tr.df$x) ## Uses training data
plot(tr.df, pch = 20, col = 'green',
     cex.main = 1, cex.lab = 1, cex.axis = 1, main = "Question 1.3 - Training, True Fit, Reg Line") #Plots training data per instructor 
abline(1.8, 2, col = "black", lwd = 4)
abline(Q1.lm$coef, col = "blue", lty = 2, lwd = 4)

## Part 4
test = data.frame(x = sort(tr.df$x))

par(mfrow=c(1,2),oma = c(0, 0, 2, 0)) # 1 x 2 Graph
#K = 2
plot(tr.df, pch = 20, col = 'green',
  cex.main = 1, cex.lab = 1, cex.axis = 1, main = "K = 2") #Plots training data per instructor 
abline(1.8, 2, col = "black", lwd = 4)
near = kknn(y~x, train = tr.df, test, k=2, kernel = "rectangular")
lines(test$x,near$fitted,col="red",type="l")

#K = 12
plot(tr.df, pch = 20, col = 'green', #Plots training data
  cex.main = 1, cex.lab = 1, cex.axis = 1, main = "K = 12") #Plots training data per instructor 
abline(1.8, 2, col = "black", lwd = 4)
near = kknn(y~x, train = tr.df, test, k=12, kernel = "rectangular")
lines(test$x,near$fitted,col="red",type="l")

mtext("Question 1.4", outer = TRUE, cex = 1.5)

## Part 5
kvec = 2:15; nk=length(kvec)
outMSE = rep(0,nk) #will put the OOS MSE here
for (i in 1:nk) {
  near = kknn(y~x, train = tr.df, ts.df, k=kvec[i], kernel = "rectangular")
  MSE = mean((ts.df$y-near$fitted)^2)
  outMSE[i] = MSE
}


par(mfrow=c(1,1)) # 1 x 1 Graph
plot(log(1/kvec),sqrt(outMSE), pch = 20, col = 'black', #Plots test data
     cex.main = 1, cex.lab = 1, cex.axis = 1, main = "RMSE as K Changes",
     ylim = c(.9, 1.4)) #Plots test data per instructor 
MSE = mean((ts.df$y-(ts.df$x*Q1.lm$coef[2]+Q1.lm$coef[1]))^2)
abline(sqrt(MSE),0, col = "red", lwd = 2)

## Part 6
#Generate x's and errors
x.tr = rnorm(trainsize)
e.tr = rnorm(trainsize)
y.tr = exp(x.tr + 1) + 3 + e.tr  #Function of Problem
tr.df = data.frame(x=x.tr,y=y.tr) #Creates Dataframe for training data


x.ts = rnorm(testsize)
e.ts = rnorm(testsize)
y.ts = exp(x.ts + 1) + 3 + e.ts  #Function of Problem
ts.df = data.frame( x=x.ts,y=y.ts) #Creates Dataframe for test data

x.real = seq(-3, 60, .1)
y.real = exp(x.real + 1) + 3 
real.df = data.frame( x=x.real,y=y.real) #Creates Real Relationship

## Part 2 of Part 6 redo
plot(tr.df, pch = 20, col = 'green',
     cex.main = 1, cex.lab = 1, cex.axis = 1, main = "Question 1.6.2 - Training and True Fit") #Plots training data per instructor 
lines(real.df)

## Part 3 of Part 6 redo
Q1.lm = lm(tr.df$y~tr.df$x) ## Uses training data
plot(tr.df, pch = 20, col = 'green',
     cex.main = 1, cex.lab = 1, cex.axis = 1, main = "Question 1.6.3 - Training, True Fit, Reg Line") #Plots training data per instructor 
lines(real.df)
abline(Q1.lm$coef, col = "blue", lty = 2, lwd = 4)

## Part 4  of Part 6 redo
test = data.frame(x = sort(tr.df$x))

par(mfrow=c(1,2),oma = c(0, 0, 2, 0)) # 1 x 2 Graph
#K = 2
plot(tr.df, pch = 20, col = 'green',
     cex.main = 1, cex.lab = 1, cex.axis = 1, main = "K = 2") #Plots training data per instructor 
lines(real.df)
near = kknn(y~x, train = tr.df, test, k=2, kernel = "rectangular")
lines(test$x,near$fitted,col="red",type="l")

#K = 12
plot(tr.df, pch = 20, col = 'green', #Plots training data
     cex.main = 1, cex.lab = 1, cex.axis = 1, main = "K = 12") #Plots training data per instructor 
lines(real.df)
near = kknn(y~x, train = tr.df, test, k=12, kernel = "rectangular")
lines(test$x,near$fitted,col="red",type="l")

mtext("Question 1.6.4", outer = TRUE, cex = 1.5)

## Part 5 of Part 6 redo
kvec = 2:15; nk=length(kvec)
outMSE = rep(0,nk) #will put the OOS MSE here
for (i in 1:nk) {
  near = kknn(y~x, train = tr.df, ts.df, k=kvec[i], kernel = "rectangular")
  MSE = mean((ts.df$y-near$fitted)^2)
  outMSE[i] = MSE
}


par(mfrow=c(1,1)) # 1 x 1 Graph
plot(log(1/kvec),sqrt(outMSE), pch = 20, col = 'black', #Plots test data
     cex.main = 1, cex.lab = 1, cex.axis = 1, main = "RMSE as K Changes",
     ylim = c(1, 4)) #Plots test data per instructor 
MSE = mean((ts.df$y-(ts.df$x*Q1.lm$coef[2]+Q1.lm$coef[1]))^2)
abline(sqrt(MSE),0, col = "red", lwd = 2)





## Part 7
#Generate x's and errors
x.tr = rnorm(trainsize)
e.tr = rnorm(trainsize)
y.tr = sin(2*x.tr) + 2 + e.tr  #Function of Problem
tr.df = data.frame(x=x.tr,y=y.tr) #Creates Dataframe for training data


x.ts = rnorm(testsize)
e.ts = rnorm(testsize)
y.ts = sin(2*x.ts) + 2 + e.ts #Function of Problem
ts.df = data.frame( x=x.ts,y=y.ts) #Creates Dataframe for test data

x.real = seq(-3, 60, .1)
y.real = sin(2*x.real) + 2
real.df = data.frame( x=x.real,y=y.real) #Creates Real Relationship

## Part 2 of Part 7 redo
plot(tr.df, pch = 20, col = 'green',
     cex.main = 1, cex.lab = 1, cex.axis = 1, main = "Question 1.7.2 - Training and True Fit") #Plots training data per instructor 
lines(real.df)

## Part 3 of Part 7 redo
Q1.lm = lm(tr.df$y~tr.df$x) ## Uses training data
plot(tr.df, pch = 20, col = 'green',
     cex.main = 1, cex.lab = 1, cex.axis = 1, main = "Question 1.7.3 - Training, True Fit, Reg Line") #Plots training data per instructor 
lines(real.df)
abline(Q1.lm$coef, col = "blue", lty = 2, lwd = 4)

## Part 4  of Part 7 redo
test = data.frame(x = sort(tr.df$x))

par(mfrow=c(1,2),oma = c(0, 0, 2, 0)) # 1 x 2 Graph
#K = 2
plot(tr.df, pch = 20, col = 'green',
     cex.main = 1, cex.lab = 1, cex.axis = 1, main = "K = 2") #Plots training data per instructor 
lines(real.df)
near = kknn(y~x, train = tr.df, test, k=2, kernel = "rectangular")
lines(test$x,near$fitted,col="red",type="l")

#K = 12
plot(tr.df, pch = 20, col = 'green', #Plots training data
     cex.main = 1, cex.lab = 1, cex.axis = 1, main = "K = 12") #Plots training data per instructor 
lines(real.df)
near = kknn(y~x, train = tr.df, test, k=12, kernel = "rectangular")
lines(test$x,near$fitted,col="red",type="l")

mtext("Question 1.7.4", outer = TRUE, cex = 1.5)

## Part 5 of Part 7 redo
kvec = 2:15; nk=length(kvec)
outMSE = rep(0,nk) #will put the OOS MSE here
for (i in 1:nk) {
  near = kknn(y~x, train = tr.df, ts.df, k=kvec[i], kernel = "rectangular")
  MSE = mean((ts.df$y-near$fitted)^2)
  outMSE[i] = MSE
}


par(mfrow=c(1,1)) # 1 x 1 Graph
plot(log(1/kvec),sqrt(outMSE), pch = 20, col = 'black', #Plots test data
     cex.main = 1, cex.lab = 1, cex.axis = 1, main = "RMSE as K Changes",
     ylim = c(1, 1.5)) #Plots test data per instructor 
MSE = mean((ts.df$y-(ts.df$x*Q1.lm$coef[2]+Q1.lm$coef[1]))^2)
abline(sqrt(MSE),0, col = "red", lwd = 2)



