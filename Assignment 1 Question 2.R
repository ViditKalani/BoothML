#### Homework 1 Question 2
##############
# Out-of-Sample Prediction
##############
#Declare Constant
set.seed(99)
file.loc.root = 'C:/Users/Work/Dropbox/Business School/41204 - Machine Learning/Homework/'
cars <- read.csv(paste(file.loc.root, 'UsedCars.csv', sep = ""))

n=nrow(cars)
attach(cars)

ntrain = round(n*.75)

tr = sample(1:n, ntrain)
train=cars[tr,] #training data
test=cars[-tr,] #test data