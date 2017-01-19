## Part 8
#Generate x's and errors
p.loops = 20
reg.eq = "y~x"
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

## Part 5 of Part 8 redo
for (p in 2:p.loops) {
  p.tr = rnorm(trainsize)
  tr.df = cbind(tr.df, p.tr) #Combine columns
  colnames(tr.df)[p+1] <- paste('x', p, sep = "") #changes name to x3, x4, xp
  
  p.ts = rnorm(testsize)
  ts.df = cbind(ts.df, p.ts)  
  colnames(ts.df)[p+1] <- paste('x', p, sep = "")
  
  reg.eq = paste(reg.eq, paste('x', p, sep = ""), sep = "+") 
 
    near = kknn(reg.eq, train = tr.df, ts.df, k=2, kernel = "rectangular")
    MSE = mean((ts.df$y-near$fitted)^2)
    print(MSE)
}
