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


#You may get an error "margin too large" 
#This means the plot area is too small for the large number of graphs
#Either expand your plot area (by manually changing the plot space)
#or decrease the number of graphs plotted
par(mfrow=c(4,5)) # 4 x 5 Graph
## Part 5 of Part 8 redo
for (p in 2:p.loops) {
  p.tr = rnorm(trainsize)
  tr.df = cbind(tr.df, p.tr) #Combine columns
  colnames(tr.df)[p+1] <- paste('x', p, sep = "") #changes name to x3, x4, xp
  
  p.ts = rnorm(testsize)
  ts.df = cbind(ts.df, p.ts)  
  colnames(ts.df)[p+1] <- paste('x', p, sep = "")
  
  reg.eq = paste(reg.eq, paste('x', p, sep = ""), sep = "+") 
  kvec = 2:15; nk=length(kvec)
  outMSE = rep(0,nk) 
  for (i in 1:nk) {
    near = kknn(reg.eq, train = tr.df, ts.df, k=kvec[i], kernel = "rectangular")
    MSE = mean((ts.df$y-near$fitted)^2)
    outMSE[i] = MSE
  }
  
  Q8.lm = lm(reg.eq, data = tr.df)
  plot(log(1/kvec),outMSE, pch = 20, col = 'black', #Plots test data
       cex.main = 1, cex.lab = 1, cex.axis = 1, main = paste("MSE for p = ", p, sep = ""),
       ylim = c(1, 2.3)) #Plots test data per instructor 
  MSE = mean((ts.df$y-(as.matrix(ts.df[, -2]) %*% as.matrix(Q8.lm$coef[-1])+Q8.lm$coef[1]))^2)
  abline(MSE,0, col = "red", lwd = 2)
}
