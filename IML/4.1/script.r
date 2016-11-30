f.minus = function(x, y) { 
  return ( pnorm(x, mean = 0, sd = 1) * pnorm(y, mean=0, sd=1 ) )
}

f.plus = function(x, y) { 
  return ( pnorm(x, mean = 0, sd = 4) * pnorm(y, mean=0, sd=4 ) )
}

classify = function(x1, x2){
  a = f.plus(x1,x2)/(f.plus(x1,x2) + f.minus(x1,x2))
  
  #if(a > 0.5){
  #  return(1)
  #} else { return(-1)}
  
  return(a)
}

grid = expand.grid(.5*(-20:20), .5*(-20:20)) 

classes = c()

for(i in 1:length(grid[,1])){
  classes = c(classes, classify(grid[i, 1], grid[i,2]))
}

classes.mat = matrix(classes, nrow=sqrt(length(grid[,1])))

image(z=classes.mat)

pluses.x1 = rnorm(40, mean = 0, sd = 4)
pluses.x2 = rnorm(40, mean = 0, sd = 4)
pluses = data.frame(X1 = pluses.x1, X2 = pluses.x2)

minuses.x1 = rnorm(40, mean = 0, sd = 1)
minuses.x2 = rnorm(40, mean = 0, sd = 1)
minuses = data.frame(X1 = minuses.x1, X2 = minuses.x2)


