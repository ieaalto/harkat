grid = expand.grid(1:2,1:3)

dists = matrix( c( .2, .4, 0, .1, .2, .1,
                   .6, .1, .1, .1, .1, 0,
                   .1, .3, .2, .4, 0, 0),nrow=6)


draw_sample = function(){
  class = sample(1:3, size=1, replace = TRUE, prob=c(0.4, 0.3, 0.3))
  x = grid[sample(1:6, 1, replace=TRUE, prob=dists[,class]),]
  return( c(class, x))
}

gen_data = function(n){
  data = data.frame(Y = numeric(), X1 = numeric(), X2 = numeric())
  
  for(i in 1:n){
    data[i,] = draw_sample()
  }
  
  return(data)
}

conditional_estimate = function(alpha = 0){
  frame = data.frame(class = numeric(), feature = numeric(),  value = numeric(), p = numeric())
  values = c(2,3)
  
  for(y in 1:3){
    A = train[train$Y == y,]
    nc = length(A$Y)
    for(x in 1:2){
      ncx = length(A[A$X1 == x,]$Y)
      p = (ncx + alpha) / (nc + 2*alpha)
      frame[length(frame$class)+1,] = c(y, 1, x, p) 
    }
    
    for(x in 1:3){
      ncx = length(A[A$X2 == x,]$Y)
      p = (ncx + alpha) / (nc + 3*alpha)
      frame[length(frame$class)+1,] = c(y, 2, x, p) 
    }

  }
  return(frame)
}

class_estimate = function(alpha = 0){
  frame = data.frame(Y = numeric(), p = numeric())
  
  n = length(train$Y)
  for(y in 1:3){
    nc = length(train[train$Y == y,]$Y)
    p = (nc + alpha)/ (n + 3*alpha)
    frame[length(frame$Y)+1, ] = c(y, p)
  }
  return(frame)
}

train = gen_data(6400)
estimates.class.ml = class_estimate(alpha = 0) 
estimates.class.lp = class_estimate(alpha = 1) 
estimates.class.kt = class_estimate(alpha = 0.5) 


classify = function(x, e, ec){
  
  
  px = 0
  for(y in 1:3){
    nc = length(train[train$Y == y,]$Y)
    px = px + nc*(e[e$class == y & e$feature == 1 & e$value == x[1], ]$p  *  e[e$class == y & e$feature == 2 & e$value == x[2], ]$p )
  }
  
  p1 = ec[ec$Y == 1, ]$p * e[e$class == 1 & e$feature == 1 & e$value == x[1], ]$p  *  e[e$class == 1 & e$feature == 2 & e$value == x[2], ]$p / px
  p2 = ec[ec$Y == 2, ]$p * e[e$class == 2 & e$feature == 1 & e$value == x[1], ]$p  *  e[e$class == 2 & e$feature == 2 & e$value == x[2], ]$p / px
  p3 = ec[ec$Y == 3, ]$p * e[e$class == 3 & e$feature == 1 & e$value == x[1], ]$p  *  e[e$class == 3 & e$feature == 2 & e$value == x[2], ]$p / px
  
  if(max(p1, p2, p3) == p1){ return(1)}
  else{ if(max(p2, p3) == p2){ return(2)}else{return(3)} }
  
}

classify_n = function(n, test){
  for (i in 1:n){
    c = classify(c(test[i, ]$X1, test[i,]$X2), estimates.laplace, estimates.class.lp)
    test[i, ]$estY = c
  }
  
  return(test)
}

estimates.ml = conditional_estimate(alpha = 0)
estimates.laplace = conditional_estimate(alpha = 1)
estimates.kt = conditional_estimate(alpha = 0.5)

estimates.class.ml = class_estimate(alpha = 0) 
estimates.class.lp = class_estimate(alpha = 1) 
estimates.class.kt = class_estimate(alpha = 0.5) 
