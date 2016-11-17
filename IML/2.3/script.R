

distances = function(x, y, nx, ny){
  A = data.frame(x=c(), y=c())
  
  for(i in 1:nx){
    for(j in 1:ny){
      A[i, j] = sqrt(sum((x[i,] - y[j,])^2))
    }
  }  
  
  return(A)
}

classify_k_nearest = function(dists, j, k){
  nearest = c()
  for(i in 1:k){nearest = c(nearest, -1)}

  for(i in 1:length(dists[,j])){
    for(h in 1:k){
      if(nearest[h] == -1){ 
        nearest[h] = i
        break
        }
      else{
        if (dists[nearest[h],j] > dists[i,j]){
          nearest[h] = i
          break
        }  
      }
    }  
  }
  
  lbls = train$y[nearest]
  return(names(which.max(table(lbls))))
}

classify_all = function(n){
  correct_per_k = data.frame(k= 1:10, correct=1:10)
  for(k in 1:10){
    classifications = c()
    for(i in 1:n){
      classifications = c(classifications, classify_k_nearest(dists, i, k))
    }
    correct_per_k[k, 2] = sum(classifications == test$y[0:100]) 
    print(k)
  }
  
  return(correct_per_k)
}
