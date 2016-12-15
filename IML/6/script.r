


k_means = function(k, data, means=NULL){
  clusters = sample(1:k, nrow(data), replace = T)
  
  while(T){
    centroids = matrix(rep(0,k*ncol(data)), nrow =  k)
    
    if(is.null(means)){
      for(i in 1:k){centroids[i, ] = colMeans(data[clusters == i ,] )}
    } else {
      for(i in 1:k){centroids[i, ] = means[i, ]}
    }
    
    distances = dist(data, centroids)
    old_clusts = clusters
    
    for(i in 1:length(clusters)){
      clusters[i] = which.min(distances[i,])
    }
    
    if(all(old_clusts == clusters)){
      break
    }
  }
  
  return(clusters)
}

gen_data = function(n){
  return(matrix(c(rnorm(n, mean=0, sd=1), rnorm(n, mean=0, sd=1)), nrow=n))
}

plot_clusters = function(k, cols=c('red', 'blue', 'green', 'purple')){
  plot(data[clusters==1, ], col= cols[1], xlim=c(-3.5,3.5), ylim=c(-3.5,3.5))
  for(i in 2:k){
    points(data[clusters==i, ], col= cols[i])
  }
}

find_firsts = function(){
  firsts = matrix(1:(10*ncol(as.matrix(train$x))), nrow = 10)
  for( i in 1:10){
    firsts[i, ] = as.matrix(train$x[train$y == i-1, ])[1, ]
  }
  
  return(firsts)
}