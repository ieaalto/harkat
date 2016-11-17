f = function(x){
  y = 2 + x - 0.5 * x^2
  return(y)
}

mse = function(Y, E){
  return(sum((Y - E)^2) / length(Y))
}

generate_data = function(n){
  X = runif(n, -3, 3)
  Y = f(X) + rnorm(n, mean=0, sd=0.4)
  M = matrix(c(X, Y), ncol=2)
  colnames(M) = c('x', 'y')
  return( data.frame(M) )
}

plotfit = function(data, fit){
  M = matrix(c(data[,1], fitted(fit)), ncol=2)
  colnames(M) = c('x', 'y')
  M = data.frame(M)
  
  plot(data)
  lines(M[with(M, order(x)), ])
  
}

mses = function(data, fits){
  errors = c()
  for(i in 1:length(fits)){
    errors = c(errors, mse(data[,2], fitted(fits[[i]])))
  }
  T = data.frame(c(1:length(fits)))
  T[2] = errors
  colnames(T) = c('p', 'MSE')
  return(T)
}

makefits = function(n, data){
  fits = list()
  for(i in 1:n){
    fits[[i]] = lm(y ~ poly(x, i), D)
  }
  return(fits)
}




