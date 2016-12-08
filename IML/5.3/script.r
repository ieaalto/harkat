generate_data = function(n){
  data = data.frame(Y = numeric(200), X1 = numeric(200) , X2 = numeric(200))
  data$Y = sample(c(1, -1), n, replace=TRUE)
  
  for (i in 1:n){
    delta = if(data$Y[i] == 1) 4 else 1 
    data$X1[i] = rnorm(1, mean=0, sd=delta)
    data$X2[i] = rnorm(1, mean=0, sd=delta) 
  }
  
  return(data)
}

train = generate_data(200)

fit.linear = svm(factor(Y)~., data=train, kernel="linear", cost=1, scale=F)
fit.radial = svm(factor(Y)~., data=train, kernel="radial", cost=1, scale=F)
fit.polynomial = svm(factor(Y)~., data=train, kernel="polynomial", degree=2, cost=1, scale=F)

fit.linear.error = sum(predict(fit.linear, train) != train$Y )/200
fit.radial.error = sum(predict(fit.radial, train) != train$Y )/200
fit.polynomial.error = sum(predict(fit.polynomial, train) != train$Y )/200



train$X3 = train$X1^2
train$X4 = train$X2^2

fit.linear2 = svm(factor(Y)~., data=train, kernel="linear", cost=1, scale=F)
fit.radial2 = svm(factor(Y)~., data=train, kernel="radial", cost=1, scale=F)
fit.polynomial2 = svm(factor(Y)~., data=train, kernel="polynomial", degree=2, cost=1, scale=F)

fit.linear2.error = sum(predict(fit.linear2, train) != train$Y )/200
fit.radial2.error = sum(predict(fit.radial2, train) != train$Y )/200
fit.polynomial2.error = sum(predict(fit.polynomial2, train) != train$Y )/200