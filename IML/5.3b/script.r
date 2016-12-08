inds = sample(1:length(OJ$Purchase), 800)

train = OJ[inds, ]
test = OJ[-inds, ]

fit.linear = svm(factor(Purchase)~., data = train, kernel ='linear', cost = 0.01)
fit.radial = svm(factor(Purchase)~., data = train, kernel ='radial', cost = 0.01)
fit.polynomial = svm(factor(Purchase)~., data = train, kernel ='polynomial', degree = 2, cost = 0.01)

test_error.linear = sum(predict(fit.linear, test) != test$Purchase) / 270
test_error.radial = sum(predict(fit.radial, test) != test$Purchase) / 270
test_error.polynomial = sum(predict(fit.polynomial, test) != test$Purchase) / 270


fit.linear.tuned = tune(svm, factor(Purchase)~., data = train, kernel ='linear', ranges = 
                   list(cost=c(0.01, 0.1, 0.25, 0.5, 1.0, 2, 3, 5, 10)))

fit.radial.tuned = tune(svm, factor(Purchase)~., data = train, kernel ='radial', ranges = 
                          list(cost=c(0.01, 0.1, 0.25, 0.5, 1.0, 2, 3, 5, 10)))

fit.polynomial.tuned = tune(svm, factor(Purchase)~., data = train, kernel ='polynomial', degree=2, ranges = 
                          list(cost=c(0.01, 0.1, 0.25, 0.5, 1.0, 2, 3, 5, 10)))

test_error.linear.tuned = sum(predict(fit.linear.tuned$best.model, test) != test$Purchase) / 270
test_error.radial.tuned = sum(predict(fit.radial.tuned$best.model, test) != test$Purchase) / 270
test_error.polynomial.tuned = sum(predict(fit.polynomial.tuned$best.model, test) != test$Purchase) / 270