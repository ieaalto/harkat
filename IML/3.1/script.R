library('MASS')

x1.var = 2.0
x2.var = 3.0

corr = -0.75
covar = corr * sqrt(x1.var * x2.var)

sigmat = matrix( c(x1.var, covar, covar, x2.var), nrow = 2) 

data = mvrnorm(n=200, c(0,0), sigmat)
colnames(data) = c('x', 'y')

data.cov = cov(data)
data.cor = cor(data)

plot(data)
emp_densities = kde2d(x = data[,1], y = data[,2])
contour(emp_densities)
image(emp_densities)
persp(emp_densities)

density = function(x, mu, Sigma){
  return( (1 / (2*pi * sqrt(det(Sigma)))) * exp( (-(1/2) * t(x - mu)) %*% ginv(Sigma) %*% (x - mu)))
}

grid = expand.grid(.25*(-20:20), .25*(-20:20)) 

compute_densities = function(mu, Sigma=sigmat){
  densities = c()
  for(i in 1:length(grid[,1])){
    x =  c(grid[i, 1], grid[i,2])
    densities = c(densities, density(x, mu, Sigma))
  }
  return(densities)
}

dens1 = compute_densities(c(0,0))
dens2 = compute_densities(c(2,1))

compute_probs = function(){
  probs = c()
  for(i in 1:length(grid[,1])){
    probs = c(probs, (dens1[i]*(1/2))/ ((dens1[i]*(1/2) + dens2[i]*(1/2) )))
  }
  return(matrix(probs, nrow=41))
}

probs = compute_probs()
dens1.mat = matrix(dens1, nrow=41)
dens2.mat = matrix(dens2, nrow=41)



