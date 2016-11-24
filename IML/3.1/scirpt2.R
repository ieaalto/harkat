sigmat2 = matrix(c(1,0,0,
                   0,2,0,
                   0,0,3), nrow = 3)

f = function(a){
  return( -2* log(a * ((2 *pi)^(3/2) * sqrt(det(sigmat2))), base=exp(1)))  
}

b = f(1/100)