
sumtable = function(diffs){
  sums = c(diffs[1])
  for(i in 2 : length(diffs)){
    sums = c(sums, sums[i-1] + diffs[i])  
  }
  
  return(sums)
}

normalize = function(v){
  mn = min(v)
  mx = max(v)
  
  return((v - mn) *  1 / (mx - mn))
}