
ary_to_df = function(x, date, gear, n = NULL) {
  
  # dimensions and dimnames
  dims = dim(x)
  dimnmes = dimnames(x)
  
  # decide how many random observations to keep
  n = ifelse(is.null(n), dims[1], 
             ifelse(dims[1] < n, dims[1], n))
  i = as.character(1:n)
  
  # loop through species (s) and strata (j), extract the random samples for that combo, then store in a tmp df
  # paste on bottom of bigger df
  out = NULL
  for (s in dimnmes[[2]]) {
    for (j in dimnmes[[3]]) {
      tmp = x[i,s,j]
      tmp = data.frame(date = date, spp = s, stratum = j, gear = gear, iter = 1:n, catch = tmp)
      out = rbind(out, tmp)
    }
  }

  out
}

