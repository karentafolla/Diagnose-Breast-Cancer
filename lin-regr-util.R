#
# utility functions for linear regression
#

# return a list of disjoint "sub-data frames" of dat that have
# sizes in proportion to the values of parameter frac.  
# example: if frac is c(0.75, 0.25) then a list of two data
# frames is returned, the first with a randomly selected 3/4 of the rows
# of dat, and the second with the remaining rows
split_data = function(dat, frac=c(0.75, 0.25)) {
  # at least one set must be specified
  k = length(frac)
  stopifnot(k > 0)
  
  n = nrow(dat)
  frac = frac/(sum(frac))
  starts = c(1, round(cumsum(frac) * n)[-k])
  ends = c(starts[-1]-1,n)
  samp = sample(1:n)
  data_sets = list()
  for (i in 1:k) {
    data_sets[[i]] = dat[samp[starts[i]:ends[i]],]
  }
  return(data_sets)
} 

