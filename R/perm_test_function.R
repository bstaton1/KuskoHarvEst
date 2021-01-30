# PERM.TEST()

# AUTHOR: BEN STATON
# DATE UPDATED: 6/10/2017

# DESCRIPTION
# performs a permutation test for difference in means between two sets of random variables

# INPUTS
# y1: a numeric vector of measurements from one group
# y2: a numeric vector of measurements from another group
# do.plot: do you wish to see a histogram of the Null distribution? 

# OUTPUTS
# a list with the two-sided p-value and the observed difference in means

# this test is a non-parametric test - no underlying sampling distribution of 
# the test statistic is assumed. Instead, you generate the null distribution of
# the test statistic by randomizing the association between the observation and the
# group (i.e., shuffling observations across groups) many times and calculating the 
# test statistic. the p value is the proportion of times the observed difference
# was more extreme than the randomized differences.

perm.test = function(y1, y2, do.plot = T) {
  
  # observed difference
  Dobs = mean(y2, na.rm = T) - mean(y1, na.rm = T)
  
  # obtain null distribution of test statistic
  Dstar = replicate(n = 10000, {
    
    y.combined = c(y1, y2)
    
    x = c(rep(1, length(y1)), rep(2, length(y2)))
    xstar = sample(x, replace = F)
    
    mean(y.combined[xstar == 2], na.rm = T) - mean(y.combined[xstar == 1], na.rm = T)
  })
  
  
  if (do.plot) {
    # windows()
    hist(Dstar, xaxs = "i", yaxs = "i", breaks = 50, col = "grey90", main = "Null Distribution", xlim = range(c(Dobs, Dstar)))
    abline(v = Dobs, lwd = 2, lty = 2, col = "blue")
  }
  
  Dobs = Dobs
  p = mean(abs(Dstar) > abs(Dobs))
  
  list(diff = Dobs, p = p)
}


# y1 = rnorm(n = 30, 0, 1)
# y2 = rnorm(n = 30, 1, 1)
# 
# t.test(x = y1, y = y2)
# perm.test(y1, y2, do.plot = F)
