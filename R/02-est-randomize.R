# RANDOMIZE.DATA()

# AUTHOR: BEN STATON
# LAST UPDATED: 6/7/2017

# DESCRIPTION:
# randomizes an input dataset by sampling with replacement the number of observations as available in the input dataset

# INPUTS:
# dat: the raw data set that contains interview information from all available data sources (BBH, CBM, LE, FC)
# X.weight: the weight of data source X (defaults to 1 for all data sources). 
  # if BBH.weight = 2 and all others = 1, BBH.weight will be twice as likely to be sampled (i.e., carries twice as much weight)

# OUTPUTS:
# a randomized data frame of interviews

# NOTE:
  # if some data source weights are zero, these rows must be removed from the dataset to be sampled
  # otherwise, you will sample the number of observations equal to all observations, even those with zero weight
  # this results in oversampling and too much confidence
  # zero weight should equate to totally ignoring the data set with zero weight..without removing zero weighted obs, this is not the case

randomize.data = function(dat, BBH.weight = 1, FC.weight = 1, LE.weight = 1, CBM.weight = 1, ADFG.weight = 1) {

  # assign weights to observations
  dat$weight = NA
  dat$weight[dat$source == "BBH"] = BBH.weight
  dat$weight[dat$source == "FC"] = FC.weight
  dat$weight[dat$source == "LE"] = LE.weight
  dat$weight[dat$source == "CBM"] = CBM.weight
  dat$weight[dat$source == "ADFG"] = ADFG.weight
  
  # sample from only interviews with no-zero weights
  samp.dat = dat[dat$weight != 0,]
  n = nrow(samp.dat)
  
  # select random rows
  rand.rows = sample(x = 1:n, size = n, prob = samp.dat$weight, replace = T)
  
  # return the randomized dataset
  samp.dat[rand.rows,]
}