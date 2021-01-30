# FORMAT.TIMES()

# AUTHOR: BEN STATON
# LAST UPDATED: 6/7/2017

# DESCRIPTION:
# turns a 24 hour clock time to a decimal hour

# INPUTS:
# times: a character vector of times on the 24 hour clock

# OUTPUTS:
# a decimal hour version of the input times. Decimal hour is 11.5 for 11:30, 18.25 for 6:15PM (18:15PM)

format.times = function(times) {
  
  # split the times at the colon into hours and minutes
  split.times = strsplit(x = times, split = ":")
  
  # for each time, add mins/60 to hours
  unlist(lapply(split.times, function(x) {as.numeric(x)[1] + round((as.numeric(x)[2]/60),2)}))
}
