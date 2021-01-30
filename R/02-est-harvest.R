# ESTIMATE.HARVEST()

# AUTHOR: BEN STATON
# LAST UPDATED: 6/7/2017

# DESCRIPTION:
# generates harvest estimates for each chinook, chum, and sockeye based on a set of interviews and a total effort estimate

# INPUTS:
# dat: the interview dataset
# n.trips: the number of trips

# OUTPUTS:
# a numeric vector of length 3: estimated harvest for chinook, chum, and sockeye

estimate.harvest = function(dat, n.trips, includeWhiteFish = F, return.quants = F, use.medians = F) {
  
  if (use.medians) {
    avg.soak.hrs = median(dat$soak.hrs, na.rm = T)
    avg.length = median(dat$length, na.rm = T)
    avg.chinook.cpe = median(dat$chinook.cpe, na.rm = T)
    avg.chum.cpe = median(dat$chum.cpe, na.rm = T)
    avg.sockeye.cpe = median(dat$sockeye.cpe, na.rm = T)
    if (includeWhiteFish) avg.whitefish.cpe = median(dat$whitefish.cpe, na.rm = T)
  } else {
    # calculate mean soak time
    avg.soak.hrs = mean(dat$soak.hrs, na.rm = T)
    
    # calculate mean net length
    avg.length = mean(dat$length, na.rm = T)
    
    # calculate mean cpe by species
    avg.chinook.cpe = mean(dat$chinook.cpe, na.rm = T)
    avg.chum.cpe = mean(dat$chum.cpe, na.rm = T)
    avg.sockeye.cpe = mean(dat$sockeye.cpe, na.rm = T)
    if (includeWhiteFish) avg.whitefish.cpe = mean(dat$whitefish.cpe, na.rm = T)
    
  }
  
  # produce harvest estimates by species
  chinook.harv = avg.chinook.cpe * avg.soak.hrs * avg.length * n.trips
  chum.harv = avg.chum.cpe * avg.soak.hrs * avg.length * n.trips
  sockeye.harv = avg.sockeye.cpe * avg.soak.hrs * avg.length * n.trips
  if (includeWhiteFish) whitefish.harv = avg.whitefish.cpe * avg.soak.hrs * avg.length * n.trips
  
  # return a vector with harvest estimates
  if (includeWhiteFish) {
    out = c(chinook = chinook.harv, chum = chum.harv, sockeye = sockeye.harv, whitefish = whitefish.harv)
  } else {
    out = c(chinook = chinook.harv, chum = chum.harv, sockeye = sockeye.harv)
  }
  
  if (return.quants) {
    out = c(avg.soak.hrs = avg.soak.hrs, avg.length = avg.length, avg.chinook.cpe = avg.chinook.cpe, avg.chum.cpe = avg.chum.cpe, avg.sockeye.cpe = avg.sockeye.cpe)
  }
  
  return(out)
}