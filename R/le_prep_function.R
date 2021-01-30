# LE.PREP()

# AUTHOR: BEN STATON
# LAST UPDATED: 6/9/2017

# DESCRIPTION:
# prepares data from the law enforcement interviews into the format necessary for the rest of the analysis

# INPUTS:
# dat: the raw data set 

# OUTPUTS:
# a data frame in the consistent format for all analyses

# NOTES:
  # because these are not completed trip interviews, certain information must not be included in analysis
  # in particular, can only use catch rates (cpe), not soak time, not start or stop times

# dat = le.raw.dat


LE.prep = function(dat) {
  # add a source variable
  dat$source = "LE"
  
  # format dates and times
  dat$date = format.dates(dat$date)

  # calculate catch rates
  dat$chinook.cpe = dat$chinook/(dat$soak.hrs * dat$length)
  dat$chum.cpe = dat$chum/(dat$soak.hrs * dat$length)
  dat$sockeye.cpe = dat$sockeye/(dat$soak.hrs * dat$length)
  
  dat = dat[!is.na(dat$chinook.cpe) & !is.na(dat$chum.cpe) & !is.na(dat$sockeye.cpe),]
  
  # make it so soak hours can't contribute to harvest estimates
  dat$soak.hrs = NA
  
  # make it so trip times can't contribute to effort estimates
  dat$trip.start = NA
  dat$trip.end = NA
  dat$mesh = NA
  
  return(dat[,cols])
  
}
