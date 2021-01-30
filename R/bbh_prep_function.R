# BBH.PREP()

# AUTHOR: BEN STATON
# LAST UPDATED: 6/11/2017

# DESCRIPTION:
# prepares data from the Bethel Boat Harbor interviews into the format necessary for the rest of the analysis

# INPUTS:
# dat: the raw data set form given by the devoted Access Database
# includeWhiteFish: logical. should whitefish (sheefish + other whitefishes) be included?

# OUTPUTS:
# a data frame in the consistent format for all analyses

bbh.prep = function(dat, includeWhiteFish = F) {
  # add a source variable
  dat$source = "BBH"
  
  # format the date variable
  dat$date = format.dates(dat$date)
  
  # format the time variables
  dat$trip.start = format.times(times = dat$trip.start)
  dat$trip.end = format.times(times = dat$trip.end)
  dat$soak.hrs = dat$soak.min/60
  
  # standardize the gear variable
  dat$gear = ifelse(is.na(dat$gear), NA, 
                    ifelse(dat$gear == "Drift", "drift", 
                           ifelse(dat$gear == "Set", "set", NA)))
  
  # extract the catch and effort information
  # dat = dat[,-which(colnames(dat) %in% c("sheefish", "whitefish"))]
  
  # calculate catch per effort
  dat$chinook.cpe = dat$chinook/(dat$length * dat$soak.hrs)
  dat$chum.cpe = dat$chum/(dat$length * dat$soak.hrs)
  dat$sockeye.cpe = dat$sockeye/(dat$length * dat$soak.hrs)
  dat$whitefish = rowSums(dat[,c("whitefish", "sheefish")], na.rm = T)
  dat$whitefish.cpe = dat$whitefish/(dat$length * dat$soak.hrs)
  
  if (includeWhiteFish) {
    out = dat[,c(cols, "whitefish", "whitefish.cpe")]
  } else {
    out = dat[,cols]
  }
  
  # return the output
  return(out)
  
}


