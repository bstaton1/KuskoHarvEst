# CBM.PREP()

# AUTHOR: BEN STATON
# LAST UPDATED: 6/7/2017

# DESCRIPTION:
# formats the data supplied by community based monitoring project into the standard format for all analyses

# INPUTS:
# dat: the raw data frame supplied by Bill Bechtol
# includeWhiteFish: logical. should whitefish (sheefish + others) cpe and catch be included?
# includeType: logical. Should interview type (Boat Launch, Village, Fish camp) be included?
# includeVillage: logical. Should the village the interview originated from be included?

# OUTPUTS:
# a data frame with all of the necessary variables

cbm.prep = function(dat, includeWhiteFish = F, includeType = F, includeVillage = F) {
  
  # add a source variable
  dat$source = "CBM"
  
  # format the date variable
  dat$date = format.dates(dat$date)
  
  # format the time variables
  dat$trip.start = format.times(times = dat$trip.start)
  dat$trip.end = format.times(times = dat$trip.end)
  if (class(dat$soak.hrs) == "character") dat$soak.hrs = format.times(times = dat$soak.hrs)
  
  # standardize the gear variable
  dat$gear = ifelse(is.na(dat$gear), NA, 
                    ifelse(dat$gear == "Driftnet", "drift", 
                           ifelse(dat$gear == "Setnet", "set", NA)))
  
  # extract the harvest goal variables
  goals = dat[,c("village", "chinook.goal", "chum.goal", "sockeye.goal")]
  
  # extract the catch and effort information
  # dat = dat[,-which(colnames(dat) %in% c("sheefish", "whitefish", "chinook.goal", "chum.goal", "sockeye.goal"))]
  
  dat$whitefish = rowSums(dat[,c("sheefish", "whitefish")])
  
  # calculate catch per effort
  dat$chinook.cpe = dat$chinook/(dat$length * dat$soak.hrs)
  dat$chum.cpe = dat$chum/(dat$length * dat$soak.hrs)
  dat$sockeye.cpe = dat$sockeye/(dat$length * dat$soak.hrs)
  dat$whitefish.cpe = dat$whitefish/(dat$length * dat$soak.hrs)
  
  add = NULL
  
  if (includeWhiteFish) {
    add = c("whitefish", "whitefish.cpe")
  }
  if (includeVillage) {
    add = c(add, "village")
  }
  
  if (includeType) {
    add = c(add, "type")
  }
  
  # return the output
  return(list(
    catch.effort = dat[,c(cols, add)],
    goals = goals
  ))
  
}
