# TRIPS.AVAILABLE()

# AUTHOR: BEN STATON
# LAST UPDATED: 6/7/2017

# DESCRIPTION:
# determines if a set of interviewed fisher trips were available to be counted during a flight

# INPUTS:
# trip.start: a numeric vector with the start times of the interviewed trips. must be in decimal hours, eg. 13.5 is 1:30PM
# trip.end: a numeric vector with the end times of the interviewed trips. must be in decimal hours, e.g., 8.75 is 8:45AM
# f.times: a numeric vector of length two with [1] as the start time of the flight and [2] as the end time of the flight. must be formatted same as trip.start/trip.end

# OUTPUT: 
# a binary vector indicating if each trip was available to be counted (1) or not available (0)
trips.available = function(trip.start, trip.end, f.times) {
  
  # create more intitive names
  flight.start = f.times[1]
  flight.end = f.times[2]
  
  # scenario 1: trip started before flight and ended within flight
  scen1 = ifelse(trip.start < flight.start & (trip.end > flight.start & trip.end <= flight.end), 1, 0)
  
  # scenario 2: trip started within flight and trip ended after flight
  scen2 = ifelse((trip.start > flight.start & trip.start < flight.end) & trip.end > flight.end, 1, 0)
  
  # scenario 3: trip started and ended within flight
  scen3 = ifelse((trip.start > flight.start & trip.start < flight.end) & (trip.end > flight.start & trip.end < flight.end), 1, 0)
  
  # scenario 4: trip started before flight started and ended after flight ended
  scen4 = ifelse(trip.start <= flight.start & trip.end > flight.end, 1, 0)
  
  # scenario 5: trip started
  scen5 = ifelse((trip.start > flight.start & trip.start < flight.end) | (trip.end > flight.start & trip.end < flight.end), 1, 0)
  
  # scenario 6: trip started exactly on flight start and ended exactly on trip end
  scen6 = ifelse(trip.start == flight.start & trip.end == flight.end, 1, 0)
  
  # combine scenarios
  scenarios = cbind(scen1, scen2, scen3, scen4, scen5, scen6)
  
  # determine if at least one scenario occurred, if so return a 1, if not, return 0
  counted = ifelse(rowSums(scenarios) > 0, 1, 0)
  
  return(counted)
}

# trips.available = function(trip.start, trip.end, f.times) {
# 
#   # create more intitive names
#   flight.start = f.times[1]
#   flight.end = f.times[2]
# 
#   # scenario 1: trip started before flight and ended within flight
#   scen1 = ifelse(trip.start <= flight.start & (trip.end >= flight.start & trip.end <= flight.end), 1, 0)
# 
#   # scenario 2: trip started within flight and trip ended after flight
#   scen2 = ifelse((trip.start >= flight.start & trip.start <= flight.end) & trip.end >= flight.end, 1, 0)
# 
#   # scenario 3: trip started and ended within flight
#   scen3 = ifelse((trip.start >= flight.start & trip.start <= flight.end) & (trip.end >= flight.start & trip.end <= flight.end), 1, 0)
# 
#   # scenario 4: trip started before flight started and ended after flight ended
#   scen4 = ifelse(trip.start <= flight.start & trip.end >= flight.end, 1, 0)
# 
#   # scenario 5: trip started
#   scen5 = ifelse((trip.start >= flight.start & trip.start <= flight.end) | (trip.end >= flight.start & trip.end <= flight.end), 1, 0)
# 
#   # combine scenarios
#   scenarios = cbind(scen1, scen2, scen3, scen4, scen5)
# 
#   # determine if at least one scenario occurred, if so return a 1, if not, return 0
#   counted = ifelse(rowSums(scenarios) > 0, 1, 0)
# 
#   return(counted)
# }

