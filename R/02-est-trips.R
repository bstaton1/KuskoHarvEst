# ESTIMATE.TRIPS()

# AUTHOR: BEN STATON
# LAST UPDATED: 6/7/2017

# DESCRIPTION:
# performs the boat expansion described in Staton and Coggins (2016). 
# when more than one flight is flown, relies on the rules of probability to remove double counted boats
# marginal probability: p(count on fA)
# joint probability: p(count on fA and fB)
# conditional probability P(count on fA|fB)
  # p(fA|fB) = p(fA & fB)/p(fB)
# the fraction p(fA|fB) must have been counted twice and need to be removed from the second flight (carry-overs from first flight)

# uses a Lincoln-Petersen type estimator for the number of trips that occurred but weren't counted.

# INPUTS:
# trips: a data frame with columns trip.start and trip.end, formatted as decimal hours
# fX.times: a numeric vector with two elements: [1] is start time and [2] is end time of the Xth flight of the day. 
  # can currently handle a maximum of three flights per day. must be formatted as decimal hours
# num.fX: the number of boats counted on the water during the Xth flight.

# OUTPUTS
# a data frame with output from the expansion method

estimate.trips = function(trips, f1.times, f2.times, f3.times, num.f1, num.f2, num.f3) {
  
  # 0.) Error handling
  if (missing(f1.times) & missing(f2.times) & missing(f3.times)) stop ("must supply at least f1.times")
  if ((!missing(f1.times) & missing(num.f1)) |
      (!missing(f2.times) & missing(num.f2)) |
      (!missing(f3.times) & missing(num.f3))) {
    stop ("if supplying flight times, must also supply the number counted on that flight")
  }
  if ((missing(f1.times) & !missing(num.f1)) |
      (missing(f2.times) & !missing(num.f2)) |
      (missing(f3.times) & !missing(num.f3))) {
    stop ("if supplying flight counts, must also supply the corresponding flight times")
  }
  
  # remove records that don't have either trip start or end times (will include ALL LE interviews if used, see LE.prep())
  trips = trips[!is.na(trips$trip.start) & !is.na(trips$trip.end),c("trip.start", "trip.end")]
  
  # 1.) Logical flags determining which flights were flown
  f1 = !missing(f1.times)
  f2 = !missing(f2.times)
  f3 = !missing(f3.times)
  
  # 2.) based on the flights flown, perform the appropriate boat expansion
  ## FULL THREE FLIGHT COUNT SCENARIO ##
  if (f1 & f2 & f3) {
    # 3.) determine if each trip was available to be counted by plane and count them
    trips$count1 = trips.available(trips$trip.start, trips$trip.end, f1.times)
    trips$count2 = trips.available(trips$trip.start, trips$trip.end, f2.times)
    trips$count3 = trips.available(trips$trip.start, trips$trip.end, f3.times)
    
    # 4.) determine if a counted trip was present on more than one flight
    trips$count1and2 = ifelse(trips$count1 == 1 & trips$count2 == 1, 1, 0)                        # indicator of interviewed trip counted on f1 & f2
    trips$count2and3 = ifelse(trips$count2 == 1 & trips$count3 == 1, 1, 0)                        # indicator of interviewed trip counted on f2 & f3
    trips$counted = ifelse(trips$count1 == 1 | trips$count2 == 1 | trips$count3 == 1, 1, 0)       # indicator of interviewed trip counted at on at least one flight
    trips$not.counted = ifelse(trips$count1 != 1 & trips$count2 != 1 & trips$count3 != 1, 1, 0)   # indicator of interviewed trip not counted on any flight
    
    # 5.) count number in each category
    n.count1and2 = sum(trips$count1and2)       # number of interviewed trips counted on f1 & f2
    n.count2and3 = sum(trips$count2and3)       # number of interviewed trips counted on f1 & f3
    n.count1 = sum(trips$count1)               # number of interviewed trips counted on f1
    n.count2 = sum(trips$count2)               # number of interviewed trips counted on f2
    n.count3 = sum(trips$count3)               # number of interviewed trips counted on f3
    n.not.counted = sum(trips$not.counted)     # number of interviewed trips not counted on any flight
    n.counted = sum(trips$counted)             # number of interviewed trips counted on any flight
    
    # 6.) boat expansion
    p.old1 = n.count1and2/n.count2  # what proportion of boats counted in f2 were also counted in f1
    p.old2 = n.count2and3/n.count3  # what proportion of boats counted in f3 were also counted in f2
    
    old.fishers1 = num.f2 * p.old1         # number of old fishers from flight 1 that were counted in flight 2
    new.fishers2 = num.f2 - old.fishers1   # number of new fishers in flight 2
    old.fishers2 = num.f3 * p.old2         # number of old fishers from flight 2 that were counted in flight 3
    new.fishers3 = num.f3 - old.fishers2   # number of new fishers in flight 3
    
    total.a = num.f1 + new.fishers2 + new.fishers3         # total trips without accounting for trips conducted during times that were not counted
    trips.not.counted = n.not.counted * (total.a/n.counted)  # total trips that would have been uncounted
    total.b = total.a + trips.not.counted
    
    out = c(total.a = round(total.a), 
      total.b = round(total.b), 
      mean = round(mean(c(total.a, total.b))), 
      n.not.counted = n.not.counted,
      n.counted = n.counted, p.old1 = p.old1, p.old2 = p.old2, 
      trips.not.counted = trips.not.counted)
  }
  
  # ## TWO FLIGHT SCENARIO: 1 & 2 ##
  if (f1 & f2 & !f3) {
    
    # 3.) determine if each trip was available to be counted by plane and count them
    trips$count1 = trips.available(trips$trip.start, trips$trip.end, f1.times)
    trips$count2 = trips.available(trips$trip.start, trips$trip.end, f2.times)
    
    # 4.) determine if a counted trip was present on more than one flight
    trips$count1and2 = ifelse(trips$count1 == 1 & trips$count2 == 1, 1, 0)  # indicator of interviewed trip counted on f1 & f2
    trips$counted = ifelse(trips$count1 == 1 | trips$count2 == 1, 1, 0)     # indicator of interviewed trip counted at on at least one flight
    trips$not.counted = ifelse(trips$count1 != 1 & trips$count2 != 1, 1, 0) # indicator of interviewed trip not counted on any flight
    
    # 5.) count number in each category
    n.count1and2 = sum(trips$count1and2)       # number of interviewed trips counted on f1 & f2
    n.count1 = sum(trips$count1)               # number of interviewed trips counted on f1
    n.count2 = sum(trips$count2)               # number of interviewed trips counted on f2
    n.not.counted = sum(trips$not.counted)     # number of interviewed trips not counted on any flight
    n.counted = sum(trips$counted)             # number of interviewed trips counted on any flight
    
    # 6.) boat expansion
    p.old1 = n.count1and2/n.count2  # what proportion of boats counted in f2 were also counted in f1
    
    old.fishers1 = num.f2 * p.old1         # number of old fishers from flight 1 that were counted in flight 2
    new.fishers2 = num.f2 - old.fishers1   # number of new fishers in flight 2
    
    total.a = num.f1 + new.fishers2                          # total trips without accounting for trips conducted during times that were not counted
    trips.not.counted = n.not.counted * (total.a/n.counted)  # total trips that would have been uncounted
    total.b = total.a + trips.not.counted
    
    out = c(total.a = round(total.a), 
            total.b = round(total.b), 
            mean = round(mean(c(total.a, total.b))), 
            n.not.counted = n.not.counted,
            n.counted = n.counted, p.old1 = p.old1, 
            trips.not.counted = trips.not.counted)
  }
  
  # ## TWO FLIGHT SCENARIO: 1 & 3 ##
  if (f1 & !f2 & f3) {
    stop ("You specified f1.times and f3.times.\nIf only two flights are desired, specify times for flight 1 and flight 2")
  }
  ## TWO FLIGHT SCENARIO: 2 & 3 ##
  if (!f1 & f2 & f3) {
    stop ("You specified f2.times and f3.times.\nIf only two flights are desired, specify times for flight 1 and flight 2")
  }
  # ## ONE FLIGHT SCENARIO: 1 ONLY ##
  if (f1 & !f2 & !f3) {
    # 3.) determine if each trip was available to be counted by plane and count them
    trips$count1 = trips.available(trips$trip.start, trips$trip.end, f1.times)
    
    # 4.) determine if a counted trip was present on more than one flight
    trips$counted = ifelse(trips$count1 == 1, 1, 0)     # indicator of interviewed trip counted at on the only flight
    trips$not.counted = ifelse(trips$count1 != 1, 1, 0) # indicator of interviewed trip not counted on any flight
    
    # 5.) count number in each category
    n.not.counted = sum(trips$not.counted)     # number of interviewed trips not counted on the flight
    n.counted = sum(trips$counted)             # number of interviewed trips counted on the flight
    
    # 6.) boat expansion
    total.a = num.f1                                         # total trips without accounting for trips conducted during times that were not counted
    trips.not.counted = n.not.counted * (total.a/n.counted)  # total trips that would have been uncounted
    total.b = total.a + trips.not.counted
    
    out = c(total.a = round(total.a), 
            total.b = round(total.b), 
            mean = round(mean(c(total.a, total.b))), 
            n.not.counted = n.not.counted,
            n.counted = n.counted, 
            trips.not.counted = trips.not.counted)
  }
  
  ## ONE FLIGHT SCENARIO: 2 ONLY ##
  if (!f1 & f2 & !f3) {
    stop ("You specified f2.times only.\nIf only one flight is desired, specify flight times for only flight 1")
  }
  ## ONE FLIGHT SCENARIO: 3 ONLY ##
  if (!f1 & !f2 & f3) {
    stop ("You specified f3.times only.\nIf only one flight is desired, specify flight times for only flight 1")
  }
  
  return(out)
}
