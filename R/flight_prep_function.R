# FLIGHT.PREP()

# AUTHOR: BEN STATON
# LAST UPDATED: 6/8/2017

# DESCRIPTION:
# prepares the raw flight data file into the format needed by the rest of the analysis.
# this function can only handle one day at a time!

# INPUTS:
# dat: the raw flight data file

# OUTPUTS:
# a list with elements:
  #$fX.times: the start and end times of the Xth flight
  #$drift.tot: the total number of drift boats counted on each flight
  #$drift.p: the average proportion of all drift boats counted in each stratum
  #$set.nets: the maximum number of set nets counted in each stratum

flight.prep = function(dat) {
  
  dat$date = format.dates(dat$date)
  dat$start = format.times(dat$start)
  dat$end = format.times(dat$end)
  
  n.flights = max(dat$flight)
  
  strata = c("A", "B", "C", "D1")
  
  if (n.flights == 1) {
    # extract flight times
    f1.times = as.numeric(dat[1,c("start", "end")])
    
    # process drift count data
    drift.counts = dat[dat$gear == "drift",strata]
    drift.tot = sum(drift.counts)
    drift.p = as.numeric(drift.counts/drift.tot)
    names(drift.p) = strata
    
    # process set count data
    set.counts = dat[dat$gear == "set",strata]
    set.tot = sum(set.counts)
    set.p = as.numeric(set.counts/set.tot)
    names(set.p) = strata
    set.nets = round(set.tot * set.p)
    
    # output
    out = list(
      f1.times = f1.times,
      drift.tot = drift.tot,
      drift.p = drift.p,
      set.nets = set.nets
    )
  }
  
  if (n.flights == 2) {
    # extract flight times
    f1.times = as.numeric(dat[dat$flight == 1 & dat$gear == "drift",c("start", "end")])
    f2.times = as.numeric(dat[dat$flight == 2 & dat$gear == "drift",c("start", "end")])
    
    # process drift count data
    drift.counts = dat[dat$gear == "drift",strata]
    drift.tot = as.numeric(rowSums(drift.counts))
    drift.p = colMeans(t(apply(dat[dat$gear == "drift",strata], 1, function(x) x/sum(x))))
    
    # process flight count data
    set.counts = apply(dat[dat$gear == "set",strata], 2, max)
    set.tot = sum(set.counts)
    set.p = as.numeric(set.counts/set.tot)
    names(set.p) = strata
    set.nets = round(set.tot * set.p)
    
    # output
    out = list(
      f1.times = f1.times,
      f2.times = f2.times,
      drift.tot = drift.tot,
      drift.p = drift.p,
      set.nets = set.nets
    )
  }
  
  if (n.flights == 3) {
    # extract flight times
    f1.times = as.numeric(dat[dat$flight == 1 & dat$gear == "drift",c("start", "end")])
    f2.times = as.numeric(dat[dat$flight == 2 & dat$gear == "drift",c("start", "end")])
    f3.times = as.numeric(dat[dat$flight == 3 & dat$gear == "drift",c("start", "end")])
    
    # process drift count data
    drift.counts = dat[dat$gear == "drift",strata]
    drift.tot = as.numeric(rowSums(drift.counts))
    drift.p = colMeans(t(apply(dat[dat$gear == "drift",strata], 1, function(x) x/sum(x))))
    
    # process set count data
    set.counts = apply(dat[dat$gear == "set",strata], 2, max)
    set.tot = sum(set.counts)
    set.p = as.numeric(set.counts/set.tot)
    names(set.p) = strata
    set.nets = round(set.tot * set.p)
    
    # output
    out = list(
      f1.times = f1.times,
      f2.times = f2.times,
      f3.times = f3.times,
      drift.tot = drift.tot,
      drift.p = drift.p,
      set.nets = set.nets
    )
  }
  
  return(out)
  
}
