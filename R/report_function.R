# REPORT()

# AUTHOR: BEN STATON
# DATE UPDATED: 6/10/2017

# DESCRIPTION
# takes a vector output from rand.summ() and puts in a character string for reporting purposes

# INPUTS
# x: a vector with three elements: ["mean"], ["2.5%"], and ["97.5%"]

# OUTPUTS
# a character string of the form: "mean (lwr - upr)"

# NOTES
# this function performs na.rm = T automatically

report = function(x) {
  
  paste(x["mean"], " (", x["2.5%"], " - ", x["97.5%"], ")", sep = "")
  
  
}

