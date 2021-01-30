# ADD.HIST.LEGEND()

# AUTHOR: BEN STATON
# DATE UPDATED: 6/8/2017

# DESCRIPTION
# adds a legend with means to the top of the multipanel histograms

# INPUTS
# all.mean: the mean of all samples. if only one data set used, supply the mean here
# bbh.mean: the mean of samples from the bethel boat harbor
# cbm.mean: the mean of samples from the community based monitoring
# fc.mean: the mean from bethel area fish camps

# OUTPUTS
# none. generates the legend

# NOTES
  # any combinations of bbh, cbm, and fc are accepted, but all.mean must also be supplied
  # if only one data set used, supply only the all.mean


add.hist.legend = function(all.mean, bbh.mean, cbm.mean, fc.mean, loc = "topright") {
  
  bbh = !missing(bbh.mean)
  cbm = !missing(cbm.mean)
  fc = !missing(fc.mean)
  
  cex = 1
  text.font = 1
  
  # only one data set: all mean
  if (!bbh & !cbm & !fc) {
    legend(loc, legend = paste("Mean =", all.mean), cex = cex, bty = "n",text.font = text.font)
  }
  
  # two data sets: bbh and cbm
  if (bbh & cbm & !fc) {
    legend(loc, legend = c(paste("All:", all.mean),
                                  paste("BBH:", bbh.mean),
                                  paste("CBM:", cbm.mean)), cex = cex, bty = "n",text.font = text.font)
  }
  
  # two data sets: bbh and fc
  if (bbh & !cbm & fc) {
    legend(loc, legend = c(paste("All:", all.mean),
                                  paste("BBH:", bbh.mean),
                                  paste("FC:", fc.mean)), cex = cex, bty = "n",text.font = text.font)
  }
  
  # two data sets: cbm and fc
  if (!bbh & cbm & fc) {
    legend(loc, legend = c(paste("All:", all.mean),
                                  paste("CBM:", cbm.mean),
                                  paste("FC:", fc.mean)), cex = cex, bty = "n",text.font = text.font)
  }
  
  # three data sets: cbm, bbh, and fc
  if (bbh & cbm & fc) {
    legend(loc, legend = c(paste("All:", all.mean),
                                  paste("BBH:", bbh.mean),
                                  paste("CBM:", cbm.mean),
                                  paste("FC:", fc.mean)), cex = cex, bty = "n",text.font = text.font)
  }
  
}
