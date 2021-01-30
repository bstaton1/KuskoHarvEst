

goal.table = function(x, spp) {
  
  # filter out the appropriate species
  tab.dat = x[,c("village", paste(tolower(spp), "goal", sep = "."))]
  
  # count how many are in each category by each village
  out = table(tab.dat$village, tab.dat[,paste(tolower(spp), "goal", sep = ".")])
  
  # create an empty matrix with all categories, even one's that weren't observed
  # this will ensure there are always 5 categories in table, even if only 3 were observed
  empty = matrix(0, nrow = nrow(out), ncol = 5); rownames(empty) = rownames(out); colnames(empty) = 1:5
  empty[,colnames(out)] = out; out = empty; rm(empty)
  
  # turn to percentages
  out = round(t(apply(out, 1, function(x) x/sum(x))), 2) * 100
  out = t(apply(out, 1, function(x) paste(x, "%", sep = "")))
  
  # calculate aggregate percentages
  all = paste(round((table(tab.dat$chinook.goal)/nrow(tab.dat)), 2) * 100, "%", sep = "")
  out = rbind(out, All = all)
  
  # column names
  cnames = c("Not at All", "Under Half", "Half", "Over Half", "Done")
  colnames(out) = cnames
  
  # sample sizes
  N = table(tab.dat$village)
  N = c(N, All = sum(N)); names(N) = NULL
  
  # row names
  rnames = rownames(out)
  rownames(out) = NULL
  
  # add sample size and village names
  out = cbind(N = N, out)
  out = cbind(Village = rnames, out)
  
  # return pandoc table
  pander::pandoc.table(out, emphasize.strong.cols = 1, emphasize.strong.rows = nrow(out), justify = c("lcccccc"))
}

obtain.percent = function(x) {
  empty = rep(0, 5); names(empty) = 1:5
  counts = table(x)
  empty[names(counts)] = counts
  counts = empty
  percent = rev(cumsum(rev(counts))/sum(counts)) * 100
  paste(round(percent[2:5]), "%", sep = "")
}

goal.table_above = function(x, spp) {
  percents = tapply(x[,paste(tolower(spp), ".goal", sep = "")], x[,"village"], obtain.percent)
  
  villages = names(percents)
  percents = matrix(unlist(percents), length(villages), 4, byrow = T)
  
  N = unname(table(x$village))
  N = c(N, nrow(x))
  
  all = obtain.percent(x = x[,paste(tolower(spp), ".goal", sep = "")])
  
  villages = c(villages, "All")
  
  percents = rbind(percents, all)
  
  tab = cbind(villages, N, percents)
  
  rownames(tab) = colnames(tab) = NULL
  colnames(tab) = c("Village", "N", "Under Half", "Half", "Over Half", "Done")
  
  pandoc.table(tab, style = "simple", justify = c("left", rep("center", ncol(tab) - 1)),
               emphasize.strong.rows = nrow(tab), emphasize.strong.cols = 1)
  
  
}

# goal.table_above(x = x, spp = "chinook")




# 
# goal.table = function(goals, spp) {
#   
#   status.names = c("None", "Under Half", "Half", "Over Half", "Done")
#   at.x = seq(1,5)
#   
#   ordered.villages = c("Tuntutuliak", "Nunapitchuk", "Napakiak", "Napaskiak", "Kwethluk", "Akiak")
#   
#   strata = sort(unique(goals$village))
#   n.strata = length(strata)
#   
#   percents = tapply(goals[,spp], goals[,"village"], obtain.percent)
#   tab = matrix(paste(round(unlist(percents)), "%", sep = ""), length(percents), 5, byrow = T)
#   rownames(tab) = NULL
#   colnames(tab) = status.names
#   tab = cbind(Village = names(percents), tab)
#   all.percents = paste(round(obtain.percent(goals[,spp])), "%", sep = "")
#   tab = rbind(tab, c("All", all.percents))
#   
#   tab = as.data.frame(tab)
#   
#   rownames(tab) = tab$Village
#   
#   tab = tab[c(ordered.villages[ordered.villages %in% strata], "All"),]
#   rownames(tab) = NULL
#   
#   tab = tab[c("Village", "Under Half", "Half", "Over Half", "Done")]
#   
#   pandoc.table(tab, style = "simple", justify = c("left", rep("center", ncol(tab) - 1)), emphasize.strong.cols = 1, emphasize.strong.rows = nrow(tab))
#   
# }
# 
# 
